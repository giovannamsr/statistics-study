library(tidyverse)

# psi(x) - model base -------------------------------------------------

ggplot(data = data.frame(x = 0), mapping = aes(x = x)) + 
  stat_function(fun = function(x) 45*tanh(x/1.9 - 7) + 57, linewidth = 1.2) +
  labs(x = "years of education", y = "annual income") + 
  xlim(8, 18) + 
  theme_bw()


# generated sample ----------------------------------------------------------
runif(5, 8, 18)

n_obs <- 100

set.seed(123)

data <- tibble(x = sort(runif(n = n_obs, min = 8, max = 18)), 
                y = 45*tanh(x/1.9 - 7) + 57 + rnorm(n = n_obs, mean = 0, sd = 4))

data %>% 
  ggplot(aes(x, y)) + 
  geom_point() + 
  theme_bw()

# spline - training with all data ------------------------------------------------------------------

#tibble to compare the results
results <- tibble(df = 2:100, mse = NA_real_)

#prediction without separating into training and test
for (i in 1:nrow(results)) {
  #ajust spline with i degrees of freedom
  fit <- smooth.spline(data$x, data$y, df = results$df[i], all.knots = TRUE)
  
  #calculates the MSE and hold it at results$mse[i]
  results$mse[i] <- mean((data$y - predict(fit, data$x)$y)^2)
}

results %>%
  ggplot(aes(x = df, y = mse)) +
  geom_point() +
  geom_line()

#the mse only decreases with increasing degrees of freedom (df). When the df 
#is equal to the sample size, they are interpolated and mse = 0

# training and validation -------------------------------------------------

#n_obs <- 100

#set.seed(123)

data <- tibble(x = sort(runif(n = n_obs, min = 8, max = 18)), 
                y = 45*tanh(x/1.9 - 7) + 57 + rnorm(n = n_obs, mean = 0, sd = 4))

validation <- tibble(x = sort(runif(n = n_obs, min = 8, max = 18)),
                    y = 45*tanh(x/1.9 - 7) + 57 + rnorm(n = n_obs, mean = 0, sd = 4))

results2 <- tibble(df = 2:100, mse = NA_real_)

for (i in 1:nrow(results)) {
  #ajust spline with i degrees of freedom
  fit <- smooth.spline(data$x, data$y, df = results2$df[i], all.knots = TRUE)
  
  #calculates the MSE and hold it at results$mse[i]
  results2$mse[i] <- mean((validation$y - predict(fit, validation$x)$y)^2)
}

results2 %>%
  ggplot(aes(x = df, y = mse)) +
  geom_point() +
  geom_line()

# combine results
results_combine <- bind_rows(
  mutate(results, dataset = "train"),
  mutate(results2, dataset = "validation")
)

ggplot(results_combine, aes(x = df, y = mse, color = dataset)) +
  geom_point() +
  geom_line() +
  labs(title = "Spline MSE train x validation", x = "Degrees of Freedom", y = "MSE") +
  theme_minimal()

# With the separation between training and testing data, we can observe the difference between
#the testing and training MSE. With training data, the trade-off between bias and variance is observed.
#Increasing degrees of freedom excessively increases variance, causing overfitting.


# Cross-Validation --------------------------------------------------------

data <- tibble(x = sort(runif(n = n_obs, min = 8, max = 18)),
                y = 45*tanh(x/1.9 - 7) + 57 + rnorm(n = n_obs, mean = 0, sd = 4))

n_lots <- 10
n_obs_lot <- n_obs/n_lots

mse_aux <- vector("numeric", n_lots) # mse_aux <- rep(0,10)

lots <- sample(rep(1:n_lots, each = n_obs_lot))

results3 <- tibble(df = 2:(n_obs - n_obs_lot),
                   mse = NA_real_)

for (i in 1:nrow(results3)) {
  
  for (j in 1:n_lots) {
    
    # calculates the MSE of lot j and stores it in a vector
    fit <- smooth.spline(data$x[lots != j],
                         data$y[lots != j],
                         df = results3$df[i],
                         all.knots = TRUE)
    
    mse_aux[j] <- mean((data$y[lots == j] - predict(fit, data$x[lots == j])$y)^2)
  }
  
  results3$mse[i] <- mean(mse_aux)
  #store the average of the vector with the MSE in results3$mse[i]
}

results3 %>%
  arrange(mse)

results3 %>%
  ggplot(aes(df, mse)) +
  geom_point() +
  geom_line()

fit <- smooth.spline(data$x,
                     data$y,
                     df = 8,
                     all.knots = TRUE)

#With cross validation, we realized that the best average MSE is with degrees of freedom = 8
