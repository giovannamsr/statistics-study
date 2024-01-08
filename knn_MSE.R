library(tidyverse)
library(FNN)

# Generated sample ----------------------------------------------------------

n_obs <- 100

set.seed(123)

data <- tibble(x = sort(runif(n = n_obs, min = 8, max = 18)), 
                y = 45*tanh(x/1.9 - 7) + 57 + rnorm(n = n_obs, mean = 0, sd = 4))

data %>% 
  ggplot(aes(x, y)) + 
  geom_point() + 
  theme_bw()


# KNN ---------------------------------------------------------------------

# training and test -------------------------------------------------------

validation <- tibble(x = sort(runif(n = n_obs, min = 8, max = 18)), 
                     y = 45*tanh(x/1.9 - 7) + 57 + rnorm(n = n_obs, mean = 0, sd = 4)) 

#tibble to compare the results
results <- tibble(k = 1:100, mse = NA_real_)

knn.reg(train = data$x, 
        test = matrix(data$x[1:10]), 
        y =  data$y, 
        k = 10)$pred

for (i in 1:nrow(results)){
  #ajust the knn with k = i
  y_fit <- knn.reg(train = data$x, 
                   test = matrix(validation$x), 
                   y =  data$y, 
                   k = i)
  #calculates the MSE for k = i
  results$mse[i] <- mean((validation$y - y_fit$pred)^2)
}

results %>%
  arrange(mse)

x_pred <- seq(8, 18, 0.101)

tibble(ajuste = x_pred,
       y_fit = knn.reg(train = data$x, 
                       test = matrix(validation$x), 
                       y =  data$y, 
                       k = 5)$pred) %>% 
  ggplot(aes(ajuste, y_fit)) + 
  geom_point(data = data, aes(x, y)) +
  geom_step(color = "red")

# Cross Validation --------------------------------------------------------

n_lots <- 10
n_obs_lot <- n_obs/n_lots

mse_aux <- vector("numeric", n_lots)

lots <- sample(rep(1:n_lots, each = n_obs_lot))

results2 <- tibble(df = 2:(n_obs - n_obs_lot),
                   mse = NA_real_)

for (i in 1:nrow(results2)) {
  for (j in 1:n_lots) {
    
    # calculates the MSE of lot j and stores it in a vector
    fit <- knn.reg(train = data$x, 
                   test = matrix(validation$x), 
                   y =  data$y, 
                   k = i)
    mse_aux[j] <-  mean((validation$y - fit$pred)^2)
  }
  
  results2$mse[i] <- mean(mse_aux)
  #store the average of the vector with the MSE in results2$mse[i]
}

results2 %>%
  arrange(mse)

results2 %>%
  ggplot(aes(df, mse)) +
  geom_point() +
  geom_line()

tibble(ajuste = x_pred,
       y_fit = knn.reg(train = data$x, 
                       test = matrix(validation$x), 
                       y =  data$y, 
                       k = 6)$pred) %>% 
  ggplot(aes(ajuste, y_fit)) + 
  geom_point(data = data, aes(x, y)) +
  geom_step(color = "red")

#With cross validation, we realized that the best average MSE is with k = 6
