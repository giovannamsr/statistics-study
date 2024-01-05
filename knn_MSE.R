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

x_pred <- seq(8, 18, 0.1)

tibble(ajuste = x_pred,
       y_fit = knn.reg(train = data$x, 
                       test = matrix(validation$x), 
                       y =  data$y, 
                       k = 5)$pred) %>% 
  ggplot(aes(ajuste, y_fit)) + 
  geom_point(data = data, aes(x, y)) +
  geom_step(color = "red")

# Cross Validation --------------------------------------------------------


