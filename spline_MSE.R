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

# spline - training ------------------------------------------------------------------

results <- tibble(gl = 2:100, eqm = NA_real_)

for (i in 1:nrow(results)) {
  #ajust spline with i degrees of freedom
  fit <- smooth.spline(data$x, data$y, df = results$gl[i], all.knots = TRUE)
  
  #calculates the MSE and hold it at results$eqm[i]
  results$eqm[i] <- mean((data$y - predict(fit, data$x)$y)^2)
}

results %>%
  ggplot(aes(x = gl, y = eqm)) +
  geom_point() +
  geom_line()

# validation -------------------------------------------------

n_obs <- 100

set.seed(123)

data <- tibble(x = sort(runif(n = n_obs, min = 8, max = 18)), 
                y = 45*tanh(x/1.9 - 7) + 57 + rnorm(n = n_obs, mean = 0, sd = 4))

validation <- tibble(x = sort(runif(n = n_obs, min = 8, max = 18)),
                    y = 45*tanh(x/1.9 - 7) + 57 + rnorm(n = n_obs, mean = 0, sd = 4))

results2 <- tibble(gl = 2:100, eqm = NA_real_)

for (i in 1:nrow(results)) {
  #ajust spline with i degrees of freedom
  fit <- smooth.spline(data$x, data$y, df = results2$gl[i], all.knots = TRUE)
  
  #calculates the MSE and hold it at results$eqm[i]
  results2$eqm[i] <- mean((validation$y - predict(fit, validation$x)$y)^2)
}

results2 %>%
  ggplot(aes(x = gl, y = eqm)) +
  geom_point() +
  geom_line()


# combine results
results_combine <- bind_rows(
  mutate(results, dataset = "train"),
  mutate(results2, dataset = "validation")
)

ggplot(results_combine, aes(x = gl, y = eqm, color = dataset)) +
  geom_point() +
  geom_line() +
  labs(title = "MSE train x validation", x = "GL", y = "EQM") +
  theme_minimal()
