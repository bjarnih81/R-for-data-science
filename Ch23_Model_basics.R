# Ch. 23 Model basics
# http://r4ds.had.co.nz/model-basics.html

# Introduction ####
# packages
library(tidyverse)
library(modelr)
library(ggplot2)
options(na.action = na.warn)
# plot
ggplot(sim1, aes(x, y)) + 
  geom_point()
# generate and plot 250 models
models <- tibble(
  a1 = runif(250, -20, 40),
  a2 = runif(250, -5, 5)
)
ggplot(sim1, aes(x, y)) +
  geom_abline(aes(intercept = a1, slope = a2), data = models, alpha = 1/4) +
  geom_point()
# measure the distance between each point and the model
model1 <- function(a, data){
  a[1] + data$x * a[2]
}
model1(c(7, 1.5), sim1)
# root mean squared deviation
measureDistance <- function(mod, data){
  diff <- data$y - model1(mod, data)
  sqrt(mean(diff ^ 2))
}
measureDistance(c(7, 1.5), sim1)
# compute distance for all models above
sim1Dist <- function(a1, a2){
  measureDistance(c(a1, a2), sim1)
}
models <- models %>%
  mutate(dist = purrr::map2_dbl(a1, a2, sim1Dist))
models
# overlay the best 10 models
ggplot(sim1, aes(x, y)) +
  geom_point(size = 2, color = "grey30") +
  geom_abline(
    aes(intercept = a1, slope = a2, color = -dist),
    data = filter(models, rank(dist) <= 10)
  )
# thinking about all 250 models as observations
ggplot(models, aes(a1, a2)) +
  geom_point(data = filter(models, rank(dist) <= 10), size = 4, color = "red") +
  geom_point(aes(color = -dist))
# grid search
grid <- expand.grid(
  a1 = seq(-5, 20, length = 25),
  a2 = seq(1, 3, length = 25)
) %>% 
  mutate(dist = purrr::map2_dbl(a1, a2, sim1Dist))
grid %>%
  ggplot(aes(a1, a2)) +
  geom_point(data = filter(grid, rank(dist) <= 10), size = 4, color = "red") +
  geom_point(aes(color = -dist))
# overlay all original 10 models on the data
ggplot(sim1, aes(x, y)) +
  geom_point(size = 2, color = "grey30") +
  geom_abline(
    aes(intercept = a1, slope = a2, color = -dist),
    data = filter(grid, rank(dist) <= 10)
  )
# Newton-Raphson algorithm
best <- optim(c(0, 0), measureDistance, data = sim1)
best$par
ggplot(sim1, aes(x, y)) +
  geom_point(size = 2, color = "grey30") +
  geom_abline(intercept = best$par[1], slope = best$par[2])
# using lm
sim1Mod <- lm(y ~ x, data = sim1)
coef(sim1Mod)
# Exercises ####
# 1.
sim1a <- tibble(
  x = rep(1:10, each = 3),
  y = x * 1.5 + 6 + rt(length(x), df = 2)
)
sim1aMod <- lm(y ~ x, data = sim1a)
ggplot(sim1a, aes(x, y)) + 
  geom_point(size = 2, color = "grey30") +
  geom_abline(intercept = sim1aMod$coefficients[1], slope = sim1aMod$coefficients[2])
# Not sure how to answer this; the linear model is sensitive to outliers?
# from https://jrnold.github.io/r4ds-exercise-solutions/model-basics.html
simt <- function(i){
  tibble(
    x = rep(1:10, each = 3),
    y = x * 1.5 + 6 + rt(length(x), df = 2),
    .id = i
  )
}
sims <- map_df(1:12, simt)
ggplot(sims, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  facet_wrap(~ .id, ncol = 4)
# with normal distributions
simNorm <- function(i) {
  tibble(
    x = rep(1:10, each = 3),
    y = x * 1.5 + 6 + rnorm(length(x)),
    .id = i
  )
}
simdfNorm <- map_df(1:12, simNorm)
ggplot(simdfNorm, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  facet_wrap(~ .id, ncol = 4)
#
tibble(
  x = seq(-5, 5, length.out = 100),
  normal = dnorm(x),
  student_t = dt(x, df = 2) 
) %>% 
  gather(distribution, density, -x) %>%
  ggplot(aes(x = x, y = density, colour = distribution)) +
  geom_line()

# 2.
# simulating data
sim1a <- tibble(
x = rep(1:10, each = 3),
y = x * 1.5 + 6 + rt(length(x), df = 2)
)
# using mean-absolute distance rather than root mean squared distance
makePrediction <- function(mod, data){
  mod[1] + mod[2] * data$x
}
#
measureDistance2 <- function(mod, data) {
  diff <- data$y - makePrediction(mod, data)
  mean(abs(diff))
}
# using N-R algorithm
best2 <- optim(c(0, 0), measureDistance2, data = sim1a)
best2$par
ggplot(sim1a, aes(x, y)) +
  geom_point(size = 2, color = "grey30") +
  geom_abline(intercept = best2$par[1], slope = best2$par[2])
# comparing lm to N-R
sim1aMod <- lm(y ~ x, data = sim1a)
ggplot(sim1a, aes(x, y)) + 
  geom_point(size = 2, color = "grey30") +
  geom_abline(intercept = sim1aMod$coefficients[1], slope = sim1aMod$coefficients[2]) + 
  geom_abline(intercept = best2$par[1], slope = best2$par[2], color = "red")
# 3.
# the problem is that an optimum for one parameter isn't guranteed to be an optimum for another
# 
# Visualizing models ####
# Predictions
# Making an evenly-spaced grid of values that covers the region where the data lies:
grid <- sim1 %>%
  data_grid(x)
grid
# adding predictions
grid <- grid %>%
  add_predictions(sim1Mod)
grid
#
ggplot(sim1, aes(x)) +
  geom_point(aes(y = y)) +
  geom_line(aes(y = pred), data = grid, color = "red", size = 1)
# Residuals
sim1 <- sim1 %>% 
  add_residuals(sim1Mod)
sim1
# a frequency polygon to understand the spread of the residuals
ggplot(sim1, aes(resid)) +
  geom_freqpoly(binwidth = 0.5)
# recreating the plot using residuals rather than the original predictor
ggplot(sim1, aes(x, resid)) +
  geom_ref_line(h = 0) +
  geom_point()
# Exercises ####
# 1.
sim1Loess <- loess(y ~ x, data = sim1)
sim1Lm <- lm(y ~ x, data = sim1)

gridLoess <- sim1 %>%
  add_predictions(sim1Loess)

sim1 <- sim1 %>%
  add_residuals(sim1Lm) %>%
  add_predictions(sim1Lm) %>%
  add_residuals(sim1Loess, var = "residLoess") %>%
  add_predictions(sim1Loess, var = "predLoess")

plotSim1Loess <-
  ggplot(sim1, aes(x = x, y = y)) +
  geom_point() +
  geom_line(aes(x = x, y = pred), data = gridLoess, color = "red")
plotSim1Loess
# plotting loess residuals vs. lm residuals
ggplot(sim1, aes(x = x)) +
  geom_ref_line(h = 0) +
  geom_point(aes(y = resid)) +
  geom_point(aes(y = residLoess), color = "red")
# 2.
# add_predictions adds a single column of predictions to the input
# spread_predictions adds a single column of predictions for each model (i.e. "wide")
# gather_predictions adds two columns, model and prediction, for each input row (i.e. "long")
# 3.
# geom_ref_line comes from the modelr package and it adds a reference line to a ggplot2 plot
# this is useful/helpful in examining residuals because it lets us easily identify the zero line
# 4. 
# Using the absolute value lets us examine the magnitude of the residuals, but does not allow us to see whether
#   the model systematicaly over- or under-estimates the residuals
#   
# Formulas and model families ####

df <- tribble(
  ~y, ~x1, ~x2,
  4,  2,   5,
  5,  1,   6
)
model_matrix(df, y ~ x1)
# dropping the intercept
model_matrix(df, y ~ x1 - 1)
# adding more variables
model_matrix(df, y ~ x1 + x2)
#
# Categorical variables
df <- tribble(
  ~ sex, ~ response,
  "male", 1,
  "female", 2,
  "male", 1
)
model_matrix(df, response ~ sex)
#
ggplot(sim2) + 
  geom_point(aes(x, y))
#
mod2 <- lm(y ~ x, data = sim2)
grid <- sim2 %>%
  data_grid(x) %>%
  add_predictions(mod2)
grid
#
ggplot(sim2, aes(x)) +
  geom_point(aes(y = y)) +
  geom_point(data = grid, aes(y = pred), color = "red", size = 4)
#
# Interactions (continuous and categorical)
# 
ggplot(sim3, aes(x1, y)) +
  geom_point(aes(color = x2))
# could fit 2 models to this data:
mod1 <- lm(y ~ x1 + x2, data = sim3)
mod2 <- lm(y ~ x1 * x2, data = sim3)
# visualizing:
#   data_grid() finds all unique values of x1 and x2 and the generates all combinations
#   gather_predictions() adds a prediction to each row
grid <- sim3 %>%
  data_grid(x1, x2) %>%
  gather_predictions(mod1, mod2)
grid
#
ggplot(sim3, aes(x1, y, color = x2)) +
  geom_point() +
  geom_line(data = grid, aes(y = pred)) +
  facet_wrap(~ model)
# which model fits better? look at the residuals
sim3 <- sim3 %>%
  gather_residuals(mod1, mod2)
ggplot(sim3, aes(x1, resid, color = x2)) +
  geom_point() +
  facet_grid(model ~ x2)
# model 1 has not captured some pattern that we can see in b, c, and d, that appears to be better captured in model 2
# 
# Interactions (2 continuous)
# 
mod1 <- lm(y ~ x1 + x2, data = sim4)
mod2 <- lm(y ~ x1 * x2, data = sim4)

grid <- sim4 %>% 
  data_grid(
    x1 = seq_range(x1, 5),# seq_range() instead of data_grid() to get a regularly-spaced grid of 5 values between the
                          # minimum and maximum numbers. 
    x2 = seq_range(x2, 5)
  ) %>%
  gather_predictions(mod1, mod2)
grid
# visualizing
ggplot(grid, aes(x1, x2)) +
  geom_tile(aes(fill = pred)) +
  facet_wrap(~ model)
# but this is partly a trick: our eyes arent very good at discerning shades of color
# instead we can look at it from the side:
ggplot(grid, aes(x1, pred, color = x2, group = x2)) +
  geom_line() +
  facet_wrap(~ model)
ggplot(grid, aes(x2, pred, color = x1, group = x1)) +
  geom_line() +
  facet_wrap(~ model)
#
# Transformations
# 
df <- tribble(
  ~y, ~x,
  1,  1,
  2,  2,
  3,  3
)
model_matrix(df, y ~ x^2 + x)
#vs.
model_matrix(df, y ~ I(x^2) + x)
#
# the poly() helper function
# 
model_matrix(df, y ~ poly(x, 2))
# this may be a problem, however, because outliers may shoot off into +/- infinity
# safer to use natural splines from the splines package
library(splines)
model_matrix(df, y ~ ns(x, 2))
#
sim5 <- tibble(
  x = seq(0, 3.5 * pi, length = 50),
  y = 4 * sin(x) + rnorm(length(x))
)
ggplot(sim5, aes(x, y)) +
  geom_point()
# fit 5 models to this data:
mod1 <- lm(y ~ ns(x, 1), data = sim5)
mod2 <- lm(y ~ ns(x, 2), data = sim5)
mod3 <- lm(y ~ ns(x, 3), data = sim5)
mod4 <- lm(y ~ ns(x, 4), data = sim5)
mod5 <- lm(y ~ ns(x, 5), data = sim5)
#
grid <- sim5 %>%
  data_grid(x = seq_range(x, n = 50, expand = 0.1)) %>%
  gather_predictions(mod1, mod2, mod3, mod4, mod5, .pred = "y")
ggplot(sim5, aes(x, y)) +
  geom_point() +
  geom_line(data = grid, color = "red") +
  facet_wrap(~ model)
#
# Exercises ####
# 
# 1.
mod2 <- lm(y ~ x, data = sim2)
mod2a <- lm(y ~ x - 1, data = sim2)

grid <- sim2 %>%
  data_grid(x) %>%
  spread_predictions(mod2, mod2a)
grid
# the predictions are the exact same
# 2.
# 3.
mod1 <- lm(y ~ x1 + x2, data = sim3)
mod2 <- lm(y ~ x1 * x2, data = sim3)
#
modelMatrixMod1 <- function(.data){
  mutate(.data,
         `x2b` = as.numeric(x2 == "b"),
         `x2c` = as.numeric(x2 == "c"),
         `x2d` = as.numeric(x2 == "d"),
         `x1:x2b` = x1 * x2b,
         `x1:x2c` = x1 * x2c,
         `x1:x2d` = x1 * x2d) %>%
    select(x1, x2b, x2c, x2d, `x1:x2b`, `x1:x2c`, `x1:x2d`)
}
modelMatrixMod1(sim3)
# or:
modelMatrixMod2 <- function(x1, x2){
  out <- tibble(x1 = x1)
  # find levels of x2
  x2 <- as.factor(x2)
  x2lvls <- levels(x2)
  # create an indicator variable for each level
  for (lvl in x2lvls[2:nlevels(x2)]){
    out[[str_c("x2", lvl)]] <- as.numeric(x2 == lvl)
  }
  # create interactions for each level
  for (lvl in x2lvls[2:nlevels(x2)]){
    out[[str_c("x1:x2", lvl)]] <- (x2 == lvl) * x1
  }
  out
}
modelMatrixMod2(sim3)
