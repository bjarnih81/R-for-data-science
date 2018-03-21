# Ch. 24 in R4DS: Model building
# http://r4ds.had.co.nz/model-building.html
# 
# Introduction ####
library(tidyverse)
library(modelr)
options(na.action = na.warn)

library(nycflights13)
library(lubridate)
#
ggplot(diamonds, aes(cut, price)) + geom_boxplot()
ggplot(diamonds, aes(color, price)) + geom_boxplot()
ggplot(diamonds, aes(clarity, price)) + geom_boxplot()
# it looks like lower-quality diamonds are worth more, but this is confounded by weight (carats)
ggplot(diamonds, aes(carat, price)) +
  geom_hex(bins = 50)
# tweaking "diamonds":
#   1. focus on diamonds smaller than 2.5 carats
#   2. log-transform the carat and price variables
diamonds2 <- diamonds %>%
  filter(carat <= 2.5) %>%
  mutate(lprice = log2(price),
         lcarat = log2(carat))
# making it easier to see the relationship between carat and price:
ggplot(diamonds2, aes(lcarat, lprice)) +
  geom_hex(bins = 50)
# remove the linear pattern:
# first make it explicit by fitting a model
modDiamond <- lm(lprice ~ lcarat, data = diamonds2)
# then look at what the model tells us about the data
#   note the back-transformation 
grid <- diamonds2 %>%
  data_grid(carat = seq_range(carat, 20)) %>%
  mutate(lcarat = log2(carat)) %>%
  add_predictions(modDiamond, "lprice") %>%
  mutate(price = 2 ^ lprice)
ggplot(diamonds2, aes(carat, price)) + 
  geom_hex(bins = 50) +
  geom_line(data = grid, color = "red", size = 1)
# check to see if we've removed the linear pattern
diamonds2 <- diamonds2 %>%
  add_residuals(modDiamond, "lresid")
ggplot(diamonds2, aes(lcarat, lresid)) +
  geom_hex(bins = 50)
# re-do our original plots with the residuals instead of price:
ggplot(diamonds2, aes(cut, lresid)) + geom_boxplot()
ggplot(diamonds2, aes(color, lresid)) + geom_boxplot()
ggplot(diamonds2, aes(clarity, lresid)) + geom_boxplot()
#
# A more complicated model
# 
modDiamond2 <- lm(lprice ~ lcarat + color + cut + clarity, data = diamonds2)
summary(modDiamond2)

# plotting
grid <- diamonds2 %>% 
  data_grid(cut, .model = modDiamond2) %>% 
  add_predictions(modDiamond2)

grid <- diamonds2 %>%
  data_grid(cut, 
            lcarat = typical(diamonds2$lcarat), # data_grid() uses "typical()" to fill-in values
            color = typical(diamonds2$color), 
            clarity = typical(diamonds2$clarity)
            ) %>% 
    # original threw an error; replaced with fix from
    # https://github.com/hadley/r4ds/issues/612
  add_predictions(modDiamond2)

ggplot(grid, aes(cut, pred)) +
  geom_point()
#
diamonds2 <- diamonds2 %>%
  add_residuals(modDiamond2, "lresid2")
ggplot(diamonds2, aes(lcarat, lresid2)) +
  geom_hex(bins = 50)
# there are some unusual values with residuals > 1; a residual=2 implies a price 4-times greater than expected
diamonds2 %>%
  filter(abs(lresid2) > 1) %>%
  add_predictions(modDiamond2) %>%
  mutate(pred = round(2 ^ pred)) %>%
  select(price, pred, carat:table, x:z) %>%
  arrange(price)
#
# Exercises
# 
# 1.
# the lines are "breaks" in the data where price info was made human-friendly