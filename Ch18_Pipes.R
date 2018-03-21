# Pipes
# http://r4ds.had.co.nz/pipes.html
# ----
library(magrittr)
# ----
diamonds <- ggplot2::diamonds
diamonds2 <- diamonds %>%
  dplyr::mutate(price_per_carat = price / carat)
pryr::object_size(diamonds)
# T-pipe
rnorm(100) %>%
  matrix(ncol = 2) %>%
  plot() %>%
  str()
#
rnorm(100) %>%
  matrix(ncol = 2) %T>%
  plot() %>%
  str()
# exploding pipe (%$%)
mtcars %$%
  cor(disp, mpg)
# assignment pipe
mtcars <- mtcars %>% 
  transform(cyl = cyl * 2)
# can be written
mtcars %<>% transform(cyl = cyl * 2)
