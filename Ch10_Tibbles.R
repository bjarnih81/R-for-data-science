# http://r4ds.had.co.nz/tibbles.html
# Tibbles
#------
backup_options <- options()
library(tidyverse)
# ----
as.tibble(iris)
#
tibble(
  x = 1:5,
  y = 1,
  z = x ^ 2 + y
)
#
tb <- tibble(
  `:)` = 'smile',
  ` `  = 'space',
  `2000` = 'number'
)
tb
# tribble
tribble(
  ~x, ~y, ~z,
  #--/--/----
  "a", 2, 3.6,
  "b", 1, 8.5
)
# 2 main differences between a tibble and a data.frame:
# Printing:
tibble(
  a = lubridate::now() + runif(1e3) * 86400,
  b = lubridate::today() + runif(1e3) * 30,
  c = 1:1e3,
  d = runif(1e3),
  e = sample(letters, 1e3, replace = TRUE)
)
# Options
nycflights13::flights %>%
  print(n = 10, width = Inf)
# scrollable viewer
nycflights13::flights %>%
  View()
# Subsetting
df <- tibble(
  x = runif(5),
  y = rnorm(5)
)
# extract by name
df$x
df[['x']]
# extract by position
df[[1]]
# to use in a pipe

df %>% .$x
df %>% .[['x']]
# ----
# Exercises
# ----
# 1.
#You can tell it is a tibble because it will say tibble when you print it
print(mtcars)
print(as.tibble(mtcars))
# ----
# 2.
df <- data.frame(abc = 1, xyz = 'a')
df$x
df[, 'xyz']
df[, c('abc', 'xyz')]
#
dft <- as.tibble(df)
dft$x
# tibbles dont do partial matching
dft[, 'xyz']
#
dft[, c('abc', 'xyz')]
# tibble has coerced the integer value "1" to a double(?)
# ----
# 3. extract referance variable with a variable name stored in an object
var <- 'model'
mpg[[var]]
mpg %>% .$var
# ----
# 4.
annoying <- tibble(
  `1` = 1:10,
  `2` = `1` * 2 + rnorm(length(`1`))
)
# 4.1
annoying[[1]]
# 4.2
plot(x = annoying[[1]], y = annoying[[2]])
# 4.3
annoying <- mutate(annoying,`3` = annoying[[2]]/1)
# 4.4 
rename(annoying, `1` = one)
