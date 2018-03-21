# Iteration
# http://r4ds.had.co.nz/iteration.html
# ++++++++++++++++++++++++++++++++++++++++++
library(tidyverse)
# For loops ----
df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)
#
output <- vector("double", ncol(df)) # 1. output
for (i in seq_along(df)) {           # 2. sequence
  output[[i]] <- median(df[[i]])     # 3. body
}
output
# Exercises ----
# 1.
  # 1.1
output <- vector("double", ncol(mtcars))
for (i in seq_along(mtcars)) {
  output[[i]] <- mean(mtcars[[i]])
}
output
  # 1.2
library(nycflights13)
output <- vector("list", ncol(flights))
names(output) <- names(flights)
for (i in names(flights)) {
  output[[i]] <- class(flights[[i]])
}
output
  # 1.3
iris <- iris
irisUnique <- vector("double", ncol(iris))
names(irisUnique) <- names(iris)
for (i in names(iris)) {
  irisUnique[i] <- length(unique(iris[[i]]))
}
irisUnique
# 1.4
n <- 10
mu <- c(-10, 0, 10, 100)
normals <- vector("list", length(mu))
for (i in seq_along(normals)) {
  normals[[i]] <- rnorm(n, mean = mu[i])
}
normals
# 2.
  # 2.1
out <- ""
for (x in letters) {
  out <- stringr::str_c(out, x)
}
# replace with:
out <- stringr::str_c(letters, collapse = "")
  # 2.2
x <- sample(100)
sd <- 0
for (i in seq_along(x)) {
  sd <- sd + (x[i] - mean(x)) ^ 2
}
std <- sqrt(sd / (length(x) - 1))
# replace with:
sd(x)
# or:
sqrt(var(x))
# or:
sqrt(sum((x - mean(x))^2)/(length(x) - 1))
  # 2.3
x <- runif(100)
out <- vector("numeric", length(x))
out[1] <- x[1]
for (i in 2:length(x)) {
  out[i] <- out[i - 1] + x[i]
}
out
# replace with:
x <- runif(100)
cumsum(x)
# 3.
  # 3.1
humps <- c("five", "four", "three", "two", "one", "no")
for (i in humps) {
  cat(str_c("Alice the camel has ", rep(i, 3), " humps.",
            collapse = "\n"), "\n")
  if (i == "no") {
    cat("Now Alice is a horse.\n")
  } else {
    cat("So go, Alice, go.\n")
  }
  cat("\n")
}
  # 3.2
inBed <- c("ten", "nine", "eight", "seven", "six", "five", "four", "three", "two", "one")
for (i in inBed) {
  cat(str_c("There were ", i, " in the bed"), "\n",
      str_c("And the little one said,"), "\n",
      str_c("\"Roll over! Roll over!\""), "\n",
      str_c("So they all rolled over and one fell out"), "\n")
  if (i == "one") {
    cat(str_c("There was one in the bed"), "\n",
        str_c("And the little one said,"),"\n","\n",
        str_c("I'm lonely!"))
  }
  cat("\n")
}
  # 3.3
bottleFxn <- function(i){# helper function to print correct number of bottles
  if (i > 2) {
    bottles <- str_c(i - 1, " bottles")
  } else if (i == 2) {
    bottles <- "1 bottle"
  } else {
    bottles <- "No more bottles"
  }
  bottles
}
#
beer_bottle <- function(n, beverage) {
  # test whether n >= 1
  for (i in seq(n, 1)) {
    cat(str_c(bottleFxn(i), " of ", beverage," on the wall, ", bottleFxn(i), " of ", beverage,".\n"))
    cat(str_c("Take one down and pass it around, ", bottleFxn(i - 1), " of ", beverage, " on the wall.\n\n"))
  }
  cat(str_c("No more bottles of ", beverage, " on the wall, no more bottles of ", beverage, ".\n"),
      str_c("Go to the store and by some more, ", bottleFxn(n), " of ", beverage, " on the wall.\n"))
}
beer_bottle(5, "soda")
# 4.

library(microbenchmark)
add_to_vector <- function(n) {
  output <- vector("integer", 0)
  for (i in seq_len(n)) {
    output <- c(output, i)
  }
  output
}

microbenchmark(add_to_vector(10000), times = 3)
#
add_to_vector_2 <- function(n) {
  output <- vector("integer", n)
  for (i in seq_len(n)) {
    output[[i]] <- i
  }
  output
}
microbenchmark(add_to_vector_2(10000), times = 3)
# For loop variations ----
  # Modify an existing object
df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)
rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}
for ( i in seq_along(df)) {
  df[[i]] <- rescale01(df[[i]])
}
  # looping patterns
    #besides looping over numeric indices (i.e. "for (i in seq_along(xs))") there are two other forms:
    # Looping over elements: "for (x in xs)"
    # Loop over names: "for (nm in names(xs))"
  #
  # Unknown output lengths; rather than iteratively growing the vector: 
means <- c(0, 1, 2)

output <- double()
for (i in seq_along(means)) {
  n <- sample(100, 1)
  output <- c(output, rnorm(n, means[[i]]))
}
str(output)
    # a better approach is to save the results in a list and then combine in a single vector after the loop is done:
means <- c(0, 1, 2)
out <- vector("list", length(means))
for (i in seq_along(means)) {
  n <- sample(100, 1)
  out[[i]] <- rnorm(n, means[[i]])
}
str(out)
str(unlist(out))
  # Unknown sequence length - the "while" loop
    #A while loop is also more general than a for loop, 
      # because you can rewrite any for loop as a while loop, but you can’t rewrite every while loop as a for loop:
for (i in seq_along(x)) {
  # body
}
# Equivalent to
i <- 1
while (i <= length(x)) {
  # body
  i <- i + 1 
}
# a while loop to find how many tries to get three heads in a row:
flip <- function() sample(c("T", "H"), 1)

flips <- 0
nheads <- 0

while (nheads < 3) {
  if (flip() == "H") {
    nheads <- nheads + 1
  } else {
    nheads <- 0
  }
  flips <- flips + 1
}
flips
##
# Exercises ----
# 1.
files <- dir("H:/R_files/AdvancedR/III-Program/csv_test/", pattern = "\\.csv$", full.names = TRUE)
#
df <- vector("list", length(files))
for (i in seq_along(files)) {
  df[[i]] <- read.csv(files[[i]])
}
df <- bind_rows(df)
#
# 2.
x <- 1:3
print(names(x))

for (nm in names(x)){
  print(nm)
  print(x[[nm]])
}
#
x <- c(a = 1, 2, c = 3)
names(x)
for (nm in names(x)){
  print(nm)
  print(x[[nm]])
}
# 3.

showMean <- function(df, digits = 2) {
  maxstr <- max(str_length(names(df)))
  for (nm in names(df)) {
    if (is.numeric(df[[nm]])) {
      cat(str_c(str_pad(str_c(nm, ":"), maxstr + 1L, side = "right"),# add ":" and 1L to the right side of each name
                format(mean(df[[nm]]), digits = digits, nsmall = digits),# then add the mean with the digits specified
                sep = " "),
          "\n")
    }
  }
}
showMean(iris)
showMean(mtcars)
##
# 4. 
trans <- list( 
  disp = function(x) x * 0.0163871,# converts CC's to cubic inches
  am = function(x) {
    factor(x, labels = c("auto", "manual"))# replaces binary (0/1) in the transmission with the factor "auto"/"manual"
  }
)
for (var in names(trans)) {
  mtcars[[var]] <- trans[[var]](mtcars[[var]])
}
trans[["disp"]]
trans[["disp"]](mtcars[["disp"]])
#
# For loops vs. functionals ----

df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)
colSummary <- function(df, fun) {
  out <- vector("double", length(df))
  for (i in seq_along(df)) {
    out[i] <- fun(df[[i]])
  }
  out
}
colSummary(df, median)
colSummary(df, mean)
# Exercises ----
# 1. 
  # apply() generalizes as.matrix and as.array
# 2.
colSummary2 <- function(df, fun) {
  # test whether column is numeric
  numericCols <- vector("logical", length(df))
  for (i in seq_along(df)) {
    numericCols[[i]] <- is.numeric(df[[i]])
  }
  # indexes of numeric columns
  idxs <- seq_along(df)[numericCols]
  # number of numeric columns
  n <- sum(numericCols)
  out <- vector("double", n)
  for (i in idxs) {
    out[i] <- fun(df[[i]])
  }
  out
}
colSummary2(diamonds, mean)
str(diamonds)
#
# The map functions ----
df <- tibble(
a = rnorm(10),
b = rnorm(10),
c = rnorm(10),
d = rnorm(10)
)
map_dbl(df, mean)
diamonds2 <- diamonds[c('carat', 'depth', 'table', 'price', 'x', 'y', 'z')]
map_dbl(diamonds2, mean)
#
diamonds2 %>% map_dbl(mean)
# map_* uses ... to pass along additional arguments to ".f" each time it is called
map_dbl(diamonds2, mean, trim = 0.5)
# the map functions also preserve names
z <- list(x = 1:3, y = 4:5)
map_int(z, length)

# Shortcuts ----
models <- mtcars %>%
  split(.$cyl) %>%
  map(function(df) lm( mpg ~ wt, data = df))
# purrr allows using "." as a pronoun: 
models <- mtcars %>%
  split(.$cyl) %>%
  map(~lm(mpg ~ wt, data = .))
# "." refers to the current list element
# shorthand for anonymous functions
models %>% 
  map(summary) %>%
  map_dbl(~.$r.squared)
# you can use a string, too
model %>%
  map(summary) %>%
  map_dbl("r.squared")
# you can use an integer to select elements by position:
x <- list(list(1, 2, 3), list(4, 5, 6), list(7, 8, 9))
x %>% map_dbl(2)
# Base R ----

x1 <- list(
  c(0.27, 0.37, 0.57, 0.91, 0.20),
  c(0.90, 0.94, 0.66, 0.63, 0.06), 
  c(0.21, 0.18, 0.69, 0.38, 0.77)
)
x2 <- list(
  c(0.50, 0.72, 0.99, 0.38, 0.78), 
  c(0.93, 0.21, 0.65, 0.13, 0.27), 
  c(0.39, 0.01, 0.38, 0.87, 0.34)
)
threshold <- function(x, cutoff = 0.8) x[x > cutoff]
x1 %>% sapply(threshold) %>% str()
#
x2 %>% sapply(threshold) %>% str()

# Exercises ----
# 1.1 
str(mtcars)
# mtcars$am is a factor, so map_dbl(mtcars, mean) throws an error
mtcars %>%
  select(-am) %>%
  map_dbl(mean)
# i guess not:
map_dbl(mtcars, mean)
# 1.2
library(nycflights13)
map(flights, class)
# or
map_chr(flights, typeof)
# 1.3
map_int(iris, ~length(unique(.)))
# 1.4
mu <- c(-10, 0, 10, 100)
map(mu, rnorm, n= 10)
# 2.
?map_lgl
map_lgl(mtcars, is.factor)
map_lgl(diamonds, is.factor)
# 3.
map(1:5, runif)
# it applies the function to each element in the vector (i.e. 1:5)
# 4.
map(-2:2, rnorm, n = 5)
# it takes n = 5 random normal samples with mean = -2, -1, 0, 1, 2
# 5. 
map(x, function(df) lm(mpg ~ wt, data = mtcars))
#
map(list(mtcars), ~ lm(mpg ~ wt, data = .))
# Dealing with failure ----
safeLog <- safely(log)
str(safeLog(10))
str(safeLog("a"))
#
x <- list(1, 10, "a")
y <- x %>% map(safely(log)) 
str(y)
#
y <- y %>% transpose()
str(y)
# Wroking with errors
isOk <- y$error %>% map_lgl(is_null)
x[!isOk]
y$result[isOk] %>% flatten_dbl()
#
# possibly() always succeeds
x <- list(1, 10, "a")
x %>% map_dbl(possibly(log, NA_real_))
# quietly() captures printed output, messages, and warnings instead of errors
x <- list(1, -1)
x %>% map(quietly(log)) %>% str()

# Mapping over multiple arguments ---- 
# http://r4ds.had.co.nz/iteration.html#mapping-over-multiple-arguments
# 
mu <- list(5, 10, -3)
mu %>%
  map(rnorm, n = 5) %>%
  str()
# what about also varying the SD?
sigma <- list(1, 5, 10)
seq_along(mu) %>% 
  map(~rnorm(5, mu[[.]], sigma[[.]])) %>% 
  str()
# but this obfuscates the intent of the code:
map2(mu, sigma, rnorm, n = 5) %>% str()
# map2, like map, is just a wrapper around a for loop
# pmap extends this
n <- list(1, 3, 5)
args1 <- list(n, mu, sigma)
args1 %>%
  pmap(rnorm) %>%
  str()
#
args2 <- list(mean = mu, sd = sigma, n = n)
args2 %>% 
  pmap(rnorm) %>%
  str()
# since the arguments are all the same length, it makes sense to store them in a data frame:
params <- tribble(
  ~mean, ~sd, ~n,
      5,   1,  1,
     10,   5,  3,
     -3,  10,  5
)
params %>%
  pmap(rnorm)

# Invoking different functions
f <- c("runif", "rnorm", "rpois")
param <- list(
  list(min = -1, max = 1),
  list(sd = 5),
  list(lambda = 10)
)
#
invoke_map(f, param, n = 5) %>% str()
# could use a tribble to make creating these matching pairs a little easier:
sim <- tribble(
  ~f,        ~params,
  "runif", list(min = -1, max = 1),
  "rnorm", list(sd = 5),
  "rpois", list(lambda = 10)
)
sim %>%
  mutate(sim = invoke_map(f, params, n = 10))

# Walk ----
# http://r4ds.had.co.nz/iteration.html#walk
x <- list(1, "a", 3)
x %>%
  walk(print)

# Other patterns of for loops ----
# http://r4ds.had.co.nz/iteration.html#other-patterns-of-for-loops
# predicate functions

iris %>% 
  keep(is.factor) %>%
  str()
#
iris %>%
  discard(is.factor) %>%
  str()
#
x <- list(1:5, letters, list(10))
#
x %>%
  some(is_character)
x %>%
  every(is_vector)
#
x <- sample(10)
x
x %>%
  detect(~ . > 5) # find first element where the predicate is true
#
x %>%
  detect_index(~ . > 5)
#
x %>% 
  head_while(~ . > 5)

x %>% 
  tail_while(~ . > 5)
# Reduce and accumulate
dfs <- list(
  age = tibble(name = "John", age = 30),
  sex = tibble(name = c("John", "Mary"), sex = c("M", "F")),
  trt = tibble(name = "Mary", treatment = "A")
)
dfs %>% reduce(full_join)
#
vs <- list(
c(1, 3, 5, 6, 10),
c(1, 2, 3, 7, 8, 10),
c(1, 2, 3, 4, 8, 9, 10)
)
vs %>% reduce(intersect)
# Exercises ----
# 1.
purrr::every
myEvery <- function(.x, .p, ...){
  for (i in .x) {
    if (!.p(i, ...)) {
      #if any is FALSE then we know not all were TRUE
      return(FALSE)
    }
  }
  TRUE
}
myEvery(1:3, function(x) {x > 1})
myEvery(1:3, function(x) {x > 0})
# 2.
myColSum <- function(df, f, ...){
  map(keep(df, is.numeric), f, ...)
}
myColSum(iris, mean)
myColSum(diamonds, mean)
# 3.
col_sum3 <- function(df, f) {
  is_num <- sapply(df, is.numeric)
  df_num <- df[, is_num]
  
  sapply(df_num, f)
}
df <- tibble(
  x = 1:3, 
  y = 3:1,
  z = c("a", "b", "c")
)
col_sum3(df, mean)# is ok
col_sum3(df[1:2], mean)
col_sum3(df[1], mean)
col_sum3(df[0], mean)