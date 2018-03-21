# Functions
# http://r4ds.had.co.nz/functions.html
# ----
# You should consider writing a function whenever you’ve copied and pasted a block of code more than twice
# ----
df <- tibble::tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

df$a <- (df$a - min(df$a, na.rm = TRUE)) / 
  (max(df$a, na.rm = TRUE) - min(df$a, na.rm = TRUE))
df$b <- (df$b - min(df$b, na.rm = TRUE)) / 
  (max(df$b, na.rm = TRUE) - min(df$b, na.rm = TRUE))# fixed Hadley's mistake
df$c <- (df$c - min(df$c, na.rm = TRUE)) / 
  (max(df$c, na.rm = TRUE) - min(df$c, na.rm = TRUE))
df$d <- (df$d - min(df$d, na.rm = TRUE)) / 
  (max(df$d, na.rm = TRUE) - min(df$d, na.rm = TRUE))
# This code only has one input: df$(x)
x <- df$a
(x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
# still some duplication; shorten:
rng <- range(x, na.rm = TRUE)
(x - rng[1]) / (rng[2] - rng[1])
# turning it into a function
rescale01 <- function(x){
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}
rescale01(c(0, 5, 10))
#
rescale01(c(-10, 0, 10))
#
rescale01((c(1,2,3,NA,5)))
# back to the original
df$a <- rescale01(df$a)
df$b <- rescale01(df$b)
df$c <- rescale01(df$c)
df$d <- rescale01(df$d)
#
x <- c(1:10, Inf)
rescale01(x)
#
rescale01 <- function(x){
  rng <- range(x, na.rm = TRUE, finite = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}
rescale01(x)
# ---
# Exercises 2 ----
# ---
# 1.
# if there were a single NA rescale01 would return all NAs
rescaleF <- function(x){
  rng <- range(x, na.rm = FALSE)
  (x - rng[1]) / (rng[2] - rng[1])
}
x <- c(1, NA, 2, 3, 4)
rescaleF(x)
# ---
# 2.
rescale02 <- function(x) {
  rng <- range(x, na.rm = TRUE, finite = TRUE)
  y <- (x - rng[1]) / (rng[2] - rng[1])
  y[y == -Inf] <- 0
  y[y == Inf] <- 1
  y
}
rescale02(c(Inf, -Inf, 1:5))
# ---
# 3.
# a.
  # mean(is.na(x))
x <- c(NA, 2:7, NA, 9:13)
mean(is.na(x)) # returns proportion of 'x' which is NA
prop.na <- function(x){
  sum(is.na(x))/length(x)
}
prop.na(x)
# b.
  # x / sum(x, na.rm = TRUE)
x / sum(x, na.rm = TRUE)# returns each value of x divided by the sum of x, allowing for NA
std.x <- function(x){
  x / sum(x, na.rm = TRUE)
}
std.x(x)
# c.
sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)# coefficient of variation
coef.var <- function(x){
  sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)
}
coef.var(x)
# ---
# 4.
# variance
var.fxn <- function(x){
  x <- x[!is.na(x)]
  n <- length(x)
  m <- mean(x)
  sq_err <- (x - m) ^ 2
  sum(sq_err) / (n - 1)
}
var.fxn(x)
var(x, na.rm = TRUE)
#
skew.fxn <- function(x){
  x <- x[!is.na(x)]
  mn <- mean(x)
  n <- length(x)
  #
  num <- (1 / n) * sum((x - mn) ^ 3)
  den <- (1 / (n - 1) * sum((x - mn) ^ 2)) ^ (3/2)
  #
  num / den
}
skew.fxn(x)
library(moments)
skewness(x, na.rm = TRUE)
# ---
# 5.
x <- c(1:4,NA,6,7,NA,NA,10)
y <- c(NA,2,3,NA,NA,6,7,8,NA,10)
both_na_position <- function(x, y){# returns the position that the two vectors share
  x.1 <- which(is.na(x))
  y.1 <- which(is.na(y))
  intersect(x.1, y.1)
}
both_na_position(x, y)
#
both_na <- function(x, y){
  sum(is.na(x) & is.na(y))
}
both_na(x,y)
# ---
# 6.
is_directory <- function(x) file.info(x)$isdir
is_directory(x)
# 7.
library(dplyr)
foo_foo <- function(){
  little_bunny()
}
threat <- function(){
  give_chances(from = good_fairy,
               to = foo_foo,
               number = chances,
               condition = "Don't behave",
               consequence = turn_into_goon)
}
lyric <- function(){
  foo_foo %>%
    hop(through = forest) %>%
    scoop(up = field_mouse) %>%
    bop(on = head)
  
  down_came(good_fairy)
  said(good_fairy,
       c("Little bunny Foo Foo",
         "I don't want to see you",
         "Scooping up the field mice",
         "And bopping them on the head"))
}
lyric()
threat(3)
# not sure why thise doesn't work (actually, why it should work...) ---
# 
# Functions are for humans and computers -----
# ---
# Conditional execution ----
# ?`if`
hasName <- function(x) {
  nms <- names(x)
  if (is.null(nms)){
    rep(FALSE, length(x))
  } else {
    !is.na(nms) & nms !=""
  }
}
# Code style ----
# Good
if (y < 0 && debug) {
  message("Y is negative")
}
# Exercises ----
# 1.
# "if" returns the value of the expression evaluated, while ifelse returns a vector of TRUE/FALSE
# 2.
library(lubridate)
greeting <- function(time = now()){
  hr <- hour(time)
  if (hr < 12) {
    print("Good morning")
  } else if (hr < 17) {
    print("Good afternoon")
  } else {
    print("Good evening")
  }
}
greeting(ymd_h("2017-03-05:6"))
greeting(ymd_h("2017-03-05:13"))
greeting(ymd_h("2017-03-05:20"))
# 3.
x <- 14
fizzbuzz <- function(x) {
  if (x %% 3 == 0 & x %% 5 == 0) {
  "fizzbuzz"
  } else if (x %% 5 == 0) {
    "buzz"
  } else if (x %% 3 == 0) {
    "fizz"
  } else {
  x
  }
}
fizzbuzz(3)
fizzbuzz(5)
fizzbuzz(15)
fizzbuzz(17)
fizzbuzz("dog")
fizzbuzz(NA)
fizzbuzz(3.3)
# 4.
temp <- runif(1, -10, 35)
if (temp <= 0) {
  "freezing"
} else if (temp <= 10) {
  "cold"
} else if (temp <= 20) {
  "cool"
} else if (temp <= 30) {
  "warm"
} else {
  "hot"
}
#
temp <- seq(-10, 50, by = 5)
cut(temp, c(-Inf, 0, 10, 20, 30, Inf), right = TRUE,
    labels = c("freezing", "cold", "cool", "warm", "hot"))
# 5.
switch(2, "one", "two", "three")
# 6.
x <- "e"
switch(x,
       a = ,
       b = "ab",
       c = ,
       d = "cd")
# Function arguments ----
mean_ci <- function(x, conf = 0.95) {
  se <- sd(x) / sqrt(length(x))
  alpha <- 1 - conf
  mean(x) + se * qnorm(c(alpha / 2, 1 - alpha / 2))
}
x <- runif(100)
mean_ci(x)
mean_ci(x, conf = 0.99)
#
wt_mean <- function(x, w){
  sum(x * w) / sum(w)
}
wt_var <- function(x, w){
  mu <- wt_mean(x, w)
  sum(w * (x - mu) ^ 2) / sum(w)
}
wt_sd <- function(x, w){
  sqrt(wt_var(x, w))
}
wt_mean(1:6, 1:3)
# no error because R recycles vectors
# instead program a stopping rule
wt_mean <- function(x, w) {
  if (length(x) != length(w)) {
    stop("`x` and `w` must be the same length", call. = FALSE)
  }
  sum(w * x) / sum(w)
}
wt_mean(1:6, 1:3)
# using stopifnot
wt_mean <- function(x, w, na.rm = FALSE) {
  stopifnot(is.logical(na.rm), length(na.rm) == 1)
  stopifnot(length(x) == length(w))
  
  if (na.rm) {
    miss <- is.na(x) | is.na(w)
    x <- x[!miss]
    w <- w[!miss]
  }
  sum(w * x) ? sum(w)
}
wt_mean(1:6, 6:1, na.rm = "foo")
# Note that when using stopifnot() you assert what should be true rather than checking for what might be wrong.
# Dot-dot-dot ----
sum(1,2,3,4,5,6,7,8,9,10)
stringr::str_c("a","b","c","d","e","f")
#
commas <- function(...) stringr::str_c(..., collapse = ", ")
commas(letters[1:10])
#
rule <- function(..., pad = "-"){
  title <- paste0(...)
  width <- getOption("width") - nchar(title) - 5
  cat(title, " ", stringr::str_dup(pad, width), "\n", sep = "")
}
rule("Important Output")
# misspelled arguments do not raise an error
x <- c(1, 2)
sum(x, na.mr = TRUE)
# Exercises ----
# 1.
commas(letters, collapse = "-")
# because we set a default collapse argument and used "...", 
    # which R evaluates as "str_c(letters, collapse = ", ", collapse = "-")
# 2.
rule <- function(..., pad = "-+"){
  title <- paste0(...)
  width <- getOption("width") - nchar(title) - 5
  cat(title, " ", stringr::str_dup(pad, width), "\n", sep = "")
}
rule("Important Output")
# this doesnt work because the pad has 2-times as many "pad" characters now
# we could calculate the width of the pad to match the width of the screen
rule <- function(..., pad = "-"){
  title <- paste0(...)
  width <- getOption("width") - nchar(title) - 5
  padchar <- nchar(pad)
  cat(title, " ", 
      stringr::str_dup(pad, width %/% padchar),
      # if not a multiple then fill-in the remaining characters
      stringr::str_sub(pad, 1, width %% padchar),
      "\n", sep = "")
}
rule("Important Output")
rule("Important Output", pad = "-+")
rule("Important Output", pad = "-+&")
# 3.
x <- runif(100)
mean(x)
mean(x, trim = .001)
mean(x, trim = .01)
mean(x, trim = .1)
mean(x, trim = .2)
# this might be useful for taking a mean that is robust to outliers
# 4.
# those arguments are different ways of calculating correlation coefficients
    # the default is pearson; the others (kendall and spearman) are rank-based measures (i.e. for non-parametric data)
# Return values ----
complicatedFunction <- function(x, y, z){
  if (length(x) == 0 || length(y) == 0) {
    return(0)
  }
  # complicated code here
}
# pipeable functions
show_missings <- function(df) {
  n <- sum(is.na(df))
  cat("Missing values: ", n, "\n", sep = "")
  
  invisible(df)
}
show_missings(mtcars)
x <- show_missings(mtcars)
class(x)
dim(x)
#
mtcars %>%
  show_missings() %>%
  mutate(mpg = ifelse(mpg < 20, NA, mpg)) %>%
  show_missings()
# Environment ----
f <- function(x) {
  x + y
}
y <- 100
f(10)
y <- 1000
f(10)
#
`+` <- function(x, y){
  if (runif(1) < 0.1) {
    sum(x, y)
  } else {
    sum(x, y) * 1.1
  }
}
table(replicate(1000, 1 + 2))
rm(`+`)
