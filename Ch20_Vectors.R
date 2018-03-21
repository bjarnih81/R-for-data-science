# Vectors
# http://r4ds.had.co.nz/vectors.html
# =======================================
library(tidyverse)
# +++++++++++++++++++++++++++++++++
# 2 key properties of vectors:
# Type
typeof(letters)
typeof(1:10)
# Length
x <- list("a", "b", 1:10)
length(x)
# Logical vectors ----
1:10 %% 3 == 0
# Numeric vectors ----
typeof(1)
typeof(1L)
  # Doubles are approximations
x <- sqrt(2) ^ 2
x
x - 2
  # Integers have one special value: NA; while Doubles have four: NA, NaN, Inf, and -Inf
c(-1, 0, 1) / 0
# Character vectors ----
x <- "this is a reasonably long string."
pryr::object_size(x)
#
y <- rep(x, 1000)
pryr::object_size(y)
# Missing values ----
NA # logical
NA_integer_ # integer
NA_real_ # double
NA_character_ # character
# Exercises ----
# 1.
x <- c(0, NA, NaN, Inf, -Inf)
is.finite(x)
!is.infinite(x)
# it looks like is.infinite takes NA and NaN as inputs, 
    #where is.finite does not (it doesn't consider non-numbers finite)
# 2.
# dplyr::near to look at source code
  # if abs(x - y) is less than 1.49e-08 then they are near
# 3.
# an integer can take +/- Infinity + 1(NA)
# 4.
# rounding down:
rd.dwn <- function(x){
  floor(x)
}
x <- c(1.34, 1.65, 1.5)
rd.dwn(x)
# Using atomic vectors ----
x <- sample(20, 100, replace = TRUE)
y <- x > 10
sum(y) # how many are > 10?
mean(y) # what proportion are > 10?
# The most complex type always wins
typeof(c(TRUE, 1L))
typeof(c(1L, 1.5))
typeof(c(1.5, "a"))
# scalars and recycling rules ----
sample(10) + 100
runif(10) > 0.5
#
1:10 + 1:2
#
1:10 + 1:3
#
tibble(x = 1:4, y = rep(1:2, each = 2))
#
set_names(1:3, c("a", "b", "c"))
# Subsetting ----
# With a numeric vector containing only integers
x <- c("one", "two", "three", "four", "five")
x[c(3, 2, 5)]
# 
x[c(1,1,5,5,5,2)]
#
x[c(-1, -3, -5)]
# With a logical vector
x <- c(10, 3, NA, 5, 8, 1, NA)
#
x[!is.na(x)] # all non-missing values of x
x[x %% 2 == 0] # all even or missing values of x
# subset (a named vector) with a character vector
x <- c(abc = 1, def = 2, xyz = 5)
x[c("xyz", "def")]
# Exercises ----
# 1.
# mean(is.na(x)) will tell me the proportion of values that are NA
# sum(!is.finite(x)) will tell me the number of values in x that are either NA, NaN, -Inf, or Inf
x <- c(0, NA, NaN, Inf, -Inf)
!is.finite(x)
# 2.
# is.vector returns a vector 
# 3.
setNames
purrr::set_names #set_names adds checks: x has to be a vector, 
    # and the length of the object and names have to be the same
# 4.
x <- c(0:9)
# 4.1
lastVal <- function(x){
  if (length(x)){
  x[[length(x)]]
  } else {
  x
  }
}
lastVal(x)
# 4.2
evenPos <- function(x){
  if (length(x)) {
  x[x %% 2 == 0]
  } else {
  x
  }
}
evenPos(x)
# 4.3
notLast <- function(x){
  if (length(x)) {
  x[-(length(x))]
  } else {
    x
  }
}
notLast(x)
# 4.4
evenNoNa <- function(x){
  x[!is.na(x) & (x %% 2 == 0)]
}
evenNoNa(x)
# 5.
x <- c(seq(-10,10,1), NA, NaN)
x[-which(x > 0)]
x[x <= 0] # this treats NaN as NA
# 6.
x[24] # you get a NA
#
x <- c(abc = 1, def = 2, xyz = 5)
x["hij"]# you also get a NA
# Recursive vectors (lists) ----
x <- list(1, 2, 3)
x
str(x)
#
xNamed <- list(a = 1, b = 2, c = 3)
str(xNamed)
# Lists can contain a mix of objects
y <- list("a", 1L, 1.5, TRUE)
str(y)
#
z <- list(list(1, 2), list(3, 4, 5))
str(z)
# Visualising lists
x1 <- list(c(1, 2), c(3, 4))
x2 <- list(list(1, 2), list(3, 4))
x3 <- list(1, list(2, list(3)))
# subsetting
a <- list(a = 1:3, b = "a string", c = pi, d = list(-1, -5))
# '[' extracts a sub-list; the result will always be a list
str(a[1:2])
#
str(a[4])
# '[[' extracts a single component from a list; it removes a level of hierarchy from the list
str(a[[1]])
# 
str(a[[4]])
# '$' is shorthand for extracting named elements of a list; 
    # it works similarly to '[[' except you dont have to use quotes
a$a
a[["a"]]
# Attributes ----
x <- 1:10
attr(x, "greeting")
#
attr(x, "greeting") <- "Hi!"
attr(x, "farewell") <- "Bye"
attributes(x)
# 3 important attributes useed to implement fundamental parts of R:
  # Names: used to name elements of a vector
  # Dimensions: dims for short, make a vector behave like a matrix
  # Class: used to implement the S3 object-oriented system
as.Date
methods("as.Date")
#
getS3method("as.Date", "default")
getS3method("as.Date", "numeric")
# Augmented Vectors ----
# Factors
x <- factor(c("ab", "cd", "ab"), levels = c("ab", "cd", "ef"))
typeof(x)
attributes(x)
# Dates and date-times
x <- as.Date("1971-01-01")
unclass(x)
typeof(x)
attributes(x)
#
x <- lubridate::ymd_hm("1970-01-01 01:00")
unclass(x)
typeof(x)
attributes(x)
# Tibbles: augmented lists
tb <- tibble(x = 1:5, y = 5:1)
typeof(tb)
attributes(tb)
