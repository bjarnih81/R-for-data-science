# data import
# http://r4ds.had.co.nz/data-import.html
# -----
library(tidyverse)
# ----
# Skipped getting started
# ----
# Parsing a vector
# ----
str(parse_logical(c("TRUE","FALSE","NA")))
#
str(parse_integer(c('1', '2', '3')))
#
str(parse_date(c("2010-01-01","1979-10-14")))
#
parse_integer(c("1", "231", ".", "456"), na = ".")
x <- parse_integer(c("123", "345", "abc", "123.45"))
x
problems(x)
# ----
# Numbers
# ----
parse_double("1.23")
parse_double("1,23", locale = locale(decimal_mark = ","))
#
parse_number("$100")
parse_number("20%")
parse_number("It costs $123.45")
#
parse_number("$123,456,789")
parse_number("123.456.689", locale = locale(grouping_mark = "."))
parse_number("123'456'789", locale = locale(grouping_mark = "'"))# Switzerland: who knew?
# ----
# Strings
# ----
charToRaw("Hadley")
#
x1 <- "El Ni\xf1o was particularly bad this year"
x2 <- "\x82\xb1\x82\xf1\x82\xc9\x82\xbf\x82\xcd"
x1
x2
#
parse_character(x1, locale = locale(encoding = "Latin1"))
parse_character(x2, locale = locale(encoding = "Shift-JIS"))
#
guess_encoding(charToRaw(x1))
guess_encoding(charToRaw(x2))
# ----
# Factors
# ----
fruit <- c("apple", "banana")
parse_factor(c("apple", "banana", "bananana"), levels = fruit)
# ----
# Dates, date-times, and times
# ----
parse_datetime("2010-10-01T2010")
# if time is omitted it will be set to midnight
parse_datetime("2010-10-01")
#
parse_date("2010-10-01")
#
library(hms)
parse_time("01:10 am")
parse_time("20:10:01")
