# Strings
# http://r4ds.had.co.nz/strings.html
# ----
library(tidyverse)
library(stringr)
# ----
string1 <- "This is a string"
string2 <- 'If I want to include a "quote" inside a string, I use single quotes'
#
double_quote <- "\""
single_quote <- '\''
#
x <- c("\"","\\")
x
writeLines(x)
#
?'"'# for special character help (e.g."\n" for new line, "\t" for tab)
#
x <- "\u00b5"
x
#
str_length(c("a", "R for data science", NA))
# Combine strings
str_c("x", "y")
str_c("x", "y", "z")
#
str_c("x","y", sep = ',')
#
x <- c("abc", NA)
str_c("|-", x, "-|")
str_c("|-", str_replace_na(x), "-|")
#
str_c("prefix-", c("a", "b", "c"), "-suffix")
#
name <- "Hadley"
time_of_day <- "morning"
birthday <- FALSE

str_c(
  "Good ", time_of_day, " ", name,
  if (birthday) " and HAPPY BIRTHDAY",
  "."
)
#
str_c(c("x", "y", "z"), collapse = ", ")
# ----
# Subsetting strings
# ----
x <- c("Apple", "Banana", "Pear")
str_sub(x, 1, 3)
str_sub(x, -3, -1)
# 
str_sub(x, 1, 1) <- str_to_lower(str_sub(x, 1, 1))
x
# Locales
str_to_upper(c('i', 'ı'))
# but in Turkey:
str_to_upper(c('i', 'ı'), locale = "tr")
#
x <- c("apple", "eggplant", "banana")
str_sort(x, locale = 'en')
str_sort(x, locale = "haw")# hawaii: why do you sort like this?
# ----
# Exercises
# ----
# 1.
# paste can be used to combine strings and characters; paste0 does this without adding spaces; 
    # these are equivalent to "str_c"
# 2.
# the 'sep' argument is the string inserted between arguments to 'str_c', while collapse is the string used to
    # separate any elements of the character vector into a character vector of length 1
# 3.
# ----
# Matching patterns with regular expressions
# ----
x <- c("apple", "banana", "pear")
str_view(x, "an")
#
str_view(x, ".a.")
#
dot <- "\\."
writeLines(dot)
str_view(c("abc", "a.c", "bef"), "a\\.c")
#
x <- "a\\b"
writeLines(x)
str_view(x, "\\\\")
# ----
# Exercises
# ----
# 2.
x <- "a\"\'\\b"
writeLines(x)
str_view(x, "\"\'\\\\")
# 3.
# it will match any patterns that are a dot followed by any character, repeated 3 times:
str_view(c(".a.b.c", ".a.b", "....."), c("\\..\\..\\.."))
# ----
# Anchors
# ----
x <- c("apple", "banana", "pear")
# 'a' at the beginning of a string
str_view(x, "^a")
# 'a' at the end of a string
str_view(x, "a$")
#
x <- c("apple pie", "apple", "apple cake")
str_view(x, "apple")

str_view(x, "^apple$")
# ----
# Exercises
# ----
# 1.
x <- c("$^$", "ab$^$cd")
writeLines(x)
str_view(x, "\\$\\^\\$")
# 2.
str_view(stringr::words, "^y", match = TRUE)# start with 'y'
str_view(stringr::words, "x$", match = TRUE)# end with 'x'
str_view(stringr::words,"^...$", match = TRUE)# are exactly 3 letters long
str_view(stringr::words, ".......", match = TRUE)# have 7 or more letters
# ----
# Character classes and alternatives
# ----
str_view(c('grey', 'gray'), 'gr(e|a)y')
# ----
# Exercises
# ----
# 1.
str_view(stringr::words, "^(a|e|i|o|u)", match = TRUE)
    # also:
str_view(stringr::words, "^[aeiou]", match = TRUE)
#
'%ni%' <- function(x, y)!('%in%'(x, y))
cons <- letters[letters %ni% c('a','e','i','o','u')]
cons2 <- str_c(cons, collapse = '|')
str_view(stringr::words, "[^aeiou]", match = TRUE)
# not sure
# 
str_view(stringr::words, "^ed$|[^e]ed$", match = TRUE)
#
str_view(stringr::words, "i(ng|se)$", match = TRUE)
#
str_view(stringr::words, "(cei|[^c]ie)", match = TRUE)
str_view(stringr::words, "(cei|[^c]ei)", match = TRUE)
# regular expressions are frustrating
# ----
# Repetition
# ----
x <- "1888 is the longest year in Roman numerals: MDCCCLXXXVIII"
str_view(x, "CC?") # 0 or 1
str_view(x, "CC+") # 1 or more
str_view(x, "C[LX]+")
#
str_view(x, "C{2}")# exactly 2 times
str_view(x, "C{2,}")# 2 or more times
str_view(x, "C{2,3}")# 2 or 3 times
# ----
# Exercises
# ----
# 1.
  # ? = {0,1}
  # + = {1,}
  # * = {0,}
# 2.

