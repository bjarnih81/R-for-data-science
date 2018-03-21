# tidy data
# http://r4ds.had.co.nz/tidy-data.html
# ----
library(tidyverse)
# ----
table1
#
table2
#
table3
# spread across 2 tibbles
table4a # cases
table4b # population
# ----
# compute rate per 10,000
table1 %>%
  mutate(rate = cases / population * 10000)
# cases per year
table1 %>%
  count(year, wt = cases)
# visualize changes over time
library(ggplot2)
ggplot(table1, aes(year, cases)) +
  geom_line(aes(group = country), color = 'gray50') +
  geom_point(aes(color = country))
# ----
# Exercises
# ----
# 1. 
# variables are columns, observations are rows, and values are cells in table 1
# table 2 has multiple rows for each country, multiple rows for year within country, 
    #and a 'cases' row and a 'population' row for each year
# table 3 is the worst and has a row for each year per country, 
    #and the cases and population are stored as a single character variable called "rate"
# -----
# 2. 
table2
# 1999
tb2.cases.99 <- filter(table2, type == "cases" & year == 1999)[["count"]]
tb2.pop.99 <- filter(table2, type == 'population' & year == 1999)[["count"]]
tb2.rate.99 <- (tb2.cases.99 / tb2.pop.99) * 10000
# 2000
tb2.cases.00 <- filter(table2, type == "cases" & year == 2000)[["count"]]
tb2.pop.00 <- filter(table2, type == 'population' & year == 2000)[["count"]]
tb2.rate.00 <- (tb2.cases.00 / tb2.pop.00) * 10000
# country
tb2.country <- filter(table2, type == 'cases' & year == 1999)[['country']]

table2_clean <- tibble(
  country = tb2.country,
  rate.1999 = tb2.rate.99,
  rate.2000 = tb2.rate.00
)
#
table4a #cases
table4b # population
#
tb4_clean <- tibble(
  country = table4a[['country']],
  rate1999 = (table4a[['1999']] / table4b[['1999']]) * 10000,
  rate2000 = (table4a[['2000']] / table4b[['2000']]) * 10000
)
# ----
# 3.
table2
table2 %>%
  filter(type == 'cases') %>%
ggplot(aes(year, count)) +
  geom_line(aes(group = country), color = 'gray50') +
  geom_point(aes(color = country))
# ----
# Spreading and gathering
# ----
# gather
tidy4a <- table4a %>%
  gather('1999', '2000', key = "year", value = "cases")
#
tidy4b <- table4b %>% 
  gather('1999', '2000', key = 'year', value = 'population')
# joining
left_join(tidy4a, tidy4b)
# spread
table2
#
spread(table2, key = type, value = count)
# ----
# Exercises
# ----
# 1.
stocks <- tibble(
  year   = c(2015, 2015, 2016, 2016),
  half  = c(   1,    2,     1,    2),
  return = c(1.88, 0.59, 0.92, 0.17)
)
stocks %>% 
  spread(year, return) %>% 
  gather("year", "return", `2015`:`2016`)
# variable type isn't transferred during spread/gather => year goes from numeric to character
# the convert argument tries to convert to the appropriate type
# ----
# 2.
table4a %>% 
  gather(1999, 2000, key = "year", value = "cases")
# need to be in quotes: gather looks for the 1999th and 2000th position
# this works:
table4a %>%
  gather('1999','2000', key = "year", value = "cases")
# ----
# 3.
people <- tribble(
  ~name,             ~key,    ~value,
  #-----------------|--------|------
  "Phillip Woods",   "age",       45,
  "Phillip Woods",   "height",   186,
  "Phillip Woods",   "age",       50,
  "Jessica Cordero", "age",       37,
  "Jessica Cordero", "height",   156
)
people %>%
  spread('name', key = 'key', value = 'value')
#doesnt work b/c phillip has two 'age' values
people2 <- tribble(
  ~name,             ~key,    ~value, ~time,
  #-----------------|--------|------|-----
  "Phillip Woods",   "age",       45,  1,
  "Phillip Woods",   "height",   186,  1,
  "Phillip Woods",   "age",       50,  2,
  "Jessica Cordero", "age",       37,  1,
  "Jessica Cordero", "height",   156,  1
)
people2 %>%
  spread('name', key = 'key', value = 'value')
# ----
# 4.
preg <- tribble(
  ~pregnant, ~male, ~female,
  "yes",     NA,    10,
  "no",      20,    12
)
#
gather(preg, sex, count, male, female) %>%
  mutate( pregnant = pregnant == 'yes',
          female = sex == 'female') %>%
  select(-sex)
# ----
# Separating and uniting
# ----
# separate
table3 %>% 
  separate(rate, into = c("cases", "population"))
# explicitly specifying the separator
table3 %>% 
  separate(rate, into = c("cases", "population"), sep = "/")
# converting the new values to integers
table3 %>% 
  separate(rate, into = c("cases", "population"), convert = TRUE)
# separating at a position
table3 %>%
  separate(year, into = c('century', 'decade'), sep = 2)
# ----
# unite
table5 %>% 
  unite(new, century, year)
#
table5 %>% 
  unite(new, century, year, sep = '')
# ----
# Exercises
# ----
# 1.
tibble(x = c("a,b,c", "d,e,f,g", "h,i,j")) %>% 
  separate(x, c("one", "two", "three"), extra = "drop")
#
tibble(x = c("a,b,c", "d,e", "f,g,i")) %>% 
  separate(x, c("one", "two", "three"), fill = 'left')
# ----
# 2.
table3 %>%
  separate(year, into = c('century', 'decade'), sep = 2, remove = TRUE)
# its good to have it to make sure nothing goofy happened
# ----
# Missing values
# ----
stocks <- tibble(
  year   = c(2015, 2015, 2015, 2015, 2016, 2016, 2016),
  qtr    = c(1, 2, 3, 4, 2, 3, 4),
  return = c(1.88, 0.59, 0.35, NA, 0.92, 0.17, 2.66)
)
# ----
# An explicit missing value is the presence of an absence; an implicit missing value is the absence of a presence
# ----
stocks %>%
  spread(year, return)
#
stocks %>%
  spread(year, return) %>%
  gather(year, return, '2015':'2016', na.rm = TRUE)
#
stocks %>%
  complete(year, qtr)
# carrying values forward
treatment <- tribble(
  ~ person,           ~ treatment, ~response,
  "Derrick Whitmore", 1,           7,
  NA,                 2,           10,
  NA,                 3,           9,
  "Katherine Burke",  1,           4
)
treatment %>%
  fill(person)
# ----
# Case Study
# ----
who
#
who1 <- who %>%
  gather(new_sp_m014:newrel_f65, key = "key", value = "cases", na.rm = TRUE)
who1
#
who1 %>%
  count(key)
# 'newrel' -> 'new_rel'
who2 <- who1 %>%
  mutate(key = stringr::str_replace(key, "newrel", "new_rel"))
# splitting into new variables
who3 <- who2 %>%
  separate(key, c("new", "type", "sexage"), sep = "_")
who3
# dropping 'new', "iso2", and "iso3"
who4 <- who3 %>%
  select(-new, -iso2, -iso3)
# splitting sexage into 'sex' and 'age'
who5 <- who4 %>%
  separate(sexage, c('sex', 'age'), sep = 1)
who5
# all in one step:
who_tidy <- who %>%
  gather(code, value, new_sp_m014:newrel_f65, na.rm = TRUE) %>%
  mutate(code = stringr::str_replace(code, "newrel", "new_rel")) %>%
  separate(code, c("new", "var", "sexage")) %>%
  select(-new, -iso2, -iso3) %>%
  separate(sexage, c("sex", "age"), sep = 1)
