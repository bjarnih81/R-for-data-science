# relational data
# http://r4ds.had.co.nz/relational-data.html
# ----
library(tidyverse)
library(nycflights13)
# ----
flights
airlines
airports
planes
weather
# ----
# Exercises
# ----
# 1.
# you would need tailnumber, origin, and destination (latitude and longitude)
# 2. 
# the relationship between airports and weather is through the "origin" variable in weather, 
  # and the faa variable in airports
# 3. 
# it would also define the destination relation
# 4.
# I would make a table with dates of hollidays/special days; this would have to vary by year 
    # (e.g. memorial day isnt always on a specific date)
# ----
# Keys
# ----
#Make sure there arent more than 1 ea. tailnum
planes %>%
  count(tailnum) %>%
  filter(n > 1)
#Make sure there arent more than 1 ea. weather per hour, day, month, year
weather %>%
  count(year, month, day, hour, origin) %>%
  filter(n > 1)
# no explicit key variable in 'flights'
flights %>%
  count(year, month, day, flight) %>%
  filter(n > 1)
#
flights %>%
  count(year, month, day, tailnum) %>%
  filter(n > 1)
# ----
# Exercises
# ----
# 1.
flights_key <-
  flights %>%
  mutate(
    key = row_number(flights$year)
  )
# 2.1
Lahman::Batting %>%
  count(playerID, yearID, stint) %>%
  filter(n > 1)
# 2.2
# dont know why this throws an error?
babynames %>%
  count(year, sex, name) %>%
  filter(n > 1)
# 2.3
atmos %>%
  count(lat, long, year, month) %>%
  filter(n > 1)
# 2.4
fueleconomy::vehicles %>% 
  count(id) %>%
  filter(n > 1)
# 2.5
ggplot2::diamonds %>%
  count(carat, cut, color, clarity, depth, table, price, x, y, z) %>%
  filter(n > 1)
# with all the variables there are still 143 observations with n > 1; there is no key variable
# ----
# Mutating joins
# ----
# making a narrower dataset
flights2 <- flights %>%
  select(year:day, hour, origin, dest, tailnum, carrier)
#
flights2 %>%
  select(-origin, -dest) %>%
  left_join(airlines, by = 'carrier')
# same results:
flights2 %>%
  select(-origin, -dest) %>%
  mutate(name = airlines$name[match(carrier, airlines$carrier)])
#
x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  3, "x3"
)
y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2",
  4, "y3"
)
#
x %>%
  inner_join(y, by = "key")
# ----
# Exercises
# ----
# 1.
delays_dest <- flights %>%
  group_by(dest) %>%
  mutate(
    mean_delay = mean(arr_delay, na.rm = TRUE)
  ) %>%
  inner_join(airports, by = (c(dest = 'faa')))

ggplot(data = delays_dest, aes(lon, lat, size = mean_delay)) +
  borders("state") +
  geom_point() +
  coord_quickmap()
# ----
# 2.
airports2 <- airports[,c('faa','lat','lon')]
airports_dep <- rename(airports2, lat.dep = lat, lon.dep = lon)
airports_arr <- rename(airports2, lat.arr = lat, lon.arr = lon)

flights2 <- flights %>%
  inner_join(airports_arr, by = c(dest = "faa")) %>%
  inner_join(airports_dep, by = c(origin = "faa"))
# 3.
planes
# assuming "year" is age of airplane
planes_year <- planes[,c('tailnum','year')]
flights_delay <- flights[,c('tailnum','arr_delay')] %>%
  left_join(planes_year, by = 'tailnum') %>%
  group_by(year) %>%
  summarise(
    avg_delay = mean(arr_delay, na.rm = TRUE)
  )
ggplot(data = flights_delay, aes(x = year, y = avg_delay)) +
  geom_point()
lm.1 <- lm(avg_delay ~ year, data = flights_delay)
summary(lm.1)
plot(flights_delay$year,flights_delay$avg_delay)
abline(reg = lm.1)
# 4.

weather %>% count(origin, year, month, day, hour) %>%
  filter(n > 1)
flights %>% count(origin, year, month, day, hour) %>%
  filter(n > 1)
# these aren't the key variables for flights, but I'll try anyway
wea_fli <- weather %>%
  inner_join(flights, by = c("origin" = "origin",
                             "year" = "year",
                             "month" = "month",
                             "day" = "day",
                             "hour" = "hour"))
# ----
# precipitation
# ----
wea_fli %>%
  group_by(precip) %>%
  summarise(delay = mean(dep_delay, na.rm = TRUE)) %>%
  ggplot(aes(x = precip, y = delay)) +
    geom_line() + geom_point()
#
raindelay <- wea_fli %>%
  group_by(precip) %>%
  summarise(delay = mean(dep_delay, na.rm = TRUE))
plot(raindelay$precip, raindelay$delay)
lm.2 <- lm(delay ~ precip, data = raindelay)
abline(reg = lm.2)
# ----
# temp
# ----
tempdelay <- wea_fli %>%
  group_by(temp) %>%
  summarise(delay = mean(dep_delay, na.rm = TRUE))
plot(tempdelay$temp, tempdelay$delay)
lm.2 <- lm(delay ~ temp, data = tempdelay)
abline(reg = lm.2)
summary(lm.2)
# significant positive linear relationship between temperatures and delays
# ----
# wind speed
# ----
summary(wea_fli$wind_speed)
# outliers
wea_fli %>% 
  count(wind_speed > 100)
# removing observations with wind speeds > 100 mph
wea_fli2 <- wea_fli[wea_fli$wind_speed <= 100,]
winddelay <- wea_fli2 %>%
  group_by(wind_speed) %>%
  summarise(delay = mean(dep_delay, na.rm = TRUE))
plot(winddelay$wind_speed, winddelay$delay)
lm.2 <- lm(delay ~ wind_speed, data = winddelay)
abline(reg = lm.2)
summary(lm.2)
# positive relationship between wind_speed and delays
# ----
# dewpoint
# ----
dewdelay <- wea_fli %>%
  group_by(dewp) %>%
  summarise(delay = mean(dep_delay, na.rm = TRUE))
plot(dewdelay$dewp, dewdelay$delay)
lm.2 <- lm(delay ~ dewp, data = dewdelay)
abline(reg = lm.2)
summary(lm.2)
# positive relationship between dewpoint and delays
# ----
# pressure
# ----
pressdelay <- wea_fli %>%
  group_by(pressure) %>%
  summarise(delay = mean(dep_delay, na.rm = TRUE))
plot(pressdelay$pressure, pressdelay$delay)
lm.2 <- lm(delay ~ pressure, data = pressdelay)
abline(reg = lm.2)
summary(lm.2)
# negative relationship between pressure and delays
# ----
# Filter joins
# ----
# find top 10 destinations
top_dest <- flights %>%
  count(dest, sort = TRUE) %>%
  head(10)
# find each flight that went to one of those
    # construct a filter yourself
flights %>%
  filter(dest %in% top_dest$dest)
# but what about multiple key variables?
flights %>%
  semi_join(top_dest)
# anti_joins to identify mis-matches
flights %>%
  anti_join(planes, by = 'tailnum') %>%
  count(tailnum, sort = TRUE)
# ----
# Exercises
# ----
# 1. 
miss_tail <- flights %>%
  anti_join(planes, by = 'tailnum') %>%
  arrange(desc(tailnum))
miss_tail <- miss_tail[is.na(miss_tail$tailnum),]
sum(is.na(miss_tail$dep_time))/length(miss_tail$dep_time)
# 100% of plqanes without tailnumbers have no departure time; they were cancelled(?)
# ----
# 2.
flights_gt100 <- flights %>%
  group_by(tailnum) %>%
  count() %>%
  filter(n > 100)
flights %>%
  semi_join(flights_gt100)
# ----
# 3.
library(fueleconomy)
glimpse(vehicles)
semi_join(vehicles, common)
# ----
# 4.
flights %>%
  group_by(year, month, day) %>%
  summarise(total.24 = sum(dep_delay, na.rm = TRUE) + sum(arr_delay, na.rm = TRUE)) %>%
  mutate(
    total.48 = total.24 + lag(total.24) 
  ) %>%
  arrange(desc(total.48)
  )
# ----
# 5.
View(anti_join(flights, airports, by = c('dest' = 'faa')))# should tell us non-U.S. destination flights
View(anti_join(airports, flights, by = c("faa" = "dest")))# are airports with no flights from NYC
# ----
# 6.
multi_carrier <-
  flights %>%
  group_by(tailnum, carrier) %>%
  count() %>%
  filter(n > 1) %>%
  select(tailnum) %>%
  distinct() %>%
  arrange(tailnum)
View(multi_carrier)
