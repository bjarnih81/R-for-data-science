# Dates and times
# http://r4ds.had.co.nz/dates-and-times.html
# ----
library(tidyverse)
library(lubridate)
library(nycflights13)
# ----
today()
now()
# from strings
ymd("2017-01-31")
mdy("January 31st, 2017")
dmy("31-Jan-2017")
#
ymd(20170131)
#
ymd_hms("2017-01-31 20:11:59")
mdy_hms("01/31/2017 08:01")
# from individual components
flights %>%
  select(year, month, day, hour, minute) %>%
  mutate(
    departure = make_datetime(year, month, day, hour, minute)
  )
# ----
make_datetime_100 <- function(year, month, day, time) {
  make_datetime(year, month, day, time %/% 100, time %% 100)
}
#

flights_dt <- flights %>% 
filter(!is.na(dep_time), !is.na(arr_time)) %>% 
  mutate(
    dep_time = make_datetime_100(year, month, day, dep_time),
    arr_time = make_datetime_100(year, month, day, arr_time),
    sched_dep_time = make_datetime_100(year, month, day, sched_dep_time),
    sched_arr_time = make_datetime_100(year, month, day, sched_arr_time)
  ) %>% 
  select(origin, dest, ends_with("delay"), ends_with("time"))
# visualize distribution of departure times across the year
flights_dt %>%
  ggplot(aes(dep_time)) + 
  geom_freqpoly(binwidth = 86400)# 86400 seconds in a day
# over a single day
flights_dt %>%
  filter(dep_time < ymd(20130102)) %>%
  ggplot(aes(dep_time)) +
  geom_freqpoly(binwidth = 600) # 600s = 10 minutes
# ----
# Date-time components
# ----
datetime <- ymd_hms("2016-07-08 12:34:56")
year(datetime)
mday(datetime)# day of the month
yday(datetime)# day of the year
# ----
flights_dt %>%
  mutate(wday = wday(dep_time, label = TRUE)) %>%
  ggplot(aes(x = wday)) +
  geom_bar()
# ----
flights_dt %>%
  mutate(
    minute = minute(dep_time)
  ) %>%
  group_by(minute) %>%
  summarise(
    avg_delay = mean(arr_delay, na.rm = TRUE),
    n = n()
  ) %>%
  ggplot(aes(minute, avg_delay)) +
  geom_line()
# ----
sched_dep <- flights_dt %>%
  mutate(minute = minute(sched_dep_time)) %>%
  group_by(minute) %>%
  summarise(
    avg_delay = mean(arr_delay, na.rm = TRUE),
                     n = n()
  )

ggplot(data = sched_dep, aes(minute, avg_delay)) + 
  geom_line()
# ----
ggplot(sched_dep, aes(minute, n)) +
  geom_line()
# ----
flights_dt %>%
  count(week = floor_date(dep_time, "week")) %>%
  ggplot(aes(week, n)) +
  geom_line()
# ----
flights_dt %>%
  mutate(dep_hour = update(dep_time, yday = 1)) %>%
  ggplot(aes(dep_hour)) +
  geom_freqpoly(binwidth = 300)
# ----
# Exercises
# ----
# 1.
flights_dt %>%
  mutate(time = hour(dep_time) * 100 + minute(dep_time),
         mon = as.factor(month(dep_time))) %>%
  ggplot(aes(x = time, group = mon)) +
  geom_freqpoly(binwidth = 100) +
  facet_grid(~ mon)
#
flights_dt %>%
  mutate(time = hour(dep_time) * 100 + minute(dep_time),
         mon = as.factor(month(dep_time))) %>%
  ggplot(aes(x = time, y = ..density.., group = mon)) +
  geom_freqpoly(binwidth = 100) +
  facet_grid(~ mon)
# ----
# 2.
flights_dt %>%
  mutate(
    dep_sch_diff = (time(dep_time)) - (time(sched_dep_time))
  ) %>% 
  select(dep_time, sched_dep_time, dep_delay, dep_sch_diff)
# ----
# Time spans
# ----
# Durations
b_age <- today() - ymd(19810107)
b_age
as.duration(b_age)
# ----
dseconds(15)
dminutes(10)
dhours(c(12, 24))
ddays(0:5)
dweeks(3)
dyears(1)
# ----
2 * dyears(1)
#
dyears(1) + dweeks(12) + dhours(15)
#
tomorrow <- today() + ddays(1)
last_year <- today() - dyears(1)
# periods
one_pm <- ymd_hms("2016-03-12 13:00:00", tz = "America/New_York")
one_pm
one_pm + days(1)
#
seconds(15)
# ----
flights_dt %>% 
  filter(overnight, arr_time < dep_time) 
#
flights_dt <- flights_dt %>%
  mutate(
    overnight = arr_time < dep_time,
    arr_time = arr_time + days(overnight * 1),
    sched_arr_time = sched_arr_time + days(overnight * 1)
  )
flights_dt %>% 
  filter(overnight, arr_time < dep_time) 
# Intervals
years(1) / days(1)
#
next_year <- today() + years(1)
(today() %--% next_year) / ddays(1)
# To find out how many periods fall into an interval, you need to use integer division:
(today() %--% next_year) %/% days(1)
# ----
# Time Zones
# ----
Sys.timezone()
length(OlsonNames())
#
(x1 <- ymd_hms("2015-06-01 12:00:00", tz = "America/New_York"))
#
(x2 <- ymd_hms("2015-06-01 18:00:00", tz = "Europe/Copenhagen"))
#
(x3 <- ymd_hms("2015-06-02 04:00:00", tz = "Pacific/Auckland"))
x4 <- c(x1, x2, x3)
x4a <- with_tz(x4, tzone = "Australia/Lord_Howe")
#
x4a - x4
