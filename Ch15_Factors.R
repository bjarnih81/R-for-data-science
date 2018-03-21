# Factors
# http://r4ds.had.co.nz/factors.html
# ----
library(tidyverse)
library(forcats)
# ----
# Creating Factors
# ----
x1 <- c("Dec", "Apr", "Jan", "Mar")
x2 <- c("Dec", "Apr", "Jam", "Mar")
#
month_levels <- c(
  "Jan", "Feb", "Mar", "Apr",
  "May", "Jun", "Jul", "Aug",
  "Sep", "Oct", "Nov", "Dec"
)
y1 <- factor(x1, levels = month_levels)
sort(y1)
#
y2 <- factor(x2, levels = month_levels)
y2
# make levels appear in order of the first appearance in the data
f1 <- factor(x1, levels = unique(x1))
f1
# or
f2 <- x1 %>% factor() %>% fct_inorder()
f2
# access levels
levels(f2)
# ----
# General social survey
# ----
gss <- forcats::gss_cat
#
gss %>%
   count(race)
#
ggplot(gss, aes(race)) +
  geom_bar()
# ----
# Exercises
# ----
# 1.
ggplot(gss, aes(rincome)) +
  geom_bar()
levels(gss$rincome)
# ----
# Modifying factor order
# ----
relig_summary <- gss %>%
  group_by(relig) %>%
  summarise(
    age = mean(age, na.rm = TRUE),
    tvhours = mean(tvhours, na.rm = TRUE),
    n = n()
  )
ggplot(relig_summary, aes(tvhours, relig)) +
  geom_point()

ggplot(relig_summary, aes(tvhours, fct_reorder(relig, tvhours))) +
  geom_point()
#
relig_summary %>%
  mutate(relig = fct_reorder(relig, tvhours)) %>%
  ggplot(aes(tvhours, relig)) +
  geom_point()
#
rincome_summary <- gss %>%
  group_by(rincome) %>%
  summarise(
    age = mean(age, na.rm = TRUE),
    tvhours = mean(tvhours, na.rm = TRUE),
    n = n()
  )
ggplot(rincome_summary, aes(age, fct_reorder(rincome, age))) +
  geom_point()
#
ggplot(rincome_summary, aes(age, fct_relevel(rincome, "Not applicable"))) +
  geom_point()
# fct_reorder2
by_age <- gss %>%
  filter(!is.na(age)) %>%
  group_by(age, marital) %>%
  count() %>%
  mutate(prop = n / sum(n))

ggplot(by_age, aes(age, prop, colour = marital)) +
  geom_line(na.rm = TRUE)
# ----
# Modify factor levels
# ----
gss %>% count(partyid)
#
gss %>%
  mutate(partyid = fct_recode(partyid,
                              "Republican, strong" = "Strong republican",
                              "Republican, weak"      = "Not str republican",
                              "Independent, near rep" = "Ind,near rep",
                              "Independent, near dem" = "Ind,near dem",
                              "Democrat, weak"        = "Not str democrat",
                              "Democrat, strong"      = "Strong democrat"
                              )) %>%
  count(partyid)
# combine levels
gss %>%
  mutate(partyid = fct_recode(partyid,
                              "Republican, strong"    = "Strong republican",
                              "Republican, weak"      = "Not str republican",
                              "Independent, near rep" = "Ind,near rep",
                              "Independent, near dem" = "Ind,near dem",
                              "Democrat, weak"        = "Not str democrat",
                              "Democrat, strong"      = "Strong democrat",
                              "Other"                 = "No answer",
                              "Other"                 = "Don't know",
                              "Other"                 = "Other party"
                              )) %>%
  count(partyid)
# collapsing many levels with fct_collapse
gss %>%
  mutate(partyid = fct_collapse(partyid,
                                other = c("No answer", "Don't know", "Other party"),
                                rep = c("Strong republican", "Not str republican"),
                                ind = c("Ind,near rep", "Independent", "Ind,near dem"),
                                dem = c("Not str democrat", "Strong democrat")
                                )) %>%
  count(partyid)
# lump together small groups
gss %>%
  mutate(relig = fct_lump(relig)) %>%
  count(relig)
    # this probably over-aggregates; use 'n=' instead
gss %>%
  mutate(relig = fct_lump(relig, n = 10)) %>%
  count(relig, sort = TRUE) %>%
  print(n = Inf)
# ----
# Exercises
# ----
# 1.
gss_cat %>%
  mutate(
    partyid = fct_collapse(partyid,
                                other = c("No answer", "Don't know", "Other party"),
                                rep = c("Strong republican", "Not str republican"),
                                ind = c("Ind,near rep", "Independent", "Ind,near dem"),
                                dem = c("Not str democrat", "Strong democrat")
                                )) %>%
  count(year, partyid) %>%
  group_by(year) %>%
  mutate(p = n / sum(n)) %>%
  ggplot(aes(x = year, y = p,
             color = fct_reorder2(partyid, year, p))) +
  geom_point() +
  geom_line() +
  labs(
    color = "Party ID",
    y = "Proportion",
    x = "Year"
  )
# ----
# 2.
levels(gss_cat$rincome)
gss_cat %>% 
  mutate(
    rincome = fct_collapse(rincome,
               "N/A" = c("No answer","Don't know","Refused","Not applicable"),
               "$10000 - 24999" = c("$20000 - 24999","$15000 - 19999","$10000 - 14999"),
               "$4000 - 9999" = c("$8000 to 9999","$7000 to 7999","$6000 to 6999","$5000 to 5999","$4000 to 4999"),
               "Lt $4000" = c("$3000 to 3999","$1000 to 2999","Lt $1000")
               )) %>%
  count(rincome)
    