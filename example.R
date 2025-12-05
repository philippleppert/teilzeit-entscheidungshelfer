library(tidyverse)
library(httr)
library(jsonlite)
library(timeDate)

req <- GET("https://get.api-feiertage.de/")

data_req <- content(req, as = "text") %>% fromJSON()

data_processed <- 
  tibble(data_req$feiertage) %>%
  mutate(
    date = ymd(date),
    weekday = weekdays(date)
  )

years <- 2026:2057



x <- 
  map_dfr(years, ~generate_holidays(year = .x, tv_holiday = T)) %>%
  pivot_longer(cols = everything(), names_to = "holiday", values_to = "date") %>%
  mutate(
    date = ymd(date),
    year = year(date),
    weekday = weekdays(date)
  )

y <- 
  x %>%
  left_join(
    data_processed %>% select(fname, bw:th) %>% distinct(fname, .keep_all = T),
    by = c("holiday" = "fname")
  ) %>%
  # add tv_holidays if NAs exist
  mutate(across(bw:th, ~if_else(is.na(.x), "1", .x)))

y %>%
  filter(be == 1) %>%
  group_by(weekday) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  filter(!(weekday %in% c("Samstag", "Sonntag")))

y %>%
  filter(be == 1) %>%
  filter(!(weekday %in% c("Samstag", "Sonntag"))) %>%
  group_by(holiday, weekday) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  pivot_wider(names_from = weekday, values_from = n) %>%
  select(Feiertag = holiday, all_of(c(seq_weekdays)))

count_thursday <-
  y %>%
  filter(be == 1) %>%
  filter(!(weekday %in% c("Samstag", "Sonntag"))) %>% 
  arrange(date) %>%
  mutate(
    diff_date = date - lag(date),
    diff_date2 = if_else(diff_date != 1, lead(diff_date), NA)
  ) %>%
  filter((diff_date != 1 & diff_date2 != 1) | is.na(diff_date)) %>%
  filter(weekday == "Donnerstag") %>%
  summarise(count = n()) 
 
count_tuesday <-
  y %>%
  filter(be == 1) %>%
  filter(!(weekday %in% c("Samstag", "Sonntag"))) %>% 
  arrange(date) %>%
  mutate(
    diff_date = date - lag(date),
    diff_date2 = if_else(diff_date != 1, lead(diff_date), NA)
  ) %>%
  filter((diff_date != 1 & diff_date2 != 1) | is.na(diff_date)) %>%
  filter(weekday == "Dienstag") %>%
  summarise(count = n()) 

fails <-
  y %>%
  filter(be == 1) %>%
  filter((weekday %in% c("Samstag", "Sonntag"))) %>%
  summarise(count = n()) 
