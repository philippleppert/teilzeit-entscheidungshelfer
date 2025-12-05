library(tidyverse)
library(httr)
library(jsonlite)
library(timeDate)
library(shiny)
library(DT)

req <- GET("https://get.api-feiertage.de/")

data_req <- content(req, as = "text") %>% fromJSON()

data_processed <- 
  tibble(data_req$feiertage) %>%
  mutate(
    date = ymd(date),
    weekday = weekdays(date)
  )

mapping_bland <-
  c(
    "bw" = "Baden-Württemberg",
    "by" = "Bayern", 
    "be" = "Berlin", 
    "bb" = "Brandenburg", 
    "hb" = "Bremen", 
    "hh" = "Hamburg", 
    "he" = "Hessen", 
    "mv" = "Mecklenburg-Vorpommern", 
    "ni" = "Niedersachsen", 
    "nw" = "Nordrhein-Westfalen", 
    "rp" = "Rheinland-Pfalz", 
    "sl" = "Saarland", 
    "sn" = "Sachsen", 
    "st" = "Sachsen-Anhalt", 
    "sh" = "Schlewswig-Holstein", 
    "th" = "Thüringen"
  )

seq_weekdays <-
  c("Montag", "Dienstag", "Mittwoch", "Donnerstag", "Freitag")