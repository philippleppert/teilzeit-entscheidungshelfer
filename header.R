library(tidyverse)
library(httr)
library(jsonlite)
library(timeDate)
library(shiny)
library(DT)

data_processed <- read_rds("api_data.rds")

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