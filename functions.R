generate_holidays <- function(year, tv_holiday){
  
  # Reference is first advent
  
  bb_ref_date <- 
    tibble(
      date = seq(ymd(str_c(year, "-11-27")), ymd(str_c(year, "-12-03")), by = "1 day"),
      weekday = weekdays(seq(ymd(str_c(year, "-11-27")), ymd(str_c(year, "-12-03")), by = "1 day"))
    ) %>%
    filter(weekday == "Sonntag") %>%
    pull(date)
  
  date_vec <- c(
    "1. Weihnachtstag" = str_c(year, "-12-25"),
    "2. Weihnachtstag" = str_c(year, "-12-26"),
    "Allerheiligen" = str_c(year, "-11-01"),
    "Buß- und Bettag" = as.character(bb_ref_date - days(11)),
    "Christi Himmelfahrt" = as.character(ymd(Easter(year)) + days(39)),
    "Fronleichnam" = as.character(ymd(Easter(year)) + days(60)),
    "Heilige Drei Könige" = str_c(year, "-01-06"),
    "Internationaler Frauentag" = str_c(year, "-03-08"),
    "Karfreitag" = as.character(ymd(Easter(year)) - days(2)),
    "Mariä Himmelfahrt" = str_c(year, "-08-15"),
    "Neujahr" = str_c(year, "-01-01"),
    "Ostermontag" = as.character(ymd(Easter(year)) + days(1)),
    "Ostersonntag" = as.character(ymd(Easter(year))),
    "Pfingstmontag" = as.character(ymd(Easter(year)) + days(50)),
    "Pfingstsonntag" = as.character(ymd(Easter(year)) + days(49)),
    "Reformationstag" = str_c(year, "-10-31"),
    "Tag der Arbeit" = str_c(year, "-05-01"),
    "Tag der deutschen Einheit" = str_c(year, "-10-03"),
    "Weltkindertag" = str_c(year, "-09-20")
  )
  
  if(tv_holiday == TRUE){
    
    date_vec <- c(
      date_vec, 
      "Heiligabend" = str_c(year, "-12-24"),
      "Silvester" = str_c(year, "-12-31")
    )
    
  } else {
    date_vec
  }
}