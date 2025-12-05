source("header.R")

source("functions.R")

ui <- fluidPage(
  fluidRow(
    br(),
  column(
    12,
    column(
      6, 
      sliderInput("time", "Zeitraum auswählen", min = 2026, max = 2070, value = 2057,
                  sep = ".", width = "600px")
      ),
    column(
      3, 
      checkboxInput("tv_holidays", "Heiligabend und Silvester frei?", value = F)
      ),
    column(
      3, 
      selectInput("bland", "Bundesland des Arbeitgebers auswählen", choices = unname(mapping_bland))
      )
    ),
  column(12, column(6, DTOutput("table1")),  column(6, DTOutput("table2")))
  ))

server <- function(input, output, session) {
  
  rvals <- reactiveValues(
    years = NULL,
    holiday_data = NULL
  )
  
  observe({
  
    rvals$years <- 2026:input$time
    
    rvals$holiday_data <- 
      map_dfr(rvals$years, ~generate_holidays(year = .x, tv_holiday = input$tv_holidays)) %>%
      pivot_longer(cols = everything(), names_to = "holiday", values_to = "date") %>%
      mutate(
        date = ymd(date),
        year = year(date),
        weekday = weekdays(date)
      ) %>%
      left_join(
        data_processed %>% select(fname, bw:th) %>% distinct(fname, .keep_all = T),
        by = c("holiday" = "fname")
      ) %>%
      # add tv_holidays if NAs exist
      mutate(across(bw:th, ~if_else(is.na(.x), "1", .x)))
    
  })
  
  output$table1 <- renderDT({
    
    res <- 
      rvals$holiday_data %>%
      filter(!!sym(names(keep(mapping_bland,~.x==input$bland))) == 1) %>%
      group_by(weekday) %>%
      summarise(n = n()) %>%
      ungroup() %>%
      filter(!(weekday %in% c("Samstag", "Sonntag")))
    
    res[match(seq_weekdays, res$weekday),] %>%
      select(Wochentag = weekday, "Anzahl Feiertage" = n) %>%
      datatable(rownames = F, options = list(dom = "t"))
    
  })
  
  output$table2 <- renderDT({
    
    res <-
      rvals$holiday_data %>%
      filter(!!sym(names(keep(mapping_bland,~.x==input$bland))) == 1) %>%
      filter(!(weekday %in% c("Samstag", "Sonntag"))) %>%
      group_by(holiday, weekday) %>%
      summarise(n = n()) %>%
      ungroup() %>%
      pivot_wider(names_from = weekday, values_from = n) %>%
      select(Feiertag = holiday, all_of(c(seq_weekdays)))
    
    datatable(res, rownames = F, options = list(dom = "t", pageLength = 25))
    
  })
  
}

shinyApp(ui, server)