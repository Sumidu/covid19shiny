#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(tidyverse)
library(lubridate)
library(shiny)
library(shinydashboard)
library(flexdashboard)
library(shinyWidgets)
library(plotly)
library(DT)
library(metathis)
library(directlabels)
library(ggthemes)
library(hrbrthemes)
library(png)
library(magick)
library(gridExtra)
library(grid)
library(jsonlite)
library(stringdist)
options(scipen = 9999)



get_png <- function(filename) {
  grid::rasterGrob(png::readPNG(filename), interpolate = TRUE)
}

get_txt <- function(txt) {
  grid::textGrob(txt)
}

add_logoplot <- function(p, size = 0.05){
  imgby = get_png("by.png")
  imgcc = get_png("cc.png")
  txt = get_txt("CC-BY André Calero Valdez/@sumidu")
  xpos = 0.01
  #size = 0.05
  logo <- 
    ggplot() +
    aes(x = 0:1, y = 1) +
    theme_void() +
    annotation_custom(txt, xmin = 0, xmax = 0.5, ymin = 0) +
    annotation_custom(imgby, xmin = 1-xpos, xmax = 1-(xpos + size), ymin = 0)+
    annotation_custom(imgcc, xmin = 1-(xpos+size), xmax = 1-(xpos+(2*size)), ymin = 0) +
    NULL
  gridExtra::grid.arrange(p, logo, heights = c(1-size, size))
}


#p <- qplot(mtcars$mpg) 
#add_logoplot(p, 0.05)

write_ts <- function(key) {
  file_timestamp <- paste0(key,"_updated.rds")
  timestamp <- lubridate::now()
  write_rds(timestamp, file_timestamp)
}

read_ts <- function(key) {
  file_timestamp <- paste0(key,"_updated.rds")
  if(file.exists(file_timestamp)) {
    timestamp <- read_rds(file_timestamp) 
  }
  else timestamp <- Sys.Date() - 1
  timestamp
}

read_cached_file <- function(url, file){
  timestamp <- read_ts(str_sub(file, end = -5))
  now <- lubridate::now()
  data_age <- lubridate::interval(timestamp, now)
  
  if (time_length(data_age, "hours") > 12) {
    data <- read_csv(url)
    write_rds(data, file)
    write_ts(str_sub(file, end = -5))
  } else {
    cat(paste("Data age:", as.duration(data_age), "- Using cached version."))
    data <- read_rds(file)
  }
  data
}

read_confirmed_cases <- function() {
  read_cached_file("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv",
                   "confirmed.rds")
}

read_death_cases <- function() {
  read_cached_file("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv",
                   "deaths.rds")
}

confirmed <- read_confirmed_cases() %>% 
  gather(date, value, -`Province/State`, -`Country/Region`, -Lat, -Long) %>% 
  mutate(date = paste0(date,"20")) %>% 
  mutate(date = mdy(date)) %>% 
  mutate(type = "confirmed")

deaths <- read_death_cases() %>% 
  gather(date, value, -`Province/State`, -`Country/Region`, -Lat, -Long) %>% 
  mutate(date = paste0(date,"20")) %>% 
  mutate(date = mdy(date)) %>% 
  mutate(type = "deceased")


all_data <- bind_rows(confirmed, deaths) %>% 
  group_by(`Country/Region`, date, type) %>% 
  summarise(value = sum(value))

countries <- all_data %>% arrange(desc(value)) %>% 
  pull(`Country/Region`) %>% unique() 



closest_match <- function(x, list) {
  id <- stringdist::amatch(x = x, countries)
  countries[id]
}


json_data <- jsonlite::read_json("https://corona.lmao.ninja/countries")
most_recent_data_pre <- map(json_data, as.data.frame) %>% do.call(rbind,.)
all_cases <- most_recent_data_pre %>% as_tibble() %>% tally(cases) %>% pull(n)

most_recent_data_pre$country <- fct_recode(most_recent_data_pre$country,
                                               "Korea, South" = "S. Korea",
                                               "United Kingdom" = "UK", 
                                                "Cruise Ship" = "Diamond Princess",
                                                "US" = "USA",
                                                "Taiwan*" = "Taiwan"
                                            )

most_recent_data <- most_recent_data_pre %>% 
  as_tibble() %>% 
  select(country, cases, deaths) %>% 
  mutate(date = Sys.Date()) %>% 
  mutate(`Country/Region` = closest_match(country, countries)) %>% 
  filter(!is.na(`Country/Region`)) %>% 
  select(`Country/Region`, date, cases, deaths) %>% 
  pivot_longer(cols = c(cases, deaths), names_to = "type", values_to = "value") %>% 
  mutate(type = str_replace(type, "cases", "confirmed"),
         type = str_replace(type, "deaths", "deceased"))
  
  #room for manual fixes

if(F) {
        most_recent_data %>% pull(`Country/Region`) %>% unique()
        all_data %>% pull(`Country/Region`) %>% unique()
        
        
        most_recent_data %>%   
          bind_rows(all_data) %>% filter(`Country/Region` == "Liechtenstein") %>% View()
        
          all_data %>% 
          mutate(days = as.numeric((date - data_start))) %>% 
          filter(value > 100 || type == "deceased")
        
        start_dataset <- all_data %>% 
          mutate(days = as.numeric((date - data_start))) %>% 
          filter(value > 100 || type == "deceased") %>% 
          group_by(`Country/Region`) %>% 
          summarise(minvalue = min(value),
                    onset = min(days))
        
        start_dataset %>% View()
        
        all_data %>% dplyr::left_join(start_dataset) %>% 
          mutate(matched_days = days - onset) %>%
          mutate(lvalue = log(value + 1))
        
        all_data %>% filter(`Country/Region` %in% c("Germany", "Liechtenstein")) %>%  
          complete(date = seq.Date(min(date), as.Date("2020-8-8"), by="day"), type) %>% 
          arrange(`Country/Region`, type) %>% 
          group_by(`Country/Region`, type) %>% 
          tidyr::fill(value, .direction = "down") %>% View()
          ggplot() + aes(x = date, y = value, color = `Country/Region`) + geom_point() +
          guides(color = FALSE)
          
        all_data %>% filter(`Country/Region` %in% c("Germany", "Liechtenstein")) %>% View()
        #most_recent_data %>% filter(country != `Country/Region`)
        #countries
}


# NOTE TO SELF... THIS breaks the visualization somehow???
# What happens with the data here?
# Join most recent data in
all_data <- most_recent_data %>%   
  bind_rows(all_data, .)  
  #arrange(`Country/Region`, type, date) %>% 
  #complete(date = seq.Date(min(date), as.Date(max(date)), by="day"), type) %>%
  #arrange(`Country/Region`, type, date) %>% 
  #group_by(`Country/Region`, type) %>% 
  #tidyr::fill(value, .direction = "down") 




data_start <- all_data %>% pull(date) %>% min()
data_end <- all_data %>% pull(date) %>% max()

# UI ----


ui <- function(request) {
  shinydashboard::dashboardPage(
  dashboardHeader(title = "CoronaTacker"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(withMathJax(),
                fluidRow(
                  column(width = 4,
                    box(width = 12,
                      h4("Total cases:", scales::number(all_cases)),
                      pickerInput("country_selector", "Select Countries", countries, multiple = TRUE, 
                                  options = list(`actions-box` = TRUE),
                                  #selectize = TRUE, 
                                  selected = c("US", "Germany", "Italy", "France", "Spain", "Korea, South")),
                      sliderInput("cases_limit", "Pick #cases for alignment", min = 1, max = 500, value = 100),
                      sliderInput("start_date", "Limit Duration", min = 0, max = 100, value=c(0,100)),
                      checkboxInput("scalesfree", "Free Y-Scale", value = TRUE),
                      checkboxInput("logscale", "Logarithmic Y-Scale", value = TRUE),
                      checkboxInput("labelshow", "Show case counts", value = FALSE),
                      uiOutput("fitselector"),
                      bookmarkButton()
                    ),
                    # Labels ----
                    box(width = 12, title = "Change Labels", collapsible = TRUE, collapsed = TRUE,
                      textInput("titletxt", "Title", value = "Comparison of case trajectories by country"),
                      textInput("yscale", "Y-Axis", value = "Count"),
                      textInput("xscalepre", "X-Axis prefix", value = "Days after"),
                      textInput("xscalepost", "X-Axis postfix", value = "confirmed cases were reached."),
                      textInput("cases_txt", "Case label", value = "confirmed"),
                      textInput("fatalities_txt", "Fatality label", value = "deceased"),
                      textInput("legend_txt", "Legend label", value = "Country/Region")
                      ),
                    box(width = 12, 
                      shiny::div("This web-app uses data from the Github repository provided by",
                                 a("Johns Hopkins CSSE.", href="https://github.com/CSSEGISandData/COVID-19"),
                                 "The data is typically 1 day behind current data."),
                      br(),
                      div("Last data update:", format(data_end, "%d-%B-%Y") ," - No warranties."),
                      div("For current data follow this ", 
                          shiny::a("link.", href = "https://www.arcgis.com/apps/opsdashboard/index.html#/bda7594740fd40299423467b48e9ecf6")
                      ),
                      div("Created by André Calero Valdez. Suggestions: Tweet me @sumidu")
                    )
                  ),
                  box(width = 8,
                    plotOutput("distPlot", width = "100%", height = "640")
                    ),
                  box(width = 8,
                      h4("Log-Linear model fit"),
                      withMathJax(
                        div("This table shows the exponential of the log-linear model fit as a percentage value and the corresponding p-value. ",
                            "A growth rate of 30% indicates that that the following exponential function best approximates the curve:",
                            "$$\\text{cases} = cases_{0} \\times 1.3^{days} + c$$",
                            "The constant cases0 refers the the alignment specified above. The constant c adjusts for small differences and is not reported here.",
                            br(),
                            "A growth rate of 30% would mean approx. 30% increase of cases per day.")),br(),
                      DT::DTOutput("modeltable")    
                    )
                  
                  )
                )
)
}

# SERVER ----

server <- function(input, output) {
  
  
  
  get_data <- reactive({
    all_data <- all_data %>% 
      mutate(days = as.numeric((date - data_start))) %>%   # turn dates into counts
      filter(value > input$cases_limit || type == "deceased") # filter all cases before confirmed threshold
    
    start_dataset <- all_data %>% 
      group_by(`Country/Region`) %>% 
      summarise(minvalue = min(value),
                onset = min(days))
    
    all_data %>% dplyr::left_join(start_dataset) %>% 
      mutate(matched_days = days - onset) %>%
      mutate(lvalue = log(value + 1))
  })
  
  get_ref_line <- reactive({
    rate <- 1 + input$grate / 100
    start <- input$case_limit
    rate <- 1.3
    start = 100
    data.frame(`Country/Region` = c("Reference Line"),
               type = "deceased",
               days = 1:100) %>% 
      mutate(value = start * (rate) ^days) %>% 
      mutate(lvalue = log(value + 1) ) %>% as_tibble()
  })
  
  get_max_day_value <- reactive({
    get_data() %>% pull(matched_days) %>% max() 
  })
  
  output$fitselector <- renderUI({
    pickerInput("model_country", "Model for Growth", choices = input$country_selector, selected = input$country_selector[1])
  })
  
  
  
  
  # Main Plot
  #
  #
  # Plotrender ----
  output$distPlot <- renderPlot({
    req(input$country_selector)
    req(input$scalesfree)
    req(input$model_country)
    # get ranges
    sdate <- input$start_date[1]
    edate <- input$start_date[2]
    
    scaleparam <- "fixed"
    if(input$scalesfree) scaleparam <- "free_y"
    
    # adjust labels
    levels <- (c("confirmed", "deceased"))
    names(levels) <- c(input$cases_txt, input$fatalities_txt)
    
    refl <- get_ref_line()
    
    d <- get_data() %>% 
      filter(value > 0) %>% 
      filter(`Country/Region` %in% input$country_selector) %>% 
      filter(matched_days %in% sdate:edate)
    
    d$type <- fct_recode(d$type, !!!levels)
    p <- d %>% 
      ggplot()+
      aes(x = matched_days, y = value, 
          group = interaction(`Country/Region`, type), 
          color = `Country/Region`,
          shape = `Country/Region`,
          label = value) +
      
      geom_line(size = 1) +
      
      geom_smooth(data = . %>% filter(`Country/Region`== input$model_country),
                  method = "nls",
                  formula = y ~ A * exp( B * x), 
                  method.args = list( start = c( A = 4, B = 0.3 ) ), 
                  se=FALSE, color = "black" ,linetype="dotted",
                  fullrange=FALSE) +
      #geom_point(size = 3)+
      
      facet_wrap(~type, scales = scaleparam, ncol = 1) +
      
      labs(x = paste(input$xscalepre, input$cases_limit, input$xscalepost)) +
      labs(y = input$yscale) +
      labs(color = input$legend_txt) +
      NULL
    # Logscale ??
    if(input$logscale) {
      p <- p + scale_y_log10() + labs(y = paste(input$yscale, "(log)"))
    }
    if (input$labelshow){
      p <- p + geom_label() 
    }
    p + 
      hrbrthemes::theme_ipsum_rc(base_size = 18) + 
      ggtitle(input$titletxt) + 
      theme(legend.position="bottom") + 
      theme(plot.caption = element_text(family = "Roboto Condensed")) +
      theme(plot.title = element_text(size = 24),
            axis.title.x = element_text(size = 16),
            axis.title.y = element_text(size = 16),
            strip.text.x = element_text(size = 18)
      ) +
      scale_x_continuous(expand=c(0, 2)) +
      coord_cartesian(clip = 'off') +
      geom_dl(aes(label = `Country/Region`), method = list(dl.trans(x = x - 0.3, y = y + 0.4), dl.combine("last.points"), cex = 0.8)) -> p
    
    add_logoplot(p)
    
    
    
  })
  
  output$modeltable <- renderDT({
    get_data() %>%
      filter(type == "confirmed") %>% 
      ungroup() %>% 
      select(`Country/Region`, days, lvalue) %>% 
      nest(data = c(days,lvalue)) %>% 
      mutate(
        fit = map(data, ~lm(lvalue ~ days, data=.x)),
        tidied = map(fit, broom::tidy)
      ) %>% 
      unnest(tidied) %>% 
      filter(term == "days") %>% 
      mutate(estimate = exp(estimate)) %>% 
      select(`Country/Region`, estimate, std.error, statistic, p.value) %>% 
      mutate(`Growth Rate` = paste0(round((estimate-1) * 100, 2), "%")) %>% 
      mutate(`p Value` = scales::pvalue(p.value,
                                        accuracy = 0.001, # Number to round to
                                        decimal.mark = ".", # The character to be used to indicate the numeric decimal point
                                        add_p = TRUE)) %>% 
      select(`Country/Region`, `Growth Rate`, `p Value`)
    
    
  })
  
  
}

# Run the application 
enableBookmarking(store = "url")
shinyApp(ui = ui, server = server, options = list("display.mode" = "showcase"))
