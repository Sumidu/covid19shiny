# HEAD ----
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
library(ggrepel)
library(purrr)
library(data.table)

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
  
  if (time_length(data_age, "hours") > 6) {
    data <- read_csv(url)
    write_rds(data, file)
    write_ts(str_sub(file, end = -5))
  } else {
    cat(paste("Data age:", as.duration(data_age), "- Using cached version."))
    if(file.exists(file)) {
      data <- read_rds(file)
    } else {
      data <- read_csv(url)
      write_rds(data, file)
      write_ts(str_sub(file, end = -5))
    }
  }
  data
}

read_confirmed_cases <- function() {
  read_cached_file("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv",
                   "confirmed.rds")
}

read_death_cases <- function() {
  read_cached_file("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv",
                   "deaths.rds")
}


# API DATA ----

read_recent_cases <- function(){
  most_recent_data_pre <- data.frame()
  tryCatch(
    expr = {
      
      json_data <- jsonlite::read_json("https://corona.lmao.ninja/countries")
      #most_recent_data_pre <- map(json_data, as.data.frame) %>% do.call(rbind,.)
      
      dt_list <- map(json_data, as.data.table)
      dt <- rbindlist(dt_list, fill = TRUE, idcol = T) 
      dt %>% 
        as_tibble() %>% 
        select(-countryInfo) %>% 
        distinct() -> most_recent_data_pre
      
      
      most_recent_data_pre$country <- fct_recode(most_recent_data_pre$country,
                                                 "Korea, South" = "S. Korea",
                                                 "United Kingdom" = "UK", 
                                                 "Cruise Ship" = "Diamond Princess",
                                                 "US" = "USA",
                                                 "Taiwan*" = "Taiwan"
      )
      
    },
    error = function(e){ 
      message('Caught an error!')
      print(e)
      return(data.frame())
    },
    warning = function(w){
      message('Caught an warning!')
      print(w)
    },
    finally = {
      message('All done, quitting.')
    }
  )
  
  
  most_recent_data_pre
}



closest_match <- function(x, list) {
  id <- stringdist::amatch(x = x, countries)
  countries[id]
}

get_png <- function(filename) {
  grid::rasterGrob(png::readPNG(filename), interpolate = TRUE)
}

get_txt <- function(txt) {
  grid::textGrob(txt)
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


most_recent_data_pre <- read_recent_cases()

all_cases <- most_recent_data_pre %>% as_tibble() %>% tally(cases) %>% pull(n)
all_deaths <- most_recent_data_pre %>% as_tibble() %>% tally(deaths) %>% pull(n)


if(nrow(most_recent_data_pre) > 0) {
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
  
  all_data <- most_recent_data %>%   
    bind_rows(all_data, .)  
  
  } else {
  most_recent_data <- data.frame()
}
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
                  column(width = 8,
                    infoBox(width = 6, "Total cases", value = scales::number(all_cases), color = "orange"),
                    infoBox(width = 6, "Total deaths", value = scales::number(all_deaths), color = "red"),
                    box(width = 12,
                        uiOutput("distPlotUI"),
                      ),
                    box(width = 12, title = "Growth Rates by Country", collapsible = TRUE, 
                        withMathJax(
                          div(strong("Non-Linear least squares model fit"),br(),
                              "This table shows the parameters of a non-linear model fit as a percentage value and the corresponding p-value.",
                              "The model tries to fit the shown data to an exponential curve. From the results only the rate of change is reported here.",
                              "A growth rate of 30% indicates that that the following exponential function best approximates the curve:",
                              "$$\\text{cases} = cases_{0} \\times 1.3^{days} + c$$",
                              "The constant cases0 refers the the alignment specified above. The constant c adjusts for small differences and is not reported here.",
                              br(),
                              "A growth rate of 30% would mean approx. 30% increase of cases per day.")),br(),
                        DT::DTOutput("modeltable")    
                      ),
                    box(width = 12, title = "Case numbers", collapsible = TRUE,
                        DT::DTOutput("casetable")
                        )
                    
                    ),
                  column(width = 4,
                         box(width = 12, title = "Configure visualization",
                             pickerInput("country_selector", "Select Countries", countries, multiple = TRUE, 
                                         options = pickerOptions(actionsBox = TRUE, 
                                                                 liveSearch = TRUE
                                                        ),
                                         selected = c("US", "Germany", "Italy", "France", "Spain", "Korea, South")),
                             sliderInput("cases_limit", "Pick number of cases for alignment", min = 1, max = 500, value = 100),
                             sliderInput("start_date", "Limit X-axis range", min = 0, max = 100, value=c(0,100)),
                             checkboxInput("scalesfree", "Free Y-axis", value = TRUE),
                             checkboxInput("logscale", "Logarithmic Y-axis", value = TRUE),
                             sliderInput("growth_rate", "Show reference growth rate %", min = 0, max = 100, value = 30),
                             checkboxInput("labelshow", "Show case counts", value = FALSE),
                             uiOutput("fitselector"),
                             bookmarkButton(label = "Generate link for this setup")
                         ),
                         # Labels ----
                         box(width = 12, title = "Customize Figure", collapsible = TRUE, collapsed = TRUE,
                             textInput("titletxt", "Title", value = "Comparison of case trajectories by country"),
                             textInput("yscale", "Y-Axis", value = "Count"),
                             textInput("xscalepre", "X-Axis prefix", value = "Days after"),
                             textInput("xscalepost", "X-Axis postfix", value = "confirmed cases were reached."),
                             textInput("cases_txt", "Case label", value = "confirmed"),
                             textInput("fatalities_txt", "Fatality label", value = "deceased"),
                             textInput("legend_txt", "Legend label", value = "Country/Region"),
                             sliderInput("figure_height", label = "Change figure height", min = 500, max = 2048, value = 800)
                         ),
                         box(width = 12, 
                             shiny::div("This web-app uses data from the Github repository provided by",
                                        a("Johns Hopkins CSSE.", href="https://github.com/CSSEGISandData/COVID-19"),
                                        "The data is typically 1 day behind current data.",
                                        "For most recent data it uses", a("this API.", href= "https://github.com/novelcovid/api"),
                                        "This shiny app is available as source code", a("here.", href="https://github.com/Sumidu/covid19shiny")),
                             br(),
                             div("Last data update:", format(data_end, "%d-%B-%Y") ," - No warranties."),
                             div("For current data follow this ", 
                                 shiny::a("link.", href = "https://www.arcgis.com/apps/opsdashboard/index.html#/bda7594740fd40299423467b48e9ecf6")
                             ),
                             div("Created by André Calero Valdez. Suggestions: Tweet me @sumidu")
                         )
                  ),
                 )
                )
)
}

# SERVER ----

server <- function(input, output) {
  
  
  
  get_data <- reactive({
    
    sdate <- input$start_date[1]
    edate <- input$start_date[2]
    
    all_data <- all_data %>% 
      mutate(days = as.numeric((date - data_start))) %>%   # turn dates into counts
      filter(value > input$cases_limit || type == "deceased") # filter all cases before confirmed threshold
    
    start_dataset <- all_data %>% 
      group_by(`Country/Region`) %>% 
      summarise(minvalue = min(value),
                onset = min(days))
    
    all_data %>% dplyr::left_join(start_dataset) %>% 
      mutate(matched_days = days - onset) %>%
      mutate(lvalue = log(value + 1)) %>% 
      filter(value > 0) %>% 
      filter(`Country/Region` %in% input$country_selector) %>% 
      filter(matched_days %in% sdate:edate) %>% 
      mutate(label = ifelse(`Country/Region` == input$model_country, scales::number(value,big.mark = ","), ""))
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
  
  
  
  # Model UI
  output$fitselector <- renderUI({
    pickerInput("model_country", "Show counts for", choices = input$country_selector, selected = input$country_selector[1])
  })
  
  
  
  
  # Main Plot
  #
  #
  
  output$distPlotUI <- renderUI({
    plotOutput("distPlot", width = "100%", height = input$figure_height)
  })
  
  # Plotrender ----
  output$distPlot <- renderPlot({
    
    ################# START 
    req(input$country_selector)
    req(input$model_country)
    # get ranges
    sdate <- input$start_date[1]
    edate <- input$start_date[2]
    
    ##### Free y scale?
    scaleparam <- "fixed"
    if(input$scalesfree) { scaleparam <- "free_y" }
    
    
    ###### adjust labels to input in form
    levels <- (c("confirmed", "deceased"))
    names(levels) <- c(input$cases_txt, input$fatalities_txt)
    
    
    ##### get filtered data 
    d <- get_data() 
    
    # apply factor recoding (i18n)
    d$type <- fct_recode(d$type, !!!levels)
    
    # get the range of the x-scale
    limit <- d %>% ungroup() %>%  summarize(end = max(matched_days)) %>% head(1) %>% pull(end)
    
    glabel <- paste0(input$growth_rate, "% Growth")
    
    
    # generate reference line ----
    refl <- data.frame(xs = sdate:(limit-2)) %>% 
      mutate(ys = input$cases_limit * (1 + input$growth_rate/100) ^xs) %>% 
      mutate(type = c("confirmed"), 
             `Country/Region` = glabel, label = "") %>% 
      bind_rows(data.frame(xs = sdate:(limit-2)) %>% 
                  mutate(ys = 1 * (1 + input$growth_rate/100) ^xs) %>% 
                  mutate(type = c("deceased"), 
                         `Country/Region` = glabel, label = ""))
    
    
    p <- d %>% 
      ggplot()+
      aes(x = matched_days, y = value, 
          group = interaction(`Country/Region`, type), 
          color = `Country/Region`,
          shape = `Country/Region`,
          label = label) +
      
      #geom_smooth(data = . %>% filter(`Country/Region`== input$model_country),
      #            method = "nls",
      #            formula = y ~ A * exp( B * x), 
      #            method.args = list( start = c( A = 4, B = 0.3 ) ), 
      #            se=FALSE, color = "black" ,linetype="dotted",
      #            fullrange=FALSE) +
      #geom_point(size = 3)+
      #stat_function(fun = fun.1) +
      geom_line(data = refl, mapping = aes(x = xs, y=ys), color = "black", linetype="dotted")  +
      geom_dl(data = refl, aes(x = xs, y=ys, label = `Country/Region`) ,
              method = list(dl.trans(x = x + 0.2, y= y + 0.1), dl.combine("last.points"), rot=25, cex = 0.8), color = "gray" ) +
      geom_line(size = 1) +
      facet_wrap(~type, scales = scaleparam, ncol = 1) +
      
      labs(x = paste(input$xscalepre, input$cases_limit, input$xscalepost)) +
      labs(y = input$yscale) +
      labs(color = input$legend_txt) +
      NULL
    # Logscale ??
    if(input$logscale) {
      p <- p + 
        scale_y_log10() +
        labs(y = paste(input$yscale, "(log)"))  # + annotation_logticks(sides = "l") 
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
      theme(panel.grid.minor.x = element_line(
        colour = "gray",
        size = 0.1)) +
      scale_x_continuous(expand=c(0, 2)) +
      coord_cartesian(clip = 'off') +
      geom_dl(aes(label = `Country/Region`), 
              method = list(dl.trans(x = x + 0.2, y= y + 0.1), dl.combine("last.points"), rot=25, cex = 0.8) 
              )  -> p
    if (input$labelshow){
      p <- p + geom_point() + 
        geom_label_repel(force = 2,
                         nudge_y = 1,
                         direction = "y", 
                         hjust = 0, 
                         show.legend = FALSE, 
                         size = 3, min.segment.length = 0.2) 
    }
    add_logoplot(p) 
    
    
    
  })

  
  # model table ----  
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
      #mutate(`Growth Rate` = paste0(round((estimate-1) * 100, 2), "%")) %>% 
      mutate(`Growth Rate` = estimate - 1 ) %>% 
      mutate(`p Value` = scales::pvalue(p.value,
                                        accuracy = 0.001, # Number to round to
                                        decimal.mark = ".", # The character to be used to indicate the numeric decimal point
                                        add_p = TRUE)) %>% 
      select(`Country/Region`, `Growth Rate`, `p Value`) %>% 
      datatable() %>% 
      formatPercentage(c('Growth Rate'), 1)
  })
  
  # case table ----
  output$casetable <- renderDT({
    most_recent_data_pre %>% 
      select(Country = country, 
             `Confirmed Cases` = cases,
             `Deaths` = deaths,
             `Active Cases` = active,
             `Critical Cases` = critical,
             `Cases per Million` = casesPerOneMillion)
  })
}

# Run the application 
enableBookmarking(store = "url")
shinyApp(ui = ui, server = server, options = list("display.mode" = "showcase"))
