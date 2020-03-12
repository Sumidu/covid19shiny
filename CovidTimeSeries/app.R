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
library(plotly)
library(DT)
library(metathis)
options(scipen = 9999)




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

countries <- all_data$`Country/Region` %>% unique() %>% as.list()

data_start <- all_data %>% pull(date) %>% min()



# Define UI for application that draws a histogram
ui <- fluidPage(
    meta() %>%
        meta_social(
            title = "A comparative visualization of the Corona virus outbreak",
            description = "A small shiny application designed to help understand the trajectory of the corona outbreak between countries.",
            url = "https://github.com/Sumidu/covid19shiny",
            image = "https://github.com/Sumidu/covid19shiny/blob/master/figures/example.png?raw=true",
            image_alt = "A graph plot",
            twitter_creator = "@sumidu",
            twitter_card_type = "summary",
            twitter_site = "@sumidu"
        ),
    # Application title
    titlePanel("Comparing Corona Trajectories"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            withMathJax(),
            selectInput("country_selector", "Select Countries", countries, multiple = TRUE, selectize = TRUE, selected = c("US", "Germany", "Italy", "France", "Iran", "Spain", "Korea, South")),
            sliderInput("cases_limit", "Pick #cases for alignment", min = 1, max = 500, value = 100),
            sliderInput("start_date", "Limit Duration", min = 0, max = 100, value=c(0,100)),
            checkboxInput("scalesfree", "Free Y Scale", value = TRUE),
            checkboxInput("logscale", "Logarithmic Y-Scale", value = TRUE),
            checkboxInput("labelshow", "Show case counts", value = FALSE),
            shiny::p(paste("This web-app uses data from the Github repository provided by Johns Hopkins CSSE at https://github.com/CSSEGISandData/COVID-19.",
                           "The data is typically 1 day behind current data. No warranties.")),
            shiny::p("For current data follow this ", 
                     shiny::a("link.", href = "https://www.arcgis.com/apps/opsdashboard/index.html#/bda7594740fd40299423467b48e9ecf6")
                     ),
            shiny::p("Created by André Calero Valdez. Suggestions: Tweet me @sumidu"),
            #DT::DTOutput("models"),
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot", width = "100%", height = "640"),
           shiny::wellPanel(
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




# Define server logic required to draw a histogram
server <- function(input, output) {

    
    
    get_data <- reactive({
        all_data <- all_data %>% 
            mutate(days = as.numeric((date - data_start))) %>% 
            filter(value > input$cases_limit || type == "deceased")
        
        start_dataset <- all_data %>% 
            group_by(`Country/Region`) %>% 
            summarise(minvalue = min(value),
                      onset = min(days))
        
        all_data %>% dplyr::left_join(start_dataset) %>% 
            mutate(matched_days = days - onset) %>%
            mutate(lvalue = log(value + 1))
    })
    
    get_max_day_value <- reactive({
        get_data() %>% pull(matched_days) %>% max() 
    })
    
    
    output$distPlot <- renderPlot({
        sdate <- input$start_date[1]
        edate <- input$start_date[2]
        scaleparam <- "fixed"
        if(input$scalesfree) scaleparam <- "free_y"
        p <- get_data() %>% 
            filter(`Country/Region` %in% input$country_selector) %>% 
            filter(matched_days %in% sdate:edate) %>% 
            ggplot()+
            aes(x = matched_days, y = value, group = interaction(`Country/Region`, type), color = `Country/Region`,
                label = value) +
            geom_line() +
            geom_point()+
           
            #scale_x_date(date_breaks = "week") +
            NULL +
            facet_wrap(~type, scales = scaleparam, ncol = 1) +
            labs(x = paste("Days after", input$cases_limit, "confirmed cases were reached.")) +
            labs(y = "Count")
        if(input$logscale) {
            p <- p + scale_y_log10() + labs(y = "Count (log-scale)")
        }
        if (input$labelshow){
            p <- p + geom_label()
        }
        p + theme_bw(base_size = 14) + ggtitle("Comparison of case trajectories by Country")
        
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
        
        
    }, ) 
}

# Run the application 
shinyApp(ui = ui, server = server)


