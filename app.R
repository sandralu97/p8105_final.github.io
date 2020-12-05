#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load packages
library(shiny)
library(readr)
library(shinythemes)
library(tidyverse)
library(plotly)
library(ggplot2)

# Load Data

covid <- read_csv("data/covid.csv")
state_poll_20 <- read_csv("data/state_poll_20.csv") %>% 
    pivot_longer(
    biden:trump,
    names_to = "side",
    values_to = "supporting_rate"
)

mean_sr <- state_poll_20 %>% 
    group_by(state, end_date, side) %>% 
    summarize(mean_rate = mean(supporting_rate))


# Define UI
ui <- fluidPage(theme = shinytheme("lumen"),
    titlePanel("Time Trend of Supporting Rate and Covid19 Confirmed Cases"),
    sidebarLayout(
        sidebarPanel(
                        
                selectInput(inputId = "state_choice", label = strong("Choose State"),
                            choices = unique(state_poll_20$state),
                            selected = "florida"),
                        
                dateRangeInput("date_range", strong("Choose Date Range"), start = "2020-03-22", end = "2020-11-21",
                                min = "2020-03-22", max = "2020-11-21"),
                        
                checkboxGroupInput(inputId = "side_choice", label = strong("Choose the Side"), choices = c("biden","trump"), selected = "biden")

                        ),
                    
                    
# Output: Description, lineplot,and reference
        mainPanel(
            plotlyOutput(outputId = "support_time_trend", height = "300px"),
            plotlyOutput(outputId = "covid19_case_trend", height = "300px"),
            textOutput(outputId = "conclusion"),
            tags$a(href = "https://www.realclearpolitics.com/", "Source: Real Clear Politics", target = "_blank")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    pal = c("red", "blue")
    pal = setNames(pal, c("trump", "biden"))
    
    output$support_time_trend <- renderPlotly({
        mean_sr %>% ungroup() %>% 
            filter(
                state == input[["state_choice"]],
                side == input[["side_choice"]],
                end_date %in% input$date_range[1]:input$date_range[2]
            ) %>% 
            mutate(text_label = str_c("Side: ", side, "\nSupporting Rate: ", mean_rate)) %>%
            plot_ly(
                x = ~end_date, y = ~mean_rate, color = ~side, colors = pal, type = "scatter", 
                mode = "markers+lines", alpha = 0.5, text = ~text_label) %>% 
            layout(yaxis = list(range=c(30,60)))
    })
    
    output$covid19_case_trend <- renderPlotly({
        covid %>% 
            filter(
                date %in% input$date_range[1]:input$date_range[2],
                state == input[["state_choice"]],
            ) %>% 
            mutate(text_label = str_c("Positive: ", positive, "\nDeath: ", death)) %>%
            plot_ly(
                x = ~date, y = ~total_case, color = ~as.factor(date), type = "bar", 
                colors = "viridis", alpha = 0.5, text = ~text_label)
    })
    
    output$conclusion <- renderText({
        paste("Our datasets were scraped from Real Clear Politics, which consist of detailed information 
              about the supporting rates of the president candidates from different poll from March to 
              November 2020. According to our output time trend of the supporting rates between Joe Biden 
              and Donald Trump in each of the swing states, Biden had averagely higher supporting rates than 
              Trump across the whole time periods. The supporting rates of both the candidates slightly fluctuated
              but overall remained steady, no great decline or growth are shown, which indicates that both the
              candidates had their stable population of voters respectively in the swing states. Although Covid19
              is one of the biggest public focus of 2020, the increase in Covid19 confrimed cases in the swing states 
              didn't seem to have much impact on the supporting rates of either the candidates.The data of total 
              confirmed covid19 cases are not avaliable for New Mexico, New Hampshire, Iowa, Nevada, Texas and Georgia.")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
