#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(leaflet.extras)
library(rworldmap)
library(janitor)
library(tidyverse)


# Define UI 

whr_19 <- read.csv("raw_data/2019 copy.csv") %>% 
    clean_names() %>% 
    rename(country_name = country_or_region)

ui <- navbarPage(
    "Happiness, Freedom, and The Status of Women",
    tabPanel("Model",
             fluidPage(
                 titlePanel("World Value Survey Questions"),
                 sidebarLayout(
                     sidebarPanel(
                         selectInput("plot_type","Question",
                                     c("Should Women Work?" = "a",
                                       "Is Being a Housewife Fullfiling?" = "b"))),
                         imageOutput("map", height = "auto")))),
    tabPanel("Discussion",
             titlePanel("Discussion"),
             p("Tour of the modeling choices you made and 
              an explanation of why you made them"),
             htmlOutput("inc")),
             leafletOutput(outputId = "freedom_map"),
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"),
             p("I am using the World Value Survey (WVS) dataset which asks people from various countries questions about their values,
               and world views. I am looking on a few questions asked about the status of women in the family and compare the answers between countries."),
             p("The second dataset is the world happiness report from 2019,
               which examins happiness compared to GDP per capita in different counties."),
             p(tags$a(href = "https://www.kaggle.com/unsdsn/world-happiness?select=2019.csv", "World Happiness Report")),
             h3("About Me"),
             p("My name is Shaked and I study Neuroscience. 
             You can reach me at shakedleibovitz@college.harvard.edu."),
             p(tags$a(href = "https://github.com/Shakedi/gov1005-milestone-3", "connect to Github-milestone-3"))))

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$map <- renderImage({
        if(input$plot_type == "a"){            
            list(
                src = "plot_q28.png",
                width = 600,
                height = 600,
                alt = "Should Women Work?")
        }                                        
        else if(input$plot_type == "b"){
            list(
                src = "plot_q29.png",
                width = 600,
                height = 600,
                alt = "Is Being a Housewife Fullfiling?")
        }
    })
    getPage<-function() {
        return(includeHTML("regression_wvs.html"))
    }
    output$inc<-renderUI({getPage()})
    output$freedom_map <- renderLeaflet({
        joinData <- joinCountryData2Map(whr_19,
                                        joinCode = "NAME",
                                        nameJoinColumn = "country_name")
        qpal <- colorNumeric("magma",
                             joinData$freedom_to_make_life_choices,
                             na.color = NA)
        freedom_interactive <- leaflet(joinData, 
                                       options = leafletOptions(attributionControl = FALSE,
                                                                minzoom=1.5)) %>%
            
            # I multiplied the score by 10 to create a scale of 1-10 because the original
            # value is from 0-1 to a boolean question which was averaged
            # defining the values read when hovering on a country:
            
            addPolygons(label= ~stringr::str_c(country_name, ' ',
                                               as.double(round((freedom_to_make_life_choices),
                                                               digits = 2))),
                        labelOptions= labelOptions(direction = 'auto'),
                        weight=1, color='#333333', opacity=1,
                        fillColor = ~qpal(freedom_to_make_life_choices),
                        fillOpacity = 1,
                        highlightOptions = highlightOptions(
                            color='#000000', weight = 2,
                            bringToFront = TRUE, sendToBack = TRUE)
            ) %>%
            addLegend(values = ~freedom_to_make_life_choices,
                      opacity = 1, pal = qpal, 
                      title = htmltools::HTML("Freedom to Make Life Choices<br>2019 World Happiness Report <h5>(from 1- lowest to 10- highest)</h5>"))
        
    })
   
}

# Run the application 
shinyApp(ui = ui, server = server)
