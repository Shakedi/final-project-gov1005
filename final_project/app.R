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


# Define UI 


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
   
}

# Run the application 
shinyApp(ui = ui, server = server)
