#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- navbarPage(
    "Happiness and The Status of Women",
    tabPanel("Model",
             fluidPage(
                 titlePanel("Model Title"),
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(
                             "plot_type",
                             "Plot Type",
                             c("Option A" = "a", "Option B" = "b")
                         )),
                     mainPanel(plotOutput("line_plot")))
             )),
    tabPanel("Discussion",
             titlePanel("Discussion"),
             p("Tour of the modeling choices you made and 
              an explanation of why you made them")),
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

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
