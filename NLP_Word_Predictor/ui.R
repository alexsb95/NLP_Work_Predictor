#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(ggplot2)
library(plotly)


library(shiny)
library(shinydashboard)

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("App", tabName = "app", icon = icon("dashboard")),
        menuItem("Documentation", tabName = "document", icon = icon("th") )
    )
)


body <- dashboardBody(
    tabItems(
        tabItem(tabName = "app",
                fluidRow(
                    box(title = "Word Predictor", 
                        status = "warning",
                        textInput("caption","Type your text here"),
                        actionButton("predict", "Predict")),
                    box(title = "Entered text",
                        verbatimTextOutput("user_input"),
                        textOutput('no_match'),
                        tags$head(tags$style("#no_match{color: red;font-style: italic;}"
                            )
                          )
                        )
                ),
                fluidRow(
                    box(width = 4, 
                        height = 40,
                        background = "light-blue",
                        textOutput("prediction1")),
                    box(width = 4, 
                        height = 40,
                        background = "light-blue",
                        textOutput("prediction2")),
                    box(width = 4, 
                        height = 40,
                        background = "light-blue",
                        textOutput("prediction3"))
                ),
                fluidRow(
                    box(width = 12, 
                        status = "primary",
                        plotlyOutput("plot"))
                )
        ),
        
        tabItem(tabName = "document",
                div(tags$h4("Documentation"),
                    "Link to the documentation of this project",
                    tags$ul(
                      tags$li(tags$a(href="https://rpubs.com/alexsb95/582863", "Analysis of the data and ngrams")),
                      tags$li("Description of the solution (Soon)"),
                      tags$li(tags$a(href="https://alexsb95.github.io/NLP_Work_Predictor/","Displays of the presentation"))
                    )
                )
        )
    )
)


ui <-dashboardPage(
    dashboardHeader(title = "NPL project"),
    sidebar,
    body
  
)

# Define UI for application that draws a histogram
shinyUI(ui)
