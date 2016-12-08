#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Subcorpus inspector"),
  
  # Sidebar with a slider input for number of bins 
  # sidebarLayout(
  #   sidebarPanel(
  #     textInput("api2_query",
  #               "ECCO API2 paragraph search query:",
  #               ""),
  #     submitButton("Search",
  #                  icon("refresh")),
  #     textOutput("helper_text")
  #   ),
  #   
  #   # Show a plot of the generated distribution
  #   mainPanel(
  #      dataTableOutput("query_results")
  #   )
  # )
  verticalLayout(
    textInput("api2_query",
              "ECCO API2 paragraph search query:",
              ""),
    submitButton("Search",
                 icon("refresh")),
    textOutput("helper_text"),
    dataTableOutput("query_results")
  )
))
