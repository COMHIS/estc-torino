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
  
  includeCSS("styles.css"),
  
  # Application title
  titlePanel("Subcorpus inspector"),
  
  fluidRow(
    column(8,
           textInput("api2_query",
                     "ECCO API2 paragraph search query:",
                     "")
    ),
    column(4,
           # submitButton("Query API", icon("question"))
           actionButton(inputId = "api_button", 
                        label = "Query API", 
                        icon = icon("question"))
    )
  ),
  textOutput("helper_text"),
  hr(),
  dataTableOutput("query_results"),
  textInput(inputId = "ids_identifier", "Save with name:", placeholder = "write this down somewhere!"),
  actionButton("save_button", "Save IDs")
  
))
