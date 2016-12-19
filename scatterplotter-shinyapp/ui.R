#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("MDS Plotter v0.1"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      textInput(inputId = "search_string", label = "Search string",
                value = "",
                width = NULL,
                placeholder = NULL),
      submitButton(text = "Search")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotlyOutput("shotgunPlot", height = 800, width = 800)
    )
  )
))
