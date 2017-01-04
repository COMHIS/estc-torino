
library(shiny)
library(jsonlite)
library(devtools)
load_all("../bibliographica")
load_all("../estc")
load_all()
source("./shotgun_plot_functions.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  mds_data <- reactive({
    if(input$search_string != "") {
      mds_df <- get_mds_data(input$search_string)
      return(mds_df)
    } else {
      return(NULL)
    }
  }) 

  output$shotgunPlot <- renderPlotly({
    plot <- get_shotgun_plot(mds_data())
    return(plot)
  })
  
})
