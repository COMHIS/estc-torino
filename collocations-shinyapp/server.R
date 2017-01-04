library(shiny)
library(devtools)
load_all()

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$collocations_table <- renderTable({
    
  })
  
})
