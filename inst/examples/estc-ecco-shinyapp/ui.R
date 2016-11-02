
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("ESTC Report"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("range_years",
                  "Years:",
                  min = 1500,
                  max = 1880,
                  value = c(1700, 1800)),
      selectInput("puclication_place",
                  "Publication place:",
                  choices = list("All",
                                 "London",
                                 "Edinburgh",
                                 "Dublin",
                                 "Boston Ma",
                                 "Philadelphia Pa",
                                 "Oxford",
                                 "Glasgow",
                                 "New York NY",
                                 "Cambridge",
                                 "Newcastle"),
                  selected = "All"),
      selectInput("language",
                  "Language:",
                  choices = list("any",
                                 "English",
                                 "Latin",
                                 "French",
                                 "German",
                                 "Welsh"),
                  selected = "any")
      
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("booksVsPamphletsPlot")
    )
  )
))
