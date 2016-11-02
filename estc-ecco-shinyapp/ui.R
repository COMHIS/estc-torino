
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
      selectInput("idsource",
                  "Queryfile:",
                  choices = list("equality.csv",
                                 "liberty.csv",
                                 "rebellion.csv",
                                 "revolution.csv",
                                 "ungodly.csv"),
                  selected = "equality.csv"),
      sliderInput("range_years",
                  "Years:",
                  min = 1500,
                  max = 1880,
                  value = c(1700, 1800)),
      selectInput("publication_place",
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
                  selected = "any"),
      selectInput("time_window",
                  "Time segments: NOT IMPLEMENTED YET!",
                  choices = list(20,
                                 10,
                                 5,
                                 1),
                  selected = 10),
      selectInput("document_type",
                  "Document type:",
                  choices = list("All", 
                                 "Books",
                                 "Pamphlets"),
                  selected = "All")
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Books/Pamphlets",
                 plotOutput("books_vs_pamphlets_plot")),
        tabPanel("Author titlecount",
                 plotOutput("title_count_top_10_authors_plot")),
        tabPanel("Top places",
                 plotOutput("top_places_by_titlecount_plot"),
                 plotOutput("top_places_titlecount_and_query_hits_plot"))
      )
    )
  )
))
