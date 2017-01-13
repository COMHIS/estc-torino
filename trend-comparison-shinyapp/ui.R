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
  titlePanel("Not Google n-gram viewer v0.7"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      textInput("subcorpus",
                "Subcorpus:",
                "",
                placeholder = "leave blank for no subcorpus"),
      radioButtons("mode", label = NULL,
                   choices = list("paragraph", "document"),
                   selected = "paragraph"),
      radioButtons("graph_geom", label = "Graph style:",
                   choices = list("smooth", "very_smooth", "stacked"),
                   selected = "smooth"),
      radioButtons("blank_total", label = "Total to use for whole ECCO with no subcorpus",
                   choices = list("titles", "paragraphs", "words"),
                   selected = "titles"),
      textInput("baseline_term",
                "Baseline term:",
                "",
                placeholder = "leave blank for whole ECCO"),
      textInput("comparative_term1",
                "Comparative terms:",
                "civility"),
      textInput("comparative_term2",
                NULL,
                ""),
      textInput("comparative_term3",
                NULL,
                ""),
      textInput("comparative_term4",
                NULL,
                ""),
      textInput("comparative_term5",
                NULL,
                ""),
      textInput("comparative_term6",
                NULL,
                ""),
      textInput("comparative_term7",
                NULL,
                ""),
      textInput("comparative_term8",
                NULL,
                ""),
      textInput("comparative_term9",
                NULL,
                ""),
      textInput("comparative_term10",
                NULL,
                ""),
      submitButton("Search",
                   icon("refresh"))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("freq_plot", height = "700px"),
       textOutput("subcorpus_explainer")
    )
  )
))
