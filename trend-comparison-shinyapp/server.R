#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(devtools)
load_all("../R/bibliographica")
load_all()
library(magrittr)
library(reshape2)
library(gridExtra)
library(knitr)
library(ggmap)
library(dplyr)
library(tidyr)
library(ggplot2)
library(sorvi)
library(tools)
library(RCurl)
library(jsonlite)
library(stringi)
library(plyr)

source("dataset_processing_functions.R")
source("query_ids_functions.R")
source("query_summary_functions.R")
source("get_query_set_list.R")
source("plot_functions.R")
source("search_api_functions.R")

dataset <- augment_original_data(readRDS("../inst/examples/data/estc_df.Rds"))
rest_api_url <- "https://vm0175.kaj.pouta.csc.fi/ecco-search/"
fields <- "&f=heading_index&f=heading_frontmatter&f=contents_index&f=heading_backmatter&f=heading_body&f=contents_frontmatter&f=contents_TOC&f=heading_TOC&f=contents_titlePage&f=contents_body&f=contents_backmatter"

shinyServer(function(input, output) {

  # reactives

  input_comparables <- reactive({
    comparables_list <- list(input$comparative_term1,
                             input$comparative_term2,
                             input$comparative_term3,
                             input$comparative_term4,
                             input$comparative_term5,
                             input$comparative_term6,
                             input$comparative_term7,
                             input$comparative_term8,
                             input$comparative_term9,
                             input$comparative_term10)
    return_list <- list()
    for (comp in comparables_list) {
      if (comp != "") {
        return_list <- append(return_list, comp)
      }
    }
    return(return_list)
  })
  
  api_query_set <- reactive({
    comparables <- input_comparables()
    api_query_set <- get_api_query_set(input$baseline_term, comparables)
    return(api_query_set)
  })
  

  baseline_yearly_pubs <- reactive({
    query_set <- api_query_set()$base_query
    baseline_yearly <-
      get_pubs_yearly_for_query(query_set$query, dataset)
    return(baseline_yearly)
  })
  
  comparable_sets_list <- reactive({
    baseline_yearly = baseline_yearly_pubs()
    comparables_api_queries <- api_query_set()$comparable_queries
    comparable_sets_list <- vector("list", length(comparables_api_queries))
    i <- 1
    for (comparable_api_query_set in comparables_api_queries) {
      relative_hits_yearly <- get_relative_hits_yearly_for_query(
        comparable_api_query_set$query,
        baseline_yearly, dataset)
      query_term <- comparable_api_query_set$term
      query_set <- list(term = query_term,
                        data = relative_hits_yearly)
      comparable_sets_list[[i]] <- query_set
      i <- i + 1
    }
    return(comparable_sets_list)
  })

  output$freq_plot <- renderPlot({
    query_sets_list <- comparable_sets_list()
    print(query_sets_list)
    title <- paste0(input$baseline_term, " --- timeline for co-terms in document")
    plot <- plot_titlecount_relative(title = title, query_sets_list)
    return(plot)
  })
  
  # output$freq_plot_download <- downloadHandler(
  #   filename = function() {
  #     paste0(input$baseline_term, "-co-term-timeline-",Sys.Date(), ".png")
  #   },
  #   content = function(con){
  #     
  #   }
  # )
  # output$paragraph_plot <- renderPlot({
  #   
  # })
})
