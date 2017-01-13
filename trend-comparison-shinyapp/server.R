
library(shiny)
library(devtools)
load_all("../bibliographica")
load_all("../estc")
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
library(plyr) # this might be a problem

source("query_ids_functions.R")
source("query_summary_functions.R")
source("get_query_set_list.R")
source("plot_functions.R")
source("paragraph_analysis_functions.R")

# dataset <- augment_original_data(readRDS("../data/estc_df.Rds"))
# rest_api_url <- "https://vm0175.kaj.pouta.csc.fi/ecco-search/"
# fields <- "&f=heading_index&f=heading_frontmatter&f=contents_index&f=heading_backmatter&f=heading_body&f=contents_frontmatter&f=contents_TOC&f=heading_TOC&f=contents_titlePage&f=contents_body&f=contents_backmatter"

shinyServer(function(input, output) {

  # reactives

  subcorpus_filter_state <- reactive({
    if (input$subcorpus != "") {
      return(TRUE)
    } else {
      return(FALSE)
    }
  })
    
  dataset <- reactive({
    base_dataset <- augment_original_data(readRDS("../data/estc_df.Rds"))
    if (subcorpus_filter_state()) {
      base_dataset <- get_data_subset_ids_csv(base_dataset, input$subcorpus)
    }
    return(base_dataset)
  })
  
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
    print(input$mode)
    print(typeof(input$mode))
    api_query_set <- get_api2_query_set(input$baseline_term, comparables, input$mode)
    return(api_query_set)
    
  })

  comparable_sets_list <- reactive({
    paragraph_query_set <- api_query_set()
    dataset <- dataset()
    comparable_sets_list <- 
      get_yearly_paragraph_frequencies_list(input$blank_total, paragraph_query_set, dataset, 
                                            subcorpus_filter_state())
    return(comparable_sets_list)
  })

  
  output$freq_plot <- renderPlot({
    query_sets_list <- comparable_sets_list()
    title <- paste0(input$baseline_term, " --- timeline for co-terms in ", input$mode)
    plot <- plot_titlecount_relative(title = title,
                                     query_sets_list,
                                     style = input$graph_geom,
                                     plot_colour = "Paired")
    return(plot)
  })
  
  
  output$subcorpus_explainer <- renderText({
    if (subcorpus_filter_state()) {
      explainer_text <- "With a subcorpus, when no comparative term is provided, the search results are compared 
                         against the yearly total titlecount of the subcorpus. Therefore on a given year: Frequency = (paragraphs in
                         subcorpus with search term) / (number of subcorpus titles). i.e. Average yearly number of paragraphs with the
                         term in books of the subcorpus."
    } else {
      explainer_text <- ""
    }
    return(explainer_text)
  })

})

