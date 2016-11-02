
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

# print(getwd())

library(shiny)
library(devtools)
load_all("../R/bibliographica")
library(bibliographica)
load_all()
library(estc)
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

nchar <- 40
ntop <- 20
time_window <- 10
dataset <- readRDS("../inst/examples/data/estc_df.Rds")
dataset <- augment_original_data(dataset, time_window)


get_idsource_fullpath <- function(idsource) {
  idsource_fullpath <- paste0("../inst/examples/data/", idsource)
  return(idsource_fullpath)
}

get_filtered_dataset <- function(input, dataset) {
  min_year <- input$range_years[1]
  max_year <- input$range_years[2]
  idsource <- get_idsource_fullpath(input$idsource)
  selected_place <- input$publication_place
  time_window <- input$time_window
  selected_language <- input$language
  selected_document_type <- input$document_type

  data_subset <- get_subset(dataset,
                            min_year,
                            max_year,
                            selected_language,
                            selected_document_type,
                            selected_place)
  
  query_ids <- format_query_ids(idsource)
  filtered_dataset <- subset(data_subset$place_subsetted,
                             id %in% query_ids$id)
  return(filtered_dataset)
}

shinyServer(function(input, output) {
  
  # idsource <- "../inst/examples/data/equality.csv" # params$idsource
  
  output$books_vs_pamphlets_plot <- renderPlot({
    
    filtered_dataset <- get_filtered_dataset(input, dataset)
    books_vs_pamphlets_plot <- plot_books_vs_pamphlets(filtered_dataset)
    return(books_vs_pamphlets_plot)
  })
  
  output$title_count_top_10_authors_plot <- renderPlot({

    filtered_dataset <- get_filtered_dataset(input, dataset)
    plot_titlecount_top10_authors <-
      plot_titlecount_timeline_for_top10_authors(filtered_dataset)
    return(plot_titlecount_top10_authors)
  })
    

  
  # output$toptitlesPlot <- renderPlot({
  #   plot_top_titles_by_title_count(df0, nchar)
  # })
  
})
