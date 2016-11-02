
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(devtools)
load_all("R/bibliographica")
library(bibliographica)
load_all("R/estc")
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
source("R/rmdshiny_report_plot_functions.R")
source("R/rmdshiny_setup_functions.R")
source("R/rmdshiny_get_subset.R")
source("R/rmdshiny_get_custom_ids.R")
source("R/rmdshiny_query_summary_functions.R")
source("R/rmdshiny_get_top_authors.R")
source("R/rmdshiny_get_top_titles.R")
source("R/rmdshiny_get_top_publishers.R")


nchar <- 40
ntop <- 20
time_window <- 10
dataset <- readRDS("data/estc_df.Rds")
dataset <- augment_original_data(dataset, time_window)

shinyServer(function(input, output) {
  
  # mydate <- params$start # not used
  selected.place <- "All" # params$place
  # time.window <- params$time.window
  
  selected_language <- "any" # params$language
  # selected_document_type <- params$document.type
  selected_document_type <- "All"
  idsource <- "data/equality.csv" # params$idsource
  
  output$booksVsPamphletsPlot <- renderPlot({
    
    min.year <- input$range_years[1]
    max.year <- input$range_years[2]
    
    data_subset <- get_subset(dataset,
                              min.year,
                              max.year,
                              selected_language,
                              selected_document_type,
                              selected.place)
    
    custom.ids <- get_custom_ids(data_subset$place_subsetted, idsource)
    filtered_dataset <- subset(data_subset$place_subsetted,
                               id %in% custom.ids$id)
    
    books_vs_pamphlets_plot <- plot_books_vs_pamphlets(filtered_dataset)
    return(books_vs_pamphlets_plot)
  })
  
  # output$toptitlesPlot <- renderPlot({
  #   plot_top_titles_by_title_count(df0, nchar)
  # })
  
})
