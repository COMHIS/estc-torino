
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
# library(estc)
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
theme_set(theme_bw(12))

get_idsource_fullpath <- function(idsource) {
  idsource_fullpath <- paste0("../inst/examples/data/", idsource)
  return(idsource_fullpath)
}


get_filtered_dataset_sans_ids <- function(input, dataset) {
  min_year <- input$range_years[1]
  max_year <- input$range_years[2]
  selected_place <- input$publication_place
  # time_window <- input$time_window
  selected_language <- input$language
  selected_document_type <- input$document_type
  
  data_subset <- get_subset(dataset,
                            min_year,
                            max_year,
                            selected_language,
                            selected_document_type,
                            selected_place)
  return(data_subset)
}


get_filtered_dataset <- function(input, dataset) {
  idsource <- get_idsource_fullpath(input$idsource)
  data_subset <- get_filtered_dataset_sans_ids(input, dataset)
  query_ids <- format_query_ids(idsource)
  filtered_dataset <- subset(data_subset$place_subsetted,
                             id %in% query_ids$id)
  filtered_dataset_allplaces <- subset(data_subset$all_places,
                                       id %in% query_ids$id)
  filtered_dataset_list <- list(place_filtered = filtered_dataset,
                                place_all = filtered_dataset_allplaces)
  return(filtered_dataset_list)
}


shinyServer(function(input, output) {

  # env <- environment()
  
  # reactive elements
  filtered_dataset_sans_ids <- reactive({
    get_filtered_dataset_sans_ids(input, dataset)
  })
    
  filtered_dataset <- reactive({
    get_filtered_dataset(input, dataset)
  })
  
  query_ids <- reactive({
    idsource <- get_idsource_fullpath(input$idsource)
    format_query_ids(idsource)
  })

  # plots
  output$books_vs_pamphlets_plot <- renderPlot({
    plot_books_vs_pamphlets(filtered_dataset()$place_filtered)
  })
  
  output$title_count_top_10_authors_plot <- renderPlot({
    plot_titlecount_timeline_for_top10_authors(filtered_dataset()$place_filtered)
  })
  
  output$top_places_by_titlecount_plot <- renderPlot({
    top_places_by_titlecount_plot(filtered_dataset()$place_filtered, ntop)
  })

  output$top_places_titlecount_and_query_hits_plot <- renderPlot({
    titlecount_queryhits_relation_plot(filtered_dataset()$place_filtered,
                                       query_ids(),
                                       field = "publication_place")
  })

  output$top_publishers_abs_hits_plot <- renderPlot({
    top_publishers_abs_hits <- get_top_publishers_abs_hits(filtered_dataset()$place_filtered,
                                                           query_ids(),
                                                           field = "publisher",
                                                           ntop = ntop,
                                                           nchar = nchar)
    top_publishers_abs_hits_plot(top_publishers_abs_hits)
  })

  output$top_publishers_plot <- renderPlot({
    top_publishers_plot(filtered_dataset()$place_filtered,
                        ntop = ntop,
                        nchar = nchar)
  })

  output$titlecount_timeline_plot <- renderPlot({
    plot_titlecount_timeline(filtered_dataset()$place_filtered,
                            filtered_dataset()$place_all,
                            input$publication_place)
  })

  output$relative_titlecount_plot <- renderPlot({
    plot_relative_titlecount_timeline(filtered_dataset()$place_filtered,
                                      filtered_dataset()$place_all,
                                      filtered_dataset_sans_ids()$place_subsetted,
                                      filtered_dataset_sans_ids()$all_places,
                                      input$publication_place)
  })

  output$paper_consumption_plot <- renderPlot({
      plot_paper_consumption_timeline(filtered_dataset()$place_filtered,
                                      filtered_dataset()$place_all,
                                      input$publication_place)
  })

  output$relative_paper_consumption_plot <- renderPlot({
    plot_relative_paper_consumption_timeline(filtered_dataset()$place_filtered,
                                             filtered_dataset()$place_all,
                                             filtered_dataset_sans_ids()$place_subsetted,
                                             filtered_dataset_sans_ids()$all_places,
                                             input$publication_place,
                                             myfield = "paper")
  })
  
  output$top_authors_total_hits_plot <- renderPlot({
    hits_per_author_all <- get_hits_per_author(filtered_dataset()$place_filtered,
                                               query_ids(),
                                               field = "author",
                                               nchar = nchar)
    top_authors_by_hits <- get_top_authors(hits_per_author_all,
                                           ntop = ntop)
    plot_top_authors_total_hits(top_authors_by_hits)
  })
  
  output$top_authors_hits_per_edition_plot <- renderPlot({
    hits_per_author_all <- get_hits_per_author(filtered_dataset()$place_filtered,
                                               query_ids(),
                                               field = "author",
                                               nchar = nchar)
    top_authors_by_hits <- get_top_authors(hits_per_author_all,
                                           ntop = ntop)
    plot_top_authors_hits_per_edition(top_authors_by_hits)
  })
  
  output$top_authors_hits_per_edition_arranged_plot <- renderPlot({
    hits_per_author_all <- get_hits_per_author(filtered_dataset()$place_filtered,
                                               query_ids(),
                                               field = "author",
                                               nchar = nchar)
    top_authors_by_edition <- get_top_authors_by_edition(hits_per_author_all)
    plot_top_authors_hits_per_edition(top_authors_by_edition)
  })
  
  output$top_titles_by_title_count_plot <- renderPlot({
    plot_top_titles_by_title_count(filtered_dataset()$place_filtered,
                                   nchar = nchar)
  })

  output$top_titlehits_plot <- renderPlot({
    total_titlehits <- get_total_titlehits(filtered_dataset()$place_filtered,
                                           query_ids(),
                                           nchar = nchar)
    top10_titles <- get_top_n_titles(total_titlehits,
                                     ntop = ntop,
                                     custom.ids = query_ids())
    top_titlehits_plot(top10_titles)
  })
  
  output$top_titlehits_edition_plot <- renderPlot({
    total_titlehits <- get_total_titlehits(filtered_dataset()$place_filtered,
                                           query_ids(),
                                           nchar = nchar)
    top10_titles <- get_top_n_titles(total_titlehits,
                                     ntop = ntop,
                                     custom.ids = query_ids())
    top_titlehits_edition_plot(top10_titles)  
  })
  
  output$top_titlehits_edition_by_edition_plot <- renderPlot({
    total_titlehits <- get_total_titlehits(filtered_dataset()$place_filtered,
                                           query_ids(),
                                           nchar = nchar)
    top_titles_by_edition <- get_top_titles_by_edition(total_titlehits, ntop)
    top_titlehits_edition_plot(top_titles_by_edition)
  })
  
  # output$memory_usage <- renderText({
  #   all_things <- ""
  #   for (thing in ls(env)) {
  #     thing_name <- as.character(thing)
  #     thing_size <- as.character(format(object.size(get(thing)), units = 'auto'))
  #     thing_row <- paste(thing_name, thing_size, "\n")
  #     # print(thing_row)
  #     all_things <- paste(all_things, thing_row)
  #   }
  #   # cat(all_things)
  #   return(all_things)
  # })
})
