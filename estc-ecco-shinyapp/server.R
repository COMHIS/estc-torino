
library(shiny)
library(devtools)
load_all("../R/bibliographica")
library(bibliographica)
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
library('RCurl')
library('jsonlite')

nchar <- 40
ntop <- 20
time_window <- 10
dataset <- readRDS("../inst/examples/data/estc_df.Rds")
dataset <- augment_original_data(dataset, time_window)
theme_set(theme_bw(12))

rest_api_url <- "https://vm0175.kaj.pouta.csc.fi/ecco-search/"
# terms_url <- "https://vm0175.kaj.pouta.csc.fi/ecco-search/terms"
terms_conf <- "&d=1&cp=1"
# results_url <- "https://vm0175.kaj.pouta.csc.fi/ecco-search/search"
fields <- "&f=heading_index&f=heading_frontmatter&f=contents_index&f=heading_backmatter&f=heading_body&f=contents_frontmatter&f=contents_TOC&f=metadata_fullTitle&f=heading_TOC&f=contents_titlePage&f=contents_body"


termset_json_to_dataframe <- function(termset_json) {
  list_data <- fromJSON(termset_json)
  col1 <- names(list_data)
  col2 <- unlist(list_data)
  resulting_dataframe <- data.frame(term = col1, count = col2)
  resulting_dataframe <- resulting_dataframe[order(resulting_dataframe$count,
                                                   decreasing = TRUE),]
  return(resulting_dataframe)
}

get_search_results <- function(rest_api_url, query_terms, fields) {
  results_url <- paste0(rest_api_url, "search")
  rest_request <- paste0(results_url, "?q=",
                         query_terms,
                         fields,
                         "&rf=metadata_ESTCID&mf=1")
  search_results <- read.csv(rest_request, header = TRUE)
  return(search_results)
}

get_rest_api_terms <- function(rest_api_url, term,
                               terms_conf = "&d=1&cp=1",
                               fields){
  terms_url <- paste0(rest_api_url, "terms")
  formatted_term <- gsub(" ", "%20", term)
  formatted_term <- paste0("%22", formatted_term, "%22")
  query_url <- paste0(terms_url, "?q=", formatted_term, terms_conf, fields)
  terms_result <- getURL(query_url)
  return(terms_result)
}

get_query_terms <- function(terms_dataframe) {
  top50terms <- head(terms_dataframe, 50)
  top50terms_list <- as.character(top50terms[, 1])
  top50merged <- paste0("%22", top50terms_list, "%22", collapse = "%20")
  top50merged <- gsub(" ", "%20", top50merged)
  return(top50merged)
}

get_rest_query_results <- function(search_term,
                                   rest_api_url,
                                   terms_conf = "&d=1&cp=1",
                                   fields) {
  terms_json <- get_rest_api_terms(rest_api_url, search_term, terms_conf, fields)
  terms_df <- termset_json_to_dataframe(terms_json)
  terms_top50 <- get_query_terms(terms_df)
  query_results <- get_search_results(rest_api_url, terms_top50, fields)
  return(query_results)
}

enrich_rest_query_results <- function(rest_query_results) {
  names(rest_query_results) <- c("id", "freq", "length")
  rest_query_results$id <- gsub("\\,$", "", as.character(rest_query_results$id))
  rest_query_results$freq <- as.numeric(as.character(rest_query_results$freq))
  rest_query_results$length <- as.numeric(as.character(rest_query_results$length))
  rest_query_results$id <- apply(cbind(substr(rest_query_results$id, 1, 1),
                                       gsub("^[A-Z]0*", "", rest_query_results$id)),
                                 1, function(x) {paste(x, collapse = "")})
  formatted_ids <- rest_query_results
  return(formatted_ids)
}

get_idsource_fullpath <- function(idsource) {
  idsource_fullpath <- paste0("../inst/examples/data/", idsource)
  return(idsource_fullpath)
}

get_query_ids_from_api <- function(input, rest_api_url, terms_conf, fields) {
  search_term <- tolower(as.character(input$search_term))
  query_results <- get_rest_query_results(search_term,
                                          rest_api_url,
                                          terms_conf,
                                          fields)
  enriched_query_results <- enrich_rest_query_results(query_results)
  return(enriched_query_results)
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

get_filtered_dataset_api <- function(input, dataset) {
  query_ids <- get_query_ids_from_api(input, rest_api_url, terms_conf, fields)
  data_subset <- get_filtered_dataset_sans_ids(input, dataset)
  filtered_dataset <- subset(data_subset$place_subsetted,
                             id %in% query_ids$id)
  filtered_dataset_allplaces <- subset(data_subset$all_places,
                                       id %in% query_ids$id)
  filtered_dataset_list <- list(place_filtered = filtered_dataset,
                                place_all = filtered_dataset_allplaces)
  return(filtered_dataset_list)
}


shinyServer(function(input, output) {

  # reactive elements
  filtered_dataset_sans_ids <- reactive({
    get_filtered_dataset_sans_ids(input, dataset)
  })
    
  filtered_dataset <- reactive({
    get_filtered_dataset_api(input, dataset)
  })
  
  query_ids <- reactive({
    # idsource <- get_idsource_fullpath(input$idsource)
    # format_query_ids(idsource)
    get_query_ids_from_api(input, rest_api_url, terms_conf, fields)  
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
