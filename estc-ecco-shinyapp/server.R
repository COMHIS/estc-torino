
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
library(RCurl)
library(jsonlite)
library(stringi)

nchar <- 40
ntop <- 20
time_window <- 10
dataset <- readRDS("../inst/examples/data/estc_df.Rds")
dataset <- augment_original_data(dataset, time_window)
theme_set(theme_bw(12))

rest_api_url <- "https://vm0175.kaj.pouta.csc.fi/ecco-search/"
terms_conf <- "&d=1&cp=1"
fields <- "&f=heading_index&f=heading_frontmatter&f=contents_index&f=heading_backmatter&f=heading_body&f=contents_frontmatter&f=contents_TOC&f=metadata_fullTitle&f=heading_TOC&f=contents_titlePage&f=contents_body"


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

get_idfiltered_dataset <- function(query_ids, dataset) {
  filtered_dataset <- subset(dataset$place_subsetted,
                             id %in% query_ids$id)
  filtered_dataset_allplaces <- subset(dataset$all_places,
                                       id %in% query_ids$id)
  filtered_dataset_list <- list(place_filtered = filtered_dataset,
                                place_all = filtered_dataset_allplaces)
  return(filtered_dataset_list)
  
}


shinyServer(function(input, output) {

  session_starttime <- Sys.time()
  session_randomhash <- stri_rand_strings(1, 8)
  
  # reactive elements
  
  sanity <- reactive({
    sanitized_term <- sanitize_term(input$search_term)
    sanity <- sanity_check_term_query(sanitized_term)
    return(sanity)
  })
  
  filtered_dataset_sans_ids <- reactive({
    if (sanity()) {
      get_filtered_dataset_sans_ids(input, dataset)
    }
  })

  query_ids <- reactive({
    # idsource <- get_idsource_fullpath(input$idsource)
    # format_query_ids(idsource)
    if (sanity()) {
      get_query_ids_from_api(input, rest_api_url, terms_conf, fields)
    }
  })
      
  filtered_dataset <- reactive({
    if (sanity()) {
      get_idfiltered_dataset(query_ids(), filtered_dataset_sans_ids())
    }
  })

  sanity_message <- observe({
    sanity <- sanity()
    if (!sanity) {
      showNotification("Please enter a valid search string. (Length > 4, no special characters.)",
                       duration = NULL,
                       id = "sanity", type = "error")
    } else {
      removeNotification(id = "sanity")
    }
  })
  
  save_search_term <- observe({
    sanity_state <- paste("sanity:", sanity())
    log_line <- paste(input$search_term,
                      session_randomhash,
                      session_starttime,
                      sanity_state,
                      sep = " --- ")
    log_file <- file("logs/search_log.txt", open = "at")
    log_line <- paste0(log_line, "\n")
    cat(log_line, file = log_file, append = TRUE)
  })

  # outputs
  # 
  # output$sanity_check <- renderText({
  #   if (!sanity()) {
  #     "Please enter a (somewhat) sane search term!"
  #   }
  # })
  
  output$books_vs_pamphlets_plot <- renderPlot({
    if (sanity()) {
      withProgress(message = 'Plotting books vs pamphlets...', value = 0.5, {
        plot_books_vs_pamphlets(filtered_dataset()$place_filtered)
      })
    }
  })
  
  output$title_count_top_10_authors_plot <- renderPlot({
    if (sanity()) {
      withProgress(message = 'Plotting top 10 authors...', value = 0.5, {
        plot_titlecount_timeline_for_top10_authors(filtered_dataset()$place_filtered)
      })
    }
  })
  
  output$top_places_by_titlecount_plot <- renderPlot({
    if (sanity()) {
      withProgress(message = 'Plotting titlecount...', value = 0.5, {
        top_places_by_titlecount_plot(filtered_dataset()$place_filtered, ntop)
      })
    }
  })

  output$top_places_titlecount_and_query_hits_plot <- renderPlot({
    if (sanity()) {
      withProgress(message = 'Plotting titles to hits...', value = 0.5, {
          titlecount_queryhits_relation_plot(filtered_dataset()$place_filtered,
                                       query_ids(),
                                       field = "publication_place")
      })
    }
  })

  output$top_publishers_abs_hits_plot <- renderPlot({
    if (sanity()) {
      withProgress(message = 'Plotting top publishers absolute...', value = 0.5, {
        top_publishers_abs_hits <- get_top_publishers_abs_hits(filtered_dataset()$place_filtered,
                                                               query_ids(),
                                                               field = "publisher",
                                                               ntop = ntop,
                                                               nchar = nchar)
        incProgress(amount = 0.3, detail = "making plot...")
        top_publishers_abs_hits_plot(top_publishers_abs_hits)
      })
    }
  })

  output$top_publishers_plot <- renderPlot({
    if (sanity()) {
      withProgress(message = 'Plotting top publishers relative...', value = 0.5, {
        top_publishers_plot(filtered_dataset()$place_filtered,
                            ntop = ntop,
                            nchar = nchar)
      })
    }
  })

  output$titlecount_timeline_plot <- renderPlot({
    if (sanity()) {
      withProgress(message = 'Plotting titles timeline...', value = 0.5, {
        plot_titlecount_timeline(filtered_dataset()$place_filtered,
                                 filtered_dataset()$place_all,
                                 input$publication_place)
      })
    }
  })
  
  output$relative_titlecount_plot <- renderPlot({
    if (sanity()) {
      withProgress(message = 'Plotting titles relative...', value = 0.5, {
        plot_relative_titlecount_timeline(filtered_dataset()$place_filtered,
                                          filtered_dataset()$place_all,
                                          filtered_dataset_sans_ids()$place_subsetted,
                                          filtered_dataset_sans_ids()$all_places,
                                          input$publication_place)
      })
    }
  })
  
  output$paper_consumption_plot <- renderPlot({
    if (sanity()) {  
      withProgress(message = 'Plotting paper consumption...', value = 0.5, {
        plot_paper_consumption_timeline(filtered_dataset()$place_filtered,
                                        filtered_dataset()$place_all,
                                        input$publication_place)
      })
    }
  })
  
  output$relative_paper_consumption_plot <- renderPlot({
    if (sanity()) {
      withProgress(message = 'Plotting relative paper...', value = 0.5, {
        plot_relative_paper_consumption_timeline(filtered_dataset()$place_filtered,
                                                 filtered_dataset()$place_all,
                                                 filtered_dataset_sans_ids()$place_subsetted,
                                                 filtered_dataset_sans_ids()$all_places,
                                                 input$publication_place,
                                                 myfield = "paper")
      })
    }
  })
  
  output$top_authors_total_hits_plot <- renderPlot({
    if (sanity()) {
      withProgress(message = 'Plotting hits per author...', value = 0.5, {
        hits_per_author_all <- get_hits_per_author(filtered_dataset()$place_filtered,
                                                   query_ids(),
                                                   field = "author",
                                                   nchar = nchar)
        incProgress(amount = 0.2, detail = "calculating...")
        top_authors_by_hits <- get_top_authors(hits_per_author_all,
                                               ntop = ntop)
        incProgress(amount = 0.2, detail = "making plot...")
        plot_top_authors_total_hits(top_authors_by_hits)
      })
    }
  })
  
  output$top_authors_hits_per_edition_plot <- renderPlot({
    if (sanity()) {
      withProgress(message = 'Plotting hits per edition...', value = 0.5, {
        hits_per_author_all <- get_hits_per_author(filtered_dataset()$place_filtered,
                                                   query_ids(),
                                                   field = "author",
                                                   nchar = nchar)
        incProgress(amount = 0.2, detail = "calculating...")
        top_authors_by_hits <- get_top_authors(hits_per_author_all,
                                               ntop = ntop)
        incProgress(amount = 0.2, detail = "making plot...")
        plot_top_authors_hits_per_edition(top_authors_by_hits)
      })
    }
  })
  
  output$top_authors_hits_per_edition_arranged_plot <- renderPlot({
    if (sanity()) {
      withProgress(message = 'Plotting more hits per edition...', value = 0.5, {
        hits_per_author_all <- get_hits_per_author(filtered_dataset()$place_filtered,
                                                   query_ids(),
                                                   field = "author",
                                                   nchar = nchar)
        incProgress(amount = 0.2, detail = "calculating...")
        top_authors_by_edition <- get_top_authors_by_edition(hits_per_author_all)
        incProgress(amount = 0.2, detail = "making plot...")
        plot_top_authors_hits_per_edition(top_authors_by_edition)
      })
    }
  })
  
  output$top_titles_by_title_count_plot <- renderPlot({
    if (sanity()) {
      withProgress(message = 'Plotting top title by titlecount...', value = 0.5, {
        plot_top_titles_by_title_count(filtered_dataset()$place_filtered,
                                       nchar = nchar)
      })
    }
  })

  output$top_titlehits_plot <- renderPlot({
    if (sanity()) {
      withProgress(message = 'Plotting top titles...', value = 0.5, {
        total_titlehits <- get_total_titlehits(filtered_dataset()$place_filtered,
                                               query_ids(),
                                               nchar = nchar)
        incProgress(amount = 0.2, detail = "calculating...")
        top10_titles <- get_top_n_titles(total_titlehits,
                                         ntop = ntop,
                                         custom.ids = query_ids())
        incProgress(amount = 0.2, detail = "making plot...")
        top_titlehits_plot(top10_titles)
      })
    }
  })
  
  output$top_titlehits_edition_plot <- renderPlot({
    if (sanity()) {
      withProgress(message = 'Plotting top titles by edition...', value = 0.5, {
        total_titlehits <- get_total_titlehits(filtered_dataset()$place_filtered,
                                               query_ids(),
                                               nchar = nchar)
        incProgress(amount = 0.2, detail = "calculating...")
        top10_titles <- get_top_n_titles(total_titlehits,
                                         ntop = ntop,
                                         custom.ids = query_ids())
        incProgress(amount = 0.2, detail = "making plot...")
        top_titlehits_edition_plot(top10_titles)
      })
    }
  })
  
  output$top_titlehits_edition_by_edition_plot <- renderPlot({
    if (sanity()) {
      withProgress(message = 'Plotting top titles by edition...', value = 0.5, {
        total_titlehits <- get_total_titlehits(filtered_dataset()$place_filtered,
                                               query_ids(),
                                               nchar = nchar)
        incProgress(amount = 0.2, detail = "calculating...")
        top_titles_by_edition <- get_top_titles_by_edition(total_titlehits, ntop)
        incProgress(amount = 0.2, detail = "making plot...")
        top_titlehits_edition_plot(top_titles_by_edition)
      })
    }
  })
})
