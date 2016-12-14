
library(shiny)
library(devtools)
load_all("../bibliographica")
load_all("../estc")
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
load_all()
source("./get_ids_with_filter.R")

nchar <- 40
ntop <- 20
dataset_from_rds <- readRDS("../data/estc_df.Rds")
theme_set(theme_bw(12))

rest_api_url <- "https://vm0175.kaj.pouta.csc.fi/ecco-search/"
terms_conf <- "&d=1&cp=1"
# fields <- "&f=heading_index&f=heading_frontmatter&f=contents_index&f=heading_backmatter&f=heading_body&f=contents_frontmatter&f=contents_TOC&f=metadata_fullTitle&f=heading_TOC&f=contents_titlePage&f=contents_body"

get_idsource_fullpath <- function(idsource) {
  idsource_fullpath <- paste0("../data/", idsource)
  return(idsource_fullpath)
}


shinyServer(function(input, output) {

  # session_starttime <- Sys.time() # not used currently
  session_randomhash <- stri_rand_strings(1, 8)

  # reactive elements

  time_window_numeric <- reactive({
    as.numeric(input$time_window)
  })

  get_dataset_augmented <- eventReactive(input$submit_button, {
    dataset_augmented <- augment_original_data(dataset_from_rds,
                                               time_window_numeric())
    if (input$subcorpus == "") {
      return(dataset_augmented)
    } else {
      dataset_augmented <- get_data_subset_ids_csv(dataset_augmented, input$subcorpus)
      return(dataset_augmented)
    }
    
  })

  sanity <- reactive({
    sanitized_term <- sanitize_term(input$search_term)
    sanity <- sanity_check_term_query(sanitized_term)
    return(sanity)
  })
  
  filtered_dataset_sans_ids <- reactive({
    if (sanity() & query_state()) {
      filtered_dataset_sans_ids <- get_filtered_dataset_sans_ids(input, get_dataset_augmented())
      return(filtered_dataset_sans_ids)
    }
  })

  query_ids <- eventReactive(input$submit_button, {
    if (sanity()) {
      removeNotification(id = "query_null")
      withProgress(message = 'Querying API...', value = 0.5, {
        selected_fields <- get_api_fields_from_input(input$search_fields)
        min_freq <- input$api_min_hits
        get_query_ids_from_api(input, rest_api_url, terms_conf, selected_fields, min_freq)
      })
    }
  })
  
  # !refractor not pretty
  query_state <- reactive({
    if (is.null(query_ids()) & sanity()){
      return(FALSE)
    } else {
      return(TRUE)
    }
  })
      
  filtered_dataset <- reactive({
    if (sanity() & query_state()) {
      get_idfiltered_dataset(query_ids(), filtered_dataset_sans_ids())
    }
  })

  sanity_message <- observe({
    if (!sanity()) {
      showNotification("Please enter a valid search string. (Length > 4, no special characters.)",
                       duration = NULL,
                       id = "sanity",
                       type = "error")
    } else {
      removeNotification(id = "sanity")
    }
  })
  
  query_null_message <- observe({
    if (!query_state()) {
      showNotification("Search returned no results.",
                       duration = NULL,
                       id = "query_null",
                       type = "error")
    } else {
      removeNotification(id = "query_null")
    }
  })
  
  log_search_term <- observe({
    if (!(input$search_term == "")) {
      sanity_line <- paste("sanity:", sanity())
      query_line <- paste("results:", query_state())
      log_line <- paste(input$search_term,
                        session_randomhash,
                        # session_starttime,
                        Sys.time(),
                        sanity_line,
                        query_line,
                        sep = " --- ")
      log_line <- paste0(log_line, "\n")
      log_file_path <- paste0("logs/", "search_log-", Sys.Date(), ".txt")
      log_file <- file(log_file_path, open = "at")
      cat(log_line, file = log_file, append = TRUE)
      close(log_file)
    }
  })
  
  observeEvent(input$save_button, {
    # print("Save button clicked!")
    ids_label <- input$ids_identifier
    query_res_df <- get_idfiltered_dataset(query_ids(), filtered_dataset_sans_ids())$place_filtered
    query_res_ids <- query_res_df$id
    query_res_ids_df <- data.frame(id = query_res_ids)
    filename <- paste0("../data/saved_query_ids/", ids_label, ".csv")
    write.csv(query_res_ids_df, file = filename)
    showNotification(paste0("Query saved with identifier: ", ids_label), duration = 10, type = "message")
  })

  # summary tab outputs

  output$intro_text <- renderText({
    if (!sanity()) {
      "Some informative and helpful text on how to use the app.
      It will disappear with a valid search string."
    }    
  })

  # !refractor this into a cleaner function
  output$summary_table <- renderTable({
    if (sanity() & query_state()) {
      withProgress(message = 'Creating summary...', value = 0.5, {
        selected_years <- paste(input$range_years[1], input$range_years[2], sep = " - ")
        api_query_hits <- nrow(query_ids())
        output_query_hits_in_data_original <- 
          get_query_summary_string(query_ids(), get_dataset_augmented())
        output_query_hits_in_data_subset <- 
          get_query_summary_string(query_ids(), filtered_dataset()$place_filtered)
        
        input_values <- list("Search string" = input$search_term,
                             "Years" = selected_years,
                             "Publication place" = input$publication_place,
                             "Language" = input$language,
                             "Time segment" = input$time_window,
                             "Document type" = input$document_type,
                             "ECCO API query hits" = api_query_hits,
                             "Unique hit IDs in ESTC or subcorpus" =
                               output_query_hits_in_data_original,
                             "Hits_final" =
                               output_query_hits_in_data_subset
        )
        summary_table <- t(data.frame(input_values))
        # fix dots etc in rownames that resulting from conversion into dataframe:
        rownames(summary_table) <- gsub("\\."," ",rownames(summary_table))
        rownames(summary_table)[rownames(summary_table) == "Hits_final"] <- 
          "Unique hit IDs after filtering for year, place, etc."
        return(summary_table)
      })
    }
  }, rownames = TRUE, colnames = FALSE)
  
  # plot outputs
  
  output$books_vs_pamphlets_plot <- renderPlot({
    if (sanity() & query_state()) {
      withProgress(message = 'Plotting books vs pamphlets...', value = 0.5, {
        plot_books_vs_pamphlets(filtered_dataset()$place_filtered)
      })
    }
  })
  
  output$title_count_top_10_authors_plot <- renderPlot({
    if (sanity() & query_state()) {
      withProgress(message = 'Plotting top 10 authors...', value = 0.5, {
        plot_titlecount_timeline_for_top10_authors(filtered_dataset()$place_filtered)
      })
    }
  })
  
  output$top_places_by_titlecount_plot <- renderPlot({
    if (sanity() & query_state()) {
      withProgress(message = 'Plotting titlecount...', value = 0.5, {
        top_places_by_titlecount_plot(filtered_dataset()$place_filtered, ntop)
      })
    }
  })

  output$top_places_titlecount_and_query_hits_plot <- renderPlot({
    if (sanity() & query_state()) {
      withProgress(message = 'Plotting titles to hits...', value = 0.5, {
          titlecount_queryhits_relation_plot(filtered_dataset()$place_filtered,
                                       query_ids(),
                                       field = "publication_place")
      })
    }
  })

  output$top_publishers_abs_hits_plot <- renderPlot({
    if (sanity() & query_state()) {
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
    if (sanity() & query_state()) {
      withProgress(message = 'Plotting top publishers relative...', value = 0.5, {
        top_publishers_plot(filtered_dataset()$place_filtered,
                            ntop = ntop,
                            nchar = nchar)
      })
    }
  })

  output$titlecount_timeline_plot <- renderPlot({
    if (sanity() & query_state()) {
      withProgress(message = 'Plotting titles timeline...', value = 0.5, {
        plot_titlecount_timeline(filtered_dataset()$place_filtered,
                                 filtered_dataset()$place_all,
                                 input$publication_place,
                                 time_window = time_window_numeric())
      })
    }
  })
  
  output$relative_titlecount_plot <- renderPlot({
    if (sanity() & query_state()) {
      withProgress(message = 'Plotting titles relative...', value = 0.5, {
        plot_relative_titlecount_timeline(filtered_dataset()$place_filtered,
                                          filtered_dataset()$place_all,
                                          filtered_dataset_sans_ids()$place_subsetted,
                                          filtered_dataset_sans_ids()$all_places,
                                          input$publication_place,
                                          time_window = time_window_numeric())
      })
    }
  })
  
  output$paper_consumption_plot <- renderPlot({
    if (sanity() & query_state()) {  
      withProgress(message = 'Plotting paper consumption...', value = 0.5, {
        plot_paper_consumption_timeline(filtered_dataset()$place_filtered,
                                        filtered_dataset()$place_all,
                                        input$publication_place,
                                        time_window = time_window_numeric())
      })
    }
  })
  
  output$relative_paper_consumption_plot <- renderPlot({
    if (sanity() & query_state()) {
      withProgress(message = 'Plotting relative paper...', value = 0.5, {
        plot_relative_paper_consumption_timeline(filtered_dataset()$place_filtered,
                                                 filtered_dataset()$place_all,
                                                 filtered_dataset_sans_ids()$place_subsetted,
                                                 filtered_dataset_sans_ids()$all_places,
                                                 input$publication_place,
                                                 myfield = "paper",
                                                 time_window = time_window_numeric())
      })
    }
  })
  
  output$top_authors_total_hits_plot <- renderPlot({
    if (sanity() & query_state()) {
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
    if (sanity() & query_state()) {
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
    if (sanity() & query_state()) {
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
    if (sanity() & query_state()) {
      withProgress(message = 'Plotting top title by titlecount...', value = 0.5, {
        plot_top_titles_by_title_count(filtered_dataset()$place_filtered,
                                       nchar = nchar)
      })
    }
  })

  output$top_titlehits_plot <- renderPlot({
    if (sanity() & query_state()) {
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
    if (sanity() & query_state()) {
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
    if (sanity() & query_state()) {
      withProgress(message = 'Plotting some nefarious plots...', value = 0.5, {
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
