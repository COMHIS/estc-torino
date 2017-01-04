

add_publication_year <- function(processed_jsearch_query_results, 
                                 estc_data) {
  title_year_df <- processed_jsearch_query_results
  title_year_df$year <- estc_data[match(title_year_df$id, estc_data$id),
                                  "publication_year"]
  return(title_year_df)
}


summarize_hits_per_year <- function(title_year_df, years = list(1705, 1799)) {
  yearly_hits_summary <- aggregate(title_year_df$freq,
                                  by = list(title_year_df$year),
                                  FUN = sum)
  years_range <- years[[1]]:years[[2]]
  years_df <- data.frame(year = years_range, hits = rep(0, length(years_range)))
  names(yearly_hits_summary) <- c("year", "hits")
  years_df$hits <- yearly_hits_summary[match(years_df$year, 
                                             yearly_hits_summary$year),
                                       "hits"]
  return(years_df)
}


get_hits_relative_frequency_yearly <- function(hits_subset_yearly, hits_all_yearly) {
  averages_yearly <- hits_subset_yearly
  averages_yearly["total_hits"] <-
    hits_all_yearly[match(averages_yearly$year, hits_all_yearly$year), "hits"]
  averages_yearly["frequency"] <-
    averages_yearly["hits"] / averages_yearly["total_hits"]
  averages_yearly <- averages_yearly[c("year", "frequency")]
  return(averages_yearly)
}


get_hits_yearly_for_api_query <- function(api_query, dataset) {
  query_results_df <- get_api2_jsearch_query_results_df(api_query)
  query_counts <- get_api2_query_counts(query_results_df)
  query_counts <- add_publication_year(query_counts, dataset)
  query_hits_yearly <- summarize_hits_per_year(query_counts)
  return(query_hits_yearly)
}


get_total_ecco_titles_yearly <- function(dataset, years = list(1705, 1799)) {
  year_list <- years[[1]]:years[[2]]
  subset_ids <- readRDS(paste0("../data/", "ecco_dump_ids", ".Rds"))
  data_subset <- dataset[dataset$id %in% subset_ids$id, ]
  yearly_titles <- plyr::count(data_subset, 'publication_year')
  yearly_title_results <- data.frame(year = year_list)
  yearly_title_results$titles <-
    yearly_titles[match(yearly_title_results$year, yearly_titles$publication_year), "freq"]
  names(yearly_title_results) <- c("year", "hits")
  return(yearly_title_results)
}


get_yearly_paragraph_frequencies_list <- function(paragraph_query_set, dataset) {
  
  base_set <- paragraph_query_set$base_query_set
  print("querying api for base term")
  if (base_set$term == "") {
    base_query_hits_yearly <- readRDS("./ecco_titles_yearly.Rds")
  } else {
    base_query_hits_yearly <- get_hits_yearly_for_api_query(base_set$query, dataset)
  }
  
  comparable_sets <- paragraph_query_set$comparable_query_sets
  comparable_results <- vector("list", length(comparable_sets))
    
  i <- 1
  for (query_set in comparable_sets) {
    print(paste0("querying api for comparable term: ", query_set$term))
    comparable_hits_yearly <-
      get_hits_yearly_for_api_query(query_set$query, dataset)
    comparable_yearly_relative_freq <-
      get_hits_relative_frequency_yearly(comparable_hits_yearly, 
                                         base_query_hits_yearly)
    comparable_result_set <- list(term = query_set$term,
                                  data = comparable_yearly_relative_freq)
    comparable_results[[i]] <- comparable_result_set
    i <- i + 1
  }
  return(comparable_results)
}

