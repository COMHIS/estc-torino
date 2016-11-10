

get_ids_not_found <- function(original_query_ids, query_hits_ids) {
  not_found <- setdiff(original_query_ids$id, query_hits_ids$id)
  return(not_found)
}


combine_duplicated_query_ids <- function(query_ids) {
  # Combine duplicated customIDs
  query_ids <- query_ids %>% group_by(id) %>%
    summarise(freq = sum(freq, na.rm = T),
              length = sum(length, na.rm = T))
  return(query_ids)
}


add_normalization_field <- function(query_ids) {
  # Add normalization field: hits per 1000 words
  query_ids$freq.normalized <- 1e3 * query_ids$freq / query_ids$length
  return (query_ids)
}


get_query_hits_amount <- function(query_ids, data) {
  # Number and percentage of the custom list IDs that were found
  # in the very original data
  hitn0 <- sum(query_ids$id %in% data$id, na.rm = TRUE)
  hitp0 <- 100 * hitn0 / nrow(query_ids)
  query_hits_amount <- list(hits_amount = hitn0, percentile = hitp0)
  return(query_hits_amount)
}


get_sum_duplicate_ids <- function(ids) {
  # Duplicated entries
  sum_duplicates <- sum(duplicated(ids), na.rm = T)
  return(sum_duplicates)
}


get_query_summary_string <- function(query_ids, data){
  query_hits_in_data <- get_query_hits_amount(query_ids,
                                                       data)
  output_query_hits_in_data <- 
    paste0(query_hits_in_data$hits_amount, " (",
           round(query_hits_in_data$percentile, 2), " %)")
  return(output_query_hits_in_data)
}

