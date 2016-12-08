

resultsdata_add_query_hits <- function(basedata, query_hits_per_id) {
  returndata <- basedata
  returndata$paragraphs_hit <-
    query_hits_per_id[match(returndata$id,
                            query_hits_per_id$id), 2]
  return(returndata)
}


resultsdata_add_ecco_pages <- function(basedata, query_results_df) {
  returndata <- basedata
  returndata$ecco_pages <-
    query_results_df[match(returndata$id,
                           query_results_df$id), 2]
  returndata$ecco_pages <- as.integer(as.character(returndata$ecco_pages)) # this is stupid hack
  return(returndata)
}


resultsdata_add_hits_per_page <- function(resultsdata, pages_as_integers = FALSE, ecco_pages = FALSE) {
  resultsdata$parahits_per_page <- resultsdata$paragraphs_hit / resultsdata$pagecount
  resultsdata$parahits_per_ecco_page <- resultsdata$paragraphs_hit / resultsdata$ecco_pages
  if (pages_as_integers) {
    resultsdata$pagecount <- ceiling(resultsdata$pagecount)
  }
  resultsdata <- subset(resultsdata, paragraphs_hit > 0)
  return(resultsdata)
}

