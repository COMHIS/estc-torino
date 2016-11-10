

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

sanitize_term <- function(term){
  term <- as.character(term)
  term <- tolower(term)
  term <- gsub("[^a-zA-Z0-9 ]", "", term)
  term <- trimws(term)
  return(term)  
}

sanity_check_term_query <- function(term){
  if (nchar(term) < 5) {
    return(FALSE)
  }
  return(TRUE)
}

get_rest_api_terms <- function(rest_api_url, term,
                               terms_conf = "&d=1&cp=1",
                               fields){
  terms_url <- paste0(rest_api_url, "terms")
  term <- sanitize_term(term)
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

validate_json <- function(jsondata) {
  conversion_results <- fromJSON(jsondata)
  if (length(conversion_results > 0)) {
    return(TRUE)
  }
  return(FALSE)
}

get_rest_query_results <- function(search_term,
                                   rest_api_url,
                                   terms_conf = "&d=1&cp=1",
                                   fields) {
  terms_json <- get_rest_api_terms(rest_api_url, search_term, terms_conf, fields)
  if (validate_json(terms_json) == FALSE) {
    return(NULL) 
  } else {
    terms_df <- termset_json_to_dataframe(terms_json)
    terms_top50 <- get_query_terms(terms_df)
    query_results <- get_search_results(rest_api_url, terms_top50, fields)
    return(query_results)
  }
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

get_query_ids_from_api <- function(input, rest_api_url, terms_conf, fields) {
  search_term <- tolower(as.character(input$search_term))
  query_results <- get_rest_query_results(search_term,
                                          rest_api_url,
                                          terms_conf,
                                          fields)
  if (is.null(query_results)) {
    return(NULL)
  }
  enriched_query_results <- enrich_rest_query_results(query_results)
  return(enriched_query_results)
}


