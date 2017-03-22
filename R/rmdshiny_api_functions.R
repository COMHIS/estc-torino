library(RCurl)
library(jsonlite)

get_api_fields_from_input <- function(selected_fields) {
  switch(selected_fields,
         "contents_headings_all" = {
           return("&f=heading_index&f=heading_frontmatter&f=heading_backmatter&f=heading_body&f=heading_TOC&f=contents_index&f=contents_frontmatter&f=contents_TOC&f=contents_titlePage&f=contents_body&f=contents_backmatter")
         },
         "contents_titlepage" = {
           return("&f=contents_titlePage")
         },
         "headings_all" = {
           return("&f=heading_index&f=heading_frontmatter&f=heading_backmatter&f=heading_body&f=heading_TOC")
         },
         "contents_headings_frontmatter" = {
           return("&f=heading_frontmatter&f=contents_frontmatter")
         },
         "contents_headings_backmatter" = {
           return("&f=heading_backmatter&f=contents_backmatter")
         },
         return("&f=heading_index&f=heading_frontmatter&f=heading_backmatter&f=heading_body&f=heading_TOC&f=contents_index&f=contents_frontmatter&f=contents_TOC&f=contents_titlePage&f=contents_body&f=contents_backmatter")
  )
}


encode_url_request <- function(request) {
  request <- gsub(": \\+", ":", request)
  request <- gsub(" ", "%20", request)
  request <- gsub("\\|", "%7C", request)
  request <- gsub("\\+", "%2B", request)
  request <- gsub("'", "%27", request)
  request <- gsub(":", "%3A", request)
  request <- gsub("<", "%3C", request)
  request <- gsub(">", "%3E", request)
  return(request)
}


termset_json_to_dataframe <- function(termset_json) {
  list_data <- fromJSON(termset_json)
  col1 <- names(list_data)
  col2 <- unlist(list_data)
  resulting_dataframe <- data.frame(term = col1, count = col2)
  resulting_dataframe <- resulting_dataframe[order(resulting_dataframe$count,
                                                   decreasing = TRUE),]
  return(resulting_dataframe)
}


get_search_results <- function(rest_api_url, query_terms, fields, min_freq = "&mf=1") {
  results_url <- paste0(rest_api_url, "search")
  rest_request <- paste0(results_url, "?q=",
                         query_terms,
                         fields,
                         "&rf=metadata_ESTCID",
                         min_freq)
  search_results <- read.csv(rest_request, header = TRUE)
  return(search_results)
}

get_api_query_search_results <- function(api_query) {
  search_results <- read.csv(api_query, header = TRUE)
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


get_query_terms <- function(terms_dataframe) {
  top50terms <- head(terms_dataframe, 50)
  top50terms_list <- as.character(top50terms[, 1])
  top50merged <- paste0("%22", top50terms_list, "%22", collapse = "%20")
  top50merged <- gsub(" ", "%20", top50merged)
  top50merged <- gsub("[^a-z0-9%']", "", top50merged)
  return(top50merged)
}


validate_json <- function(jsondata) {
  conversion_results <- fromJSON(jsondata)
  if (length(conversion_results) > 0) {
    return(TRUE)
  }
  return(FALSE)
}

get_rest_query_results <- function(search_term,
                                   rest_api_url,
                                   terms_conf = "&d=1&cp=1",
                                   fields,
                                   min_freq = "&mf=1") {
  terms_json <- get_rest_api_terms(rest_api_url, search_term, terms_conf, fields)
  if (validate_json(terms_json) == FALSE) {
    return(NULL) 
  } else {
    terms_df <- termset_json_to_dataframe(terms_json)
    terms_top50 <- get_query_terms(terms_df)
    query_results <- get_search_results(rest_api_url, terms_top50, fields, min_freq)
    return(query_results)
  }
}


api3_enrich_rest_query_results <- function(rest_query_results) {
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


get_query_ids_from_api <- function(input, rest_api_url, terms_conf, fields, min_freq = 1) {
  search_term <- tolower(as.character(input$search_term))
  mf <- paste0("&mf=", min_freq)
  query_results <- get_rest_query_results(search_term,
                                          rest_api_url,
                                          terms_conf,
                                          fields,
                                          mf)
  if (is.null(query_results)) {
    return(NULL)
  }
  enriched_query_results <- enrich_rest_query_results(query_results)
  return(enriched_query_results)
}


api3_get_fields_from_input <- function(selected_fields) {
  switch(selected_fields,
         "contents_headings_all" = {
           return("<document|")
         },
         "contents_titlepage" = {
           return("<documentPart|+documentPartType:titlePage")
         },
         "headings_all" = {
           return("<section|+heading:")
         },
         "contents_headings_frontmatter" = {
           return("<documentPart|+documentPartType:frontmatter")
         },
         "contents_headings_backmatter" = {
           return("<documentPart|+documentPartType:backmatter")
         },
         return("<document|")
  )
}


api3_get_rest_api_terms <- function(rest_api_url = "http://vm0542.kaj.pouta.csc.fi/",
                                    term,
                                    terms_conf = "&minCommonPrefix=1&maxEditDistance=1"){
  terms_url <- paste0(rest_api_url, "terms")
  term <- sanitize_term(term)
  formatted_term <- gsub(" ", "%20", term)
  formatted_term <- paste0("%22", formatted_term, "%22")
  query_url <- paste0(terms_url, "?term=", formatted_term, terms_conf)
  terms_result <- getURL(query_url)
  return(terms_result)
}


api3_get_search_results <- function(rest_api_url = "http://vm0542.kaj.pouta.csc.fi/",
                                    query_terms,
                                    fields,
                                    min_score = "&minScore=1") {
  results_url <- paste0(rest_api_url, "search")
  rest_request_start <- paste0(results_url, "?query=")
  rest_request_end <- paste0(fields,
                             " +",
                             "%28",
                             query_terms,
                             # "public",
                             "%29",
                             "|document>",
                             "&field=ESTCID",
                             min_score,
                             "&limit=-1")
  rest_request_end <- encode_url_request(rest_request_end)
  rest_request <- paste0(rest_request_start, rest_request_end)
  request_results <- getURL(rest_request)
  results <- fromJSON(request_results)$results$docs
  return(results)
}


api3_enrich_rest_query_results <- function(rest_query_results) {
  names(rest_query_results) <- c("id", "freq")
  
  rest_query_results$id <- gsub("\\,$", "", as.character(rest_query_results$id))
  rest_query_results$freq <- as.numeric(as.character(rest_query_results$freq))
  # rest_query_results$length <- as.numeric(as.character(rest_query_results$length))
  rest_query_results$id <- apply(cbind(substr(rest_query_results$id, 1, 1),
                                       gsub("^[A-Z]0*", "", rest_query_results$id)),
                                 1, function(x) {paste(x, collapse = "")})
  formatted_ids <- rest_query_results
  return(formatted_ids)
}



api3_get_rest_query_results <- function(search_term,
                                        rest_api_url = "http://vm0542.kaj.pouta.csc.fi/",
                                        terms_conf = "&minCommonPrefix=1&maxEditDistance=1",
                                        fields,
                                        min_freq = "&minScore=1") {
  terms_json <- api3_get_rest_api_terms(rest_api_url, search_term, terms_conf)
  if (validate_json(terms_json) == FALSE) {
    return(NULL) 
  } else {
    terms_df <- fromJSON(terms_json)$results
    terms_top50 <- get_query_terms(terms_df)
    query_results <- api3_get_search_results(rest_api_url,
                                             query_terms = terms_top50,
                                             fields,
                                             min_score = min_freq)
    return(query_results)
  }
}


api3_get_query_ids <- function(input,
                               rest_api_url = "http://vm0542.kaj.pouta.csc.fi/search",
                               terms_conf,
                               fields,
                               min_freq = 1) {
  search_term <- tolower(as.character(input$search_term))
  min_score <- paste0("&minScore=", min_freq)
  query_results <- api3_get_rest_query_results(search_term,
                                               rest_api_url,
                                               terms_conf,
                                               fields,
                                               min_score)
  if (is.null(query_results)) {
    return(NULL)
  }
  enriched_query_results <- api3_enrich_rest_query_results(query_results)
  return(enriched_query_results)
}


api3_sum_estcid_hits <- function(query_results) {
  # as one ecco id corresponds to multiple estcids, we need to summarize those:
  summarised_results <- aggregate(query_results$freq, by=list(query_results$id), FUN=sum)
  colnames(summarised_results) <- c("id", "freq")
  return(summarised_results)
}





