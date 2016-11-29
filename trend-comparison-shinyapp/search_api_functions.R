
process_search_term <- function(search_term){
  sanitized_term <- sanitize_term(search_term)
  sanity <- sanity_check_term_query(search_term)
  return(list(search_term, sanitized_term, sanity))
}

fix_query_chars <- function(query) {
  query <- gsub(" ", "%20", query)
  query <- gsub("\\+", "%2B", query)
  query <- gsub("\"", "%22", query)
  return(query)
}

get_api_query_set <- function(baseterm, comparables) {
  api_url <- "https://vm0175.kaj.pouta.csc.fi/ecco-search/search?q="
  fields <- "&f=heading_index&f=heading_frontmatter&f=contents_index&f=heading_backmatter&f=heading_body&f=contents_frontmatter&f=contents_TOC&f=heading_TOC&f=contents_titlePage&f=contents_body&f=contents_backmatter"
  results <- "&rf=metadata_ESTCID&mf=1"
  base_query <- paste0(api_url, baseterm, fields, results)
  base_query <- fix_query_chars(base_query)
  base_query_set <- list(term = baseterm, query = base_query)
  comparable_queries <- vector("list", length(comparables))
  
  i <- 1
  for (comparable in comparables) {
    comparable_query <- paste0(api_url, "+", baseterm, " +(", comparable, ")", fields, results)
    comparable_query <- fix_query_chars(comparable_query)
    comparable_query_set <- list(term = comparable, query = comparable_query)
    comparable_queries[[i]] <- comparable_query_set
    i <- i + 1
  }
  
  return(list(base_query = base_query_set, comparable_queries = comparable_queries))
}


get_pubs_yearly_for_query <- function(api_query, dataset) {
  ids <- enrich_rest_query_results(get_api_query_search_results(api_query))$id
  filter_list <- list(id = ids,
                      publication_year = c(1700, 1799))
  filtered_data <- get_filtered_dataset(filter_list, dataset)
  pubs_yearly <- get_publications_yearly(filtered_data)
  return(pubs_yearly)
}


get_relative_hits_yearly_for_query <- function(api_query,
                                               baseline_yearly,
                                               dataset) {
  pubs_yearly <- get_pubs_yearly_for_query(api_query, dataset)
  relative_hits_yearly <- get_relative_hits_yearly(pubs_yearly, baseline_yearly)
  return(relative_hits_yearly)
}

