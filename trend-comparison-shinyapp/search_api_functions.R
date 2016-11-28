
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
                      publication_year = c(1705, 1799))
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

get_api2_query <- function(term = "+religion",
                          api_url = "https://vm0175.kaj.pouta.csc.fi/ecco-search2/jsearch?query=",
                          api_return_fields = "&field=ESTCID") {
  api2_params <- "&limit=2147483647"
  api2_query <- paste0(api_url, term, api_return_fields, api2_params)
  api2_query <- fix_query_chars(api2_query)
  return(api2_query)
}

get_api2_query_set <- function(base_term, comparable_terms) {
  base_set <- list(base_term, get_api2_query(term = base_term))
  
  comparable_query_sets <-  vector("list", length(comparable_terms))
  i <- 1
  for (comparable_term in comparable_terms) {
    comparable_term <- paste0(base_term, " +(", comparable_term, ")")
    comparable_set <- list(comparable_term, get_api2_query(term = comparable_term))
    comparable_query_sets[[i]] <- comparable_set
    i <- i + 1
  }
  api2_query_set <- list(base_query_set = base_set,
                         comparable_query_sets = comparable_query_sets)
  return(api2_query_set)
}

# get_api2_jsearch_query_results_df <- function(query) {
#   # results is json
#   jsonlite
# }


