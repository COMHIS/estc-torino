
get_filtered_dataset_sans_ids <- function(input, dataset) {
  min_year <- input$range_years[1]
  max_year <- input$range_years[2]
  selected_place <- input$publication_place
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


get_idfiltered_dataset <- function(query_ids, dataset) {
  filtered_dataset <- subset(dataset$place_subsetted,
                             id %in% query_ids$id)
  filtered_dataset_allplaces <- subset(dataset$all_places,
                                       id %in% query_ids$id)
  filtered_dataset_list <- list(place_filtered = filtered_dataset,
                                place_all = filtered_dataset_allplaces)
  return(filtered_dataset_list)
  
}


get_filtered_dataset <- function(filter_list, dataset) {

  if ("id" %in% names(filter_list)) {
    dataset <- subset(dataset, id %in% filter_list$id)
  }
  if ("language" %in% names(filter_list)) {
    dataset <- subset(dataset, language %in% filter_list$language)
  }
  if ("publication_year" %in% names(filter_list)) {
    min_year <- filter_list$publication_year[1]
    max_year <- filter_list$publication_year[2]
    dataset <- dataset %>% filter(publication_year >= min_year &
                                  publication_year <= max_year)
  }
  return(dataset)
}

