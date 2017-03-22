

get_subset <- function(dataset_to_filter,
                       min_year,
                       max_year,
                       selected_language,
                       selected_document_type,
                       selected_location) {

  return_dataset <- dataset_to_filter
  
  # Selected time window
  return_dataset <- subset(return_dataset,
                           publication_year >= min_year &
                             publication_year <= max_year)

  # Selected language
  if (!selected_language == "any") {
    return_dataset <- subset(return_dataset,
                             language %in% selected_language)
  }
  
  # Selected document type
  if (!selected_document_type == "All") {  
    return_dataset <- subset(return_dataset,
                             document_type %in% selected_document_type)
    
    # Remove issues if Pamphlets are selected
    if (selected_document_type == "Pamphlets") {
      return_dataset <- subset(return_dataset,
                               issue == TRUE)
    }
    
  } 
  
  # Selected place
  return_dataset_all_locations <- return_dataset
  if (!selected_location == "All") {
    # Keep version with all places
    # Make version with selected place
    return_dataset <- subset(return_dataset,
                             publication_place %in% selected_location)
  }
  
  returnlist <- list(place_subsetted = return_dataset,
                     all_places = return_dataset_all_locations)
  
  return(returnlist)
}
