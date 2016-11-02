

get_subset <- function(df.orig,
                       min.year,
                       max.year,
                       selected_language,
                       selected_document_type,
                       selected.place) {

  df.preprocessed <- df.orig
  
  # Selected time window
  df.preprocessed <- df.preprocessed %>% filter(publication_year >= min.year &
                                                  publication_year <= max.year)
  
  # Selected language
  if (!selected_language == "any") {
    df.preprocessed <- df.preprocessed %>% filter(language %in% selected_language)
  }
  
  # Selected document type
  if (!selected_document_type == "All") {  
    df.preprocessed <- df.preprocessed %>% filter(document_type %in% selected_document_type)
    
    # Remove issues if Pamphlets are selected
    if (selected_document_type == "Pamphlets") {
      df.preprocessed <- df.preprocessed %>% filter(!issue)
    }
    
  } 
  
  # Selected place
  df.preprocessed.allplaces <- df.preprocessed
  if (!selected.place == "All") {
    # Keep version with all places
    # Make version with selected place
    df.preprocessed <- df.preprocessed %>% filter(publication_place %in% selected.place)
  }
  
  returnlist <- list(place_subsetted = df.preprocessed,
                     all_places = df.preprocessed.allplaces)
  
  return(returnlist)
}
