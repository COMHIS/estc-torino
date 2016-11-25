
get_idsource_fullpath <- function(idsource) {
  idsource_fullpath <- paste0("./comparables/", idsource)
  return(idsource_fullpath)
}

get_query_ids_df_from_csv <- function(query_file) {
  query_ids <- format_query_ids(query_file)
  return(query_ids)
}
