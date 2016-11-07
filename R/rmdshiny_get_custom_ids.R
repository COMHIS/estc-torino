## PICK SELECTED DOCUMENTS (df0)

# Read the custom list of entry IDs
# Including also the number of hits per document
# and document total length for normalization purposes

# Return df of:
# id of document, hits per document, lenght of document
format_query_ids <- function(idsource) {
  
  idfile <- idsource
  
  if (file_ext(idfile) == "csv") {
    custom.ids <- read.csv(idfile, header = TRUE)[, c(1, 2, 3)]  
  } else {
    custom.ids <- read.table(idfile, skip = 1)[, c(1, 2, 4)]
  }
  
  custom.ids[,1] <- gsub("\\,$", "", as.character(custom.ids[,1]))
  custom.ids[,2] <- as.numeric(as.character(custom.ids[,2]))
  custom.ids[,3] <- as.numeric(as.character(custom.ids[,3]))
  names(custom.ids) <- c("id", "freq", "length")
  # Remove leading zeroes from the IDs to get them compatible with our versions
  custom.ids$id <- apply(cbind(substr(custom.ids$id, 1, 1),
                               gsub("^[A-Z]0*", "", custom.ids$id)),
                         1, function(x) {paste(x, collapse = "")})
  formatted_ids <- custom.ids
  return(formatted_ids)
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
