## PICK SELECTED DOCUMENTS (df0)

# Read the custom list of entry IDs
# Including also the number of hits per document
# and document total length for normalization purposes
get_custom_ids <- function(df.preprocessed, idsource) {

  idfile <- idsource
  idfile_extension <- substr(idfile,
                             start = nchar(idfile) - 3,
                             stop = nchar(idfile))
  if (idfile_extension == ".csv") {
    custom.ids <- read.csv(idfile, header = TRUE)[, c(1, 2, 3)]  
  } else {
    custom.ids <- read.table(idfile, skip = 1)[, c(1, 2, 4)]
  }
  ids.orig <- as.character(custom.ids[,1])
  custom.ids[,1] <- gsub("\\,$", "", as.character(custom.ids[,1]))
  custom.ids[,2] <- as.numeric(as.character(custom.ids[,2]))
  custom.ids[,3] <- as.numeric(as.character(custom.ids[,3]))
  names(custom.ids) <- c("id", "freq", "length")
  # Remove leading zeroes from the IDs to get them compatible with our versions
  custom.ids$id <- apply(cbind(substr(custom.ids$id, 1, 1),
                               gsub("^[A-Z]0*", "", custom.ids$id)),
                         1, function(x) {paste(x, collapse = "")})
  
  return(custom.ids)
}