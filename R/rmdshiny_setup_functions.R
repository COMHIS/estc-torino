

# Add Metadata ID field 
add_metadata_id_field <- function(df.orig) {
  idfield <- "system_control_number"
  df.orig$id <- sapply(strsplit(as.character(df.orig[, idfield]), ")"),
                       function (xi) {xi[[length(xi)]]})
  return(df.orig)
}


add_document_type <- function(df.orig) {
  # Add document type
  df.orig$document_type <- rep(NA, nrow(df.orig))
  df.orig$document_type[df.orig$pagecount > 32] <- "Books"
  df.orig$document_type[df.orig$pagecount <= 32] <- "Pamphlets"
  df.orig$document_type <- factor(df.orig$document_type)
  return(df.orig)
}


set_time_window <- function(df.orig, time.window = 10) {
  # Set the desired time window (default one decade)
  if (time.window == 10) {
    df.orig$publication_time <- df.orig$publication_decade
  } else {
    df.orig$publication_time <- time.window * floor(df.orig$publication_year / time.window)
  }
  return(df.orig)
}


augment_original_data <- function(dataset, time_window = 10) {
  dataset <- add_metadata_id_field(dataset)
  # Add document type
  dataset <- add_document_type(dataset)
  # Set the desired time window (default one decade)
  dataset <- set_time_window(dataset, time_window)
  return(dataset)
}

