

get_ids_with_filter <- function(dataset, filter_value, filter_field) {
  filtered_data <- dataset[which(dataset[, filter_field] == filter_value), ]
  return(filtered_data)
}


get_data_subset_ids_csv <- function(dataset, csv_name) {
  subset_ids <- read.csv(paste0("../data/saved_query_ids/", csv_name, ".csv"))
  data_subset <- dataset[dataset$id %in% subset_ids$id, ]
  return(data_subset)
}

