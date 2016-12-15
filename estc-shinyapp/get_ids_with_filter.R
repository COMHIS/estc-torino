

get_data_subset_with_filter <- function(dataset, filter_value, filter_field, case_sensitive = TRUE) {
  # filtered_data <- dataset[which(dataset[, filter_field] == filter_value), ]
  if (case_sensitive) {
    filtered_data <- dataset[which(grepl(filter_value, dataset[, filter_field])), ]
  } else {
    filtered_data <- dataset[which(grepl(tolower(filter_value), tolower(dataset[, filter_field]))), ]
  }
  return(filtered_data)
}


get_data_subset_ids_csv <- function(dataset, csv_name) {
  subset_ids <- read.csv(paste0("../data/saved_query_ids/", csv_name, ".csv"))
  data_subset <- dataset[dataset$id %in% subset_ids$id, ]
  return(data_subset)
}


get_data_subset_id_df_filter <- function(dataset, filter_df, filter_method = c("not", "and", "none")) {
  filter_method <- match.arg(filter_method)
  if (filter_method == "not") {
    filtered_df <- dataset[!(dataset$id %in% filter_df$id), ]
  }
  if (filter_method == "and") {
    filtered_df <- dataset[(dataset$id %in% filter_df$id), ]
  }
  if (filter_method == "none") {
    filtered_df <- dataset
  }
  return(filtered_df)
}
