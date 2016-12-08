dataset <- augment_original_data(readRDS("data/estc_df.Rds"), time_window = NA)
dataset_sub <- dataset[, c("id", "author_name", "title", "publication_year","pagecount", "document_type")]