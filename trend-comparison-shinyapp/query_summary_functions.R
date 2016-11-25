

get_publications_yearly <- function(catalog_data, years = c(1700, 1799)) {
  catalog_years <- catalog_data$publication_year
  years_range <- years[1]:years[2]
  years_titles <- vector("integer", length(years_range))
  publications_yearly <- data.frame(year = years_range,
                                    titles = years_titles)
  catalog_years_count <- count(catalog_years) # plyr
  colnames(catalog_years_count) <- c("year", "titles")
  # discard out of years_range
  catalog_years_count <- subset(catalog_years_count,
                                year >= years_range[1] &
                                  year <= years_range[length(years_range)])
  # combine dataframes to fill in years with zero titles
  publications_yearly$titles <-
    catalog_years_count[match(publications_yearly$year,
                              catalog_years_count$year), 2]
  # fill na with 0
  publications_yearly$titles[is.na(publications_yearly$titles)] <- 0
  return(publications_yearly)
}

get_relative_hits_yearly <- function(hits_subset_yearly, hits_all_yearly) {
  averages_yearly <- hits_subset_yearly
  averages_yearly["total_titles"] <- hits_all_yearly["titles"]
  averages_yearly["frequency"] <- averages_yearly["titles"] / averages_yearly["total_titles"]
  return(averages_yearly)
}


get_query_set_with_freqs <- function(query_file,
                                     pubs_yearly_all,
                                     dataset) {
  filter_list = list(id = get_query_ids_df_from_csv(query_file)$id)
  idfiltered_dataset <- get_filtered_dataset(filter_list, dataset)
  query_titles_yearly <- get_publications_yearly(idfiltered_dataset)
  query_titles_yearly_averages <-
    get_relative_hits_yearly(query_titles_yearly, pubs_yearly_all)
  query_filename <- basename(query_file)
  query_name <- substr(query_filename, 1, nchar(query_filename) - 4)
  query_set <- list(term = query_name,
                    data = query_titles_yearly_averages)
  return(query_set)
}
