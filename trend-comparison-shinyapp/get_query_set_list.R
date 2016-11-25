

get_query_set_list <- function(source_dir,
                               pubs_yearly_all,
                               dataset) {
  
  sourcefiles <- list.files(paste0("./", source_dir),
                            pattern = "*.csv",
                            full.names = TRUE)
  query_set_list <- vector("list", length(sourcefiles))
  i <- 1
  for (file in sourcefiles) {
    print(paste0("processing: ", file))
    query_set <- get_query_set_with_freqs(file, pubs_yearly_all,
                                          dataset)
    query_set_list[[i]] <- query_set
    i <- i + 1
  }
  return(query_set_list)
}

# get_query_set_list_api <- function(query_sets,
#                                pubs_yearly_all,
#                                dataset) {
#   
#   query_set_list <- vector("list", length(sourcefiles))
#   i <- 1
#   for (file in sourcefiles) {
#     print(paste0("processing: ", file))
#     query_set <- get_query_set_with_freqs(file, pubs_yearly_all,
#                                           dataset)
#     query_set_list[[i]] <- query_set
#     i <- i + 1
#   }
#   return(query_set_list)
# }
