

# REFRACTOR to more general get hits per field -vv
get_hits_per_author <- function (dataset, custom.ids, field = "author", nchar = 40) {
  hits_author <- dataset
  # field <- "author"
  
  # Use unnormalized frequency for now
  hits_author$hits <- unlist(custom.ids[match(hits_author$id, custom.ids$id), "freq"],
                             use.names = F) 
  hits_author$names <- hits_author[[field]]
  hits_author_total <- hits_author %>% group_by(names) %>%
    filter(!is.na(names)) %>%
    dplyr::summarise(count = sum(hits, na.rm = T), n = n()) %>%
    mutate(hits.per.edition = count / n)
  
  # Limit title length
  hits_author_total$names <- substr(as.character(hits_author_total$names), 1, nchar)
  
  return(hits_author_total)
}


get_top_authors <- function(hits_author_total, ntop = 20) {
  dfs <- hits_author_total %>% arrange(desc(count)) %>% head(ntop) 
  dfs$names <- droplevels(factor(dfs$names, levels = rev(dfs$names)))
  return(dfs)
}

get_top_authors_by_edition <- function(hits_author_total, ntop = 20) {
  top_authors_by_edition <-
    hits_author_total %>% arrange(desc(hits.per.edition)) %>% head(ntop) 
  top_authors_by_edition$names <-
    droplevels(factor(top_authors_by_edition$names,
                      levels = rev(top_authors_by_edition$names)))
  return(top_authors_by_edition)
}
