

get_top_publishers_abs_hits <- function(dataset,
                                        query_ids,
                                        field,
                                        ntop = 20,
                                        nchar = 40) {
  df <- dataset
  custom.ids <- query_ids
  # field <- "publisher"
  
  df$hits <-  unlist(custom.ids[match(df$id, custom.ids$id), "freq"], use.names = F)
  df$names <- df[[field]]
  dfs <- df %>% group_by(names) %>%
    filter(!is.na(names)) %>% 
    summarise(count = sum(hits, na.rm = T)) %>%
    arrange(desc(count)) %>%
    head(ntop)
  
  # Limit title length
  dfs$names <- substr(as.character(dfs$names), 1, nchar)
  dfs$names <- make.unique(dfs$names)
  dfs$names <- droplevels(factor(dfs$names, levels = rev(dfs$names)))
  
  return(dfs)
}

