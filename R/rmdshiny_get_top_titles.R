
get_total_titlehits <- function(df0, custom.ids, field = "title", nchar = 40) {
  df <- df0
  # v seems to have no use -vv
  v <- 10 ^ (1:max(na.omit(round(log10(max(table(df$title)))))))
  # Use unnormalized frequency for now
  # field <- "title"
  df$hits <-  unlist(custom.ids[match(df$id, custom.ids$id), "freq"], use.names = F)
  df$names <- df[[field]]
  dfs.total <- df %>% group_by(names) %>%
    filter(!is.na(names)) %>% 
    summarise(n = n(), # number of editions
              count = sum(hits, na.rm = T)) %>% # number of hits
    mutate(hits.per.edition = count/n)
  # Limit title length
  dfs.total$names <- substr(as.character(dfs.total$names), 1, nchar)
  dfs.total$names <- make.unique(dfs.total$names)
  return(dfs.total)
}


get_top_n_titles <- function(dfs.total, ntop = 10, custom.ids = NA,
                             calculate_length = FALSE) {
  # Select top-N
  dfs <- dfs.total %>% arrange(desc(count)) %>% head(ntop) 
  # Retrieve document length from the ID info
  if (calculate_length) {
    try(if (is.na(custom.ids)) {
      stop("ERROR: No custom ids specified in function call!")
    })
    dfs$document_length <-
      # not used anywhere -vv
      custom.ids[match(df$id[match(dfs$names, df$title)], custom.ids$id), "length"]
  }
  
  dfs$names <- droplevels(factor(dfs$names, levels = rev(dfs$names)))
  # dfs$names <- as.character(dfs$names)
  return(dfs)
}

get_top_titles_by_edition <- function(dfs.total, ntop = 20) {
  dfs.total <- dfs.total %>% arrange(desc(hits.per.edition)) %>% head(ntop) 
  dfs.total$names <- droplevels(factor(dfs.total$names, levels = rev(dfs.total$names)))
  return(dfs.total)
}
