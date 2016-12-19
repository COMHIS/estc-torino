
get_mds_data <- function(search_string) {
  api_start <- "https://vm0175.kaj.pouta.csc.fi/ecco-search2/collocations"
  term_query <- paste0("?term=", search_string)
  api_conf <- "&limit=20&pretty&localScaling=FLAT&limitQuery=pubDate:[17500000%20TO%2017540000]&mdsDimensions=2&maxTotalTermFreq=5000&minTotalTermFreq=5"
  # api_conf <- "&limit=20&pretty&localScaling=FLAT&limitQuery=pubDate:[17500000%20TO%2017540000]&mdsDimensions=2&maxTotalTermFreq=5000&minTotalTermFreq=50&sumScaling=ABSOLUTE"
  api_query <- paste0(api_start, term_query, api_conf)
  mds_data <- (jsonlite::fromJSON(api_query))$collocations
  terms <- names(mds_data)
  x <- vector()
  y <- vector()
  i <- 1
  for (item in mds_data) {
    item_x <- as.numeric(item$termVector[[1]])
    item_y <- as.numeric(item$termVector[[2]])
    x[i] <- item_x
    y[i] <- item_y
    i <- i + 1
  }
  mds_df <- data.frame(terms = terms, x = x, y = y)
  return(mds_df)
}


get_shotgun_plot <- function(mds_df) {
  plot <- ggplot(data = mds_df, aes(x = mds_df$x, y = mds_df$y)) + 
    geom_point(aes(text = mds_df$terms)) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank()
          )

  plot <- ggplotly(plot, tooltip = c("text"))
  return(plot)
}
