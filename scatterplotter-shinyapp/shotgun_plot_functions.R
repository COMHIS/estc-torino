library(ggplot2)
library(plotly)
library(useful)

get_mds_data <- function(search_string) {
  api_start <- "https://vm0175.kaj.pouta.csc.fi/ecco-search2/collocations"
  term_query <- paste0("?term=", search_string)
  # api_conf <- "&limit=10&pretty&localScaling=FLAT&limitQuery=pubDate:[17890000%20TO%2017900000]&mdsDimensions=2&sumScaling=DF&minSumFreq=100"
  api_conf <- "&limit=100&pretty&localScaling=FLAT&limitQuery=pubDate:[17890000%20TO%2017900000]&mdsDimensions=2&maxTotalTermFreq=5000&minTotalTermFreq=50&sumScaling=ABSOLUTE"
  # api_conf <- "&limit=30&pretty&localScaling=FLAT&limitQuery=pubDate:[17500000%20TO%2017540000]&mdsDimensions=2&maxTotalTermFreq=5000&minTotalTermFreq=50&sumScaling=ABSOLUTE"
  # api_conf <- "&limit=20&pretty&localScaling=FLAT&limitQuery=pubDate:[17500000%20TO%2017540000]&mdsDimensions=2&maxTotalTermFreq=500&minTotalTermFreq=50"
  api_query <- paste0(api_start, term_query, api_conf)
  response_json <- (jsonlite::fromJSON(api_query))
  mds_data <- response_json$collocations
  terms <- names(mds_data)
  x <- vector()
  y <- vector()
  weight <- vector()
  colour <- vector()
  i <- 1
  for (item in mds_data) {
    item_x <- as.numeric(item$termVector[[1]])
    item_y <- as.numeric(item$termVector[[2]])
    item_weight <- as.numeric(item$weight)
    x[i] <- item_x
    y[i] <- item_y
    weight[i] <- item_weight
    carto <- cart2pol(item_x, item_y)
    colour[i] <- (carto$theta * carto$r)
    i <- i + 1
  }
  mds_df <- data.frame(terms = terms, x = x, y = y, weight = weight, colour = colour)
  return(mds_df)
}


get_shotgun_plot <- function(mds_df) {
  plot <- ggplot(data = mds_df, aes(x = mds_df$x, y = mds_df$y, size = mds_df$weight)) + 
    geom_point(aes(text = mds_df$terms, colour = mds_df$colour)) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank())
    # scale_colour_gradientn(colours = c(rainbow(40),
    #                                    rainbow(1)
    #                                    )
    #                        )

  plot <- ggplotly(plot, tooltip = c("text"))
  return(plot)
}
