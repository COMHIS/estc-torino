
library(reshape2)


get_yearly_freqs_molten_df <- function(query_sets_list) {
  year <- (query_sets_list[[1]]$data)$year
  graphdata_df <- data.frame(year)

  for (query_set in query_sets_list) {
    colname <- query_set$term
    freqs <- (query_set$data)$frequency
    graphdata_df[colname] <- freqs
  }

  molten_df <- reshape2::melt(data = graphdata_df, id.vars = "year")
  return(molten_df)
}


plot_titlecount_relative <- function(title = "placeholder",
                                     yearly_title_frequencies_list, style = "smooth", plot_colour = "Dark2") {

  graphdata_df <- get_yearly_freqs_molten_df(yearly_title_frequencies_list)

  if (style == "smooth") {
    geom <- geom_smooth(method = "loess", span = 0.2, se = FALSE)
  } else {
    graphdata_df$value[is.na(graphdata_df$value)] <- 0
    geom <- geom_ribbon(aes(ymin=0, ymax=value, fill = variable, colour = NA), position = "stack")
  } # HACK

  plot <- ggplot(data = graphdata_df,
                 aes(x = year, y = value, colour = variable)) + 
    # geom_smooth(method = "loess", span = 0.2, se = FALSE) +
    # geom_ribbon(aes(ymin=0, ymax=value, fill = variable), position = "stack") +
    geom +
    labs(title = title,
         x = "Year",
         y = "Frequency",
         colour = "Search term") +
    scale_x_continuous(breaks = 
      round(seq(min(graphdata_df$year), max(graphdata_df$year + 4), 5), 1)) +
    theme_linedraw() +
    theme(axis.text.x = element_text(size = 9, angle = -90),
          axis.title.x = element_blank(),
          legend.title = element_blank(),
          legend.position = "bottom") +
    scale_colour_brewer(type = "qual", palette = plot_colour) +
    scale_fill_brewer(type = "qual", palette = plot_colour)
  return(plot)
}
