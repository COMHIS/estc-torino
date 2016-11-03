

plot_top_titles_by_title_count <- function (dataset, nchar = 40, ntop = 20) {
  df <- dataset
  # Limit title length
  df$title <- substr(as.character(df$title), 1, nchar)
  v <- 10 ^ (1:max(na.omit(round(log10(max(table(df$title)))))))
  p1 <- top_plot(df, "title", ntop) +
    ggtitle(paste("Title count")) +
    ylab("Title count (n)") +
    scale_y_log10(breaks = v, labels = v)    		  

  return (p1)
}


plot_paper_consumption_books_vs_pamphlets <- function (dataset) {
  df <- dataset
  df2 <- df %>% group_by(publication_year, document_type) %>%
    summarize(paper = sum(paper, na.rm = TRUE),
              n = n()) %>%
    filter(!is.na(document_type))
  p <- ggplot(df2, aes(x = publication_year,
                       y = paper,
                       group = document_type,
                       color = document_type))
  p <- p + geom_point() + scale_y_log10()
  p <- p + geom_smooth(method = "loess")
  p <- p + xlab("Year")
  p <- p + ylab("Paper (sheets)")
  return (p)
}


plot_relative_titlecount_timeline <- function(df0, df0.allplaces, df.preprocessed,
                                              df.preprocessed.allplaces,
                                              selected.place = "All",
                                              myfield = "titlecount") {
  # Compare the selected field in between the two data sets
  df <- timeline_relative(df0, df.preprocessed, myfield)
  df$group <- rep(selected.place, nrow(df))
  if (!selected.place == "All") {
    df.allplaces <- timeline_relative(df0.allplaces, df.preprocessed.allplaces, myfield)
    df.allplaces$group <- rep("All", nrow(df.allplaces))
    df <- bind_rows(df, df.allplaces)
  }
  df$group <- factor(df$group)
  
  # Mark NAs to 0
  df2 <- df %>% select(publication_time, group, fraction) 
  df2 <- unique(df2)
  df2 <- df2 %>% spread(publication_time, fraction, fill = 0)
  df2 <- df2 %>% gather(publication_time, fraction, -group)
  df2$publication_time <- as.numeric(as.character(df2$publication_time))
  
  p2 <- ggplot(df2, aes(y = fraction, x = publication_time, fill = group)) + 
    geom_bar(stat = "identity", position = "dodge", color = "black") +
    scale_fill_manual(values = c("black", "darkgray")) +
    ylab("Fraction of total documents (%)")  + 
    xlab("Publication time") +
    ggtitle("Query relative to all docs") 
  # guides(fill = "none")
  if (!selected.place == "All") {
    p2 <- p2 + scale_fill_manual("Place", 
                                 values = c("black", "darkgray"), 
                                 labels = c(selected.place, "All"))
  }
  return(p2)
}


plot_titlecount_timeline <- function(df0, df0.allplaces, selected.place = "All",
                                     myfield = "titlecount") {
  # If selected place is given, then show
  # both the selected and all places
  # myfield <- "titlecount"
  df1 <- NULL
  if (!selected.place == "All") {
    df1 <- df0.allplaces
  }
  
  # Standard timeline
  # If place selection is applied then show both the
  # selected place and total data
  p1 <- plot_timeline(df0, df1, field = myfield, nmin = 0, mode = "absolute") +
    ylab("Title count (n)") + ggtitle("Total title count timeline ()") 
  #guides(fill = "none") 
  
  if (!selected.place == "All") {
    p1 <- p1 + scale_fill_manual("Place", 
                                 values = c("black", "darkgray"), 
                                 labels = c(selected.place, "All"))
  }
  
  p1 <- p1 + scale_y_log10()
  return(p1)
}


# !REFRACTOR and combine with plot_titlecount_timeline above -vv
plot_paper_consumption_timeline <- function(df0, df0.allplaces, selected.place = "All",
                                            myfield = "paper") {
  # myfield <- "paper"
  df1 <- NULL
  if (!selected.place == "All") {
    df1 <- df0.allplaces
  }
  
  # Standard timeline
  # If place selection is applied then show both the
  # selected place and total data
  p1 <- plot_timeline(df0, df1, field = myfield, nmin = 0, mode = "absolute") +
    ylab("Paper (sheets)") + ggtitle("Total paper consumption timeline ()") 
  #	guides(fill = "none")	
  
  if (!selected.place == "All") {
    p1 <- p1 + scale_fill_manual("Place", 
                                 values = c("black", "darkgray"), 
                                 labels = c(selected.place, "All"))
  }
  p1 <- p1 + scale_y_log10()
  return(p1)
}

# !REFRACTOR combine with plot_relative_titlecount_timeline -vv
plot_relative_paper_consumption_timeline <- function(df0, df0.allplaces,
                                                     df.preprocessed,
                                                     df.preprocessed.allplaces,
                                                     selected.place = "All",
                                                     myfield = "paper") {
  # Compare the selected field in between the two data sets
  df <- timeline_relative(df0, df.preprocessed, myfield)
  df$group <- rep(selected.place, nrow(df))
  if (!selected.place == "All") {
    df.allplaces <- timeline_relative(df0.allplaces, df.preprocessed.allplaces, myfield)
    df.allplaces$group <- rep("All", nrow(df.allplaces))
    df <- bind_rows(df, df.allplaces)
  }
  df$group <- factor(df$group)

  # Mark NAs to 0
  df2 <- df %>% select(publication_time, group, fraction) 
  df2 <- unique(df2)
  df2 <- df2 %>% spread(publication_time, fraction, fill = 0)
  df2 <- df2 %>% gather(publication_time, fraction, -group)
  df2$publication_time <- as.numeric(as.character(df2$publication_time))

  p2 <- ggplot(df2, aes(y = fraction, x = publication_time, fill = group)) + 
    geom_bar(stat = "identity", position = "dodge", color = "black") +
    scale_fill_manual(values = c("black", "darkgray")) +
    ylab("Fraction of total documents (%)")  + 
    xlab("Publication time") +
    ggtitle("Query relative to all docs") 
  #guides(fill = "none")
  
  if (!selected.place == "All") {
    p2 <- p2 + scale_fill_manual("Place", 
                                 values = c("black", "darkgray"), 
                                 labels = c(selected.place, "All"))
  }
  return(p2)
}

# Paper consumption: books vs. pamphlets
plot_books_vs_pamphlets <- function(df0) {
  df <- df0
  df2 <- df %>% group_by(publication_year, document_type) %>%
    summarize(paper = sum(paper, na.rm = TRUE), n = n()) %>%
    filter(!is.na(document_type))
  
  p <- ggplot(df2, aes(x = publication_year,
                       y = paper,
                       group = document_type,
                       color = document_type))
  
  p <- p + geom_point() + scale_y_log10() 
  p <- p + geom_smooth(method = "loess")
  p <- p + xlab("Year")
  p <- p + ylab("Paper (sheets)")
  
  return(p)
}


plot_top_authors_total_hits <- function(top_authors) {
  p <- ggplot(top_authors, aes(x = names, y = count))
  p <- p + geom_bar(stat = "identity")
  p <- p + coord_flip()
  v <- 10 ^ (1:max(na.omit(round(log10(max(top_authors$count))))))
  p <- p + scale_y_log10(breaks = v, labels = v)
  p <- p + ylab("Term occurrences (n)") + xlab("")
  p <- p + ggtitle("Total hits")
  return(p)
}


plot_top_authors_hits_per_edition <- function(top_authors_by_edition) {
  p <- ggplot(top_authors_by_edition, aes(x = names, y = hits.per.edition))
  p <- p + geom_bar(stat = "identity")
  p <- p + coord_flip()
  v <- 10 ^ (1:max(na.omit(round(log10(max(top_authors_by_edition$hits.per.edition))))))
  p <- p + scale_y_log10(breaks = v, labels = v)
  p <- p + ylab("Hits per edition (n)") + xlab("")
  p <- p + ggtitle("Hits/edition")
  return(p)
}


plot_titlecount_timeline_for_top10_authors <- function(df0, top_authors_n = 10){
  df <- df0
  # theme_set(theme_bw(20))
  top.authors <- names(top(df, field = "author", n = top_authors_n))
  dfs <- df %>% filter(author %in% top.authors) %>%
    group_by(author, publication_time) %>%
    tally() %>%
    arrange(publication_time)
  v <- seq(min(dfs$publication_time), max(dfs$publication_time), 20)
  p <- ggplot(dfs, aes(x = publication_time, y = n, fill = author)) + # , color = "black"
    geom_bar(stat = "identity", position = "stack") +
    xlab("Publication time") +
    ylab("Title count (n)") +
    # scale_fill_grey() +
    scale_fill_hue() +
    scale_x_continuous(breaks = v, labels = v) +
    guides(fill = guide_legend("Author", reverse = TRUE)) 
  return(p)
}


top_places_by_titlecount_plot <- function(df0, ntop = 20) {
  df <- df0
  p <- NULL
  v <- 10 ^ (1:max(na.omit(round(log10(max(table(df$publication_place)))))))
  p1 <- top_plot(df, "publication_place", ntop) +
    ggtitle(paste("Title count")) +
    ylab("Title count (n)") +
    scale_y_log10(breaks = v, labels = v) 
  return(p1)
}

# Relation between titlecount and query hits
titlecount_queryhits_relation_plot <- function(df0, custom.ids,
                                               field = "publication_place") {
  df <- df0
  df$names <- df[[field]]
  df$hits <-  unlist(custom.ids[match(df$id, custom.ids$id), "freq"], use.names = F)
  
  # Title counts
  dfs <- df %>% group_by(names) %>%
    filter(!is.na(names)) %>% 
    summarise(n = n(), hits = sum(hits, na.rm = T))
  
  p2 <- ggplot(dfs, aes(x = n, y = hits, label = names)) +
    geom_text() + geom_smooth(method = "lm") +
    scale_x_log10() + scale_y_log10() +
    xlab("Title count") + ylab("Query hits") +
    ggtitle("Comparison of title count vs. query hits")

  return(p2)
}

# !REFRACTOR top author plots very similar - same for next 2 -vv
top_titlehits_plot <- function(dfs) {
  p <- ggplot(dfs, aes(x = names, y = count))
  p <- p + geom_bar(stat = "identity")
  p <- p + coord_flip()
  v <- 10 ^ (1:max(na.omit(round(log10(max(dfs$count))))))
  p <- p + scale_y_log10(breaks = v, labels = v)
  p <- p + ylab("Term occurrences (n)") + xlab("")
  p <- p + ggtitle("Total hits")
  return(p)
}


top_titlehits_edition_plot <- function(dfs) {
  p <- ggplot(dfs, aes(x = names, y = hits.per.edition))
  p <- p + geom_bar(stat = "identity")
  p <- p + coord_flip()
  v <- 10^(1:max(na.omit(round(log10(max(dfs$hits.per.edition))))))
  p <- p + scale_y_log10(breaks = v, labels = v)
  p <- p + ylab("Hits per edition (n)") + xlab("")
  p <- p + ggtitle("Hits/edition")
  return(p)
}


top_publishers_plot <- function(dataset,
                                field = "publisher",
                                ntop = 20,
                                nchar = 40,
                                plot_title = "Title count") {
  y_label <- paste(plot_title, "(n)", sep = " ")
  p <- top_plot(dataset, field, ntop, max.char = nchar) +
    ggtitle(plot_title) +
    ylab(y_label)
  return(p)
}


top_publishers_abs_hits_plot <- function(top_publishers_abs_hits) {
  p <- ggplot(top_publishers_abs_hits, aes(x = names, y = count))
  p <- p + geom_bar(stat = "identity")
  p <- p + coord_flip()
  p <- p + ylab("Term occurrences (n)") + xlab("")
  p <- p + ggtitle("Query hits")
  return(p)
}

