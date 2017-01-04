
api2_collocations <- "https://vm0175.kaj.pouta.csc.fi/ecco-search2/collocations"
term <- "?term=private"
# conf <- "&sumScaling=DF&minSumFreq=100&limit=50&pretty&localScaling=FLAT"
conf <- "&sumScaling=TTF&limit=1000&pretty&localScaling=ABSOLUTE&minTotalTermFreq=30000&maxTotalTermFreq=10000000"


get_timelimit_conf <- function(start_year, end_year) {
  timelimit_conf <- paste0("&limitQuery=pubDate:[", start_year, "0000%20TO%20", end_year,"0000]")
  return(timelimit_conf)
}


get_unique_words_in_collocations <- function(collocation_list) {
  all_words <- vector()
  for (collocation in all_collocations) {
    words <- collocation
    all_words <- append(all_words, names(words))
  }
  all_words_u <- unique(all_words)
  return(all_words_u)
}


# start_years <- c(1700, 1710, 1720, 1730, 1740, 1750, 1760, 177, 1780, 1790)
start_years <- c(1700, 1710)

all_collocations <- list()

for (year in start_years) {
  year_collocations <-
    (jsonlite::fromJSON(paste0(api2_collocations,
                               term,
                               conf,
                               get_timelimit_conf(year, (year + 9)))
                        )
     )$collocations
  all_collocations[[as.character(year)]] <- year_collocations
}

words <- get_unique_words_in_collocations(all_collocations)


output_pre_df <- matrix(ncol = length(words), nrow = length(start_years))
words_by_year_df <- as.data.frame(output_pre_df)
colnames(words_by_year_df) <- words
rownames(words_by_year_df) <- start_years


for (row_name in rownames(words_by_year_df)) {
  print(row_name)
  
  words_by_year_df[row_name, ]
}

