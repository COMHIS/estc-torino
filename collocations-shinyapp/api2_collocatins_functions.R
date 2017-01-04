
get_api2_collocations <- function(term = "consciousness", years = c(1700, 1799)) {
  api_call_start <- "https://vm0175.kaj.pouta.csc.fi/ecco-search2/collocations"
  api_call_term <- paste0("?term=", term)
  api_call_options <- "&sumScaling=DF&minSumFreq=100&limit=100&pretty&localScaling=FLAT"
  api_call_years <- paste0("&limitQuery=pubDate:[", years[1], "0000%20TO%20", years[2], "0000]")
  api_call <- paste0(api_call_start, api_call_term, api_call_options, api_call_years)
}