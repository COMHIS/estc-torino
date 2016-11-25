

get_eccoapi_paragraph_hit_per_document <- function (query_string) {
  api_url <- "https://vm0175.kaj.pouta.csc.fi/ecco-search2/search?query="
  results_params <- "&field=ESTCID&level=paragraph"
  api_call <- paste0(api_url, query_string, results_params)
  results <- read.csv(api_call, header = TRUE)
  results_aggregated <- aggregate(Freq~ESTCID, FUN = sum, data = results)
  return(results_aggregated)
}

