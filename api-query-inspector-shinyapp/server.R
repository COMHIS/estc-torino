
library(shiny)
library(devtools)
load_all("../bibliographica")
load_all("../estc")
load_all()
source("resultsdata_functions.R")


basedata <- readRDS("../data/enriched_and_streamlined_data.Rds")


# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  api2_query_sanity <- reactive({
    api2_query_verify_sanity(input$api2_query)
  })
  
  api2_query_results_df <- reactive({
    if (api2_query_sanity()) {
      query_url <- get_api2_query(term = input$api2_query, api_return_fields = "&field=ESTCID,totalPages")
      query_results_df <- get_api2_jsearch_query_results_df(query_url, column_names = c("id", "pages_ecco", "hits"))
      print(paste0("query_results_df rows: ", nrow(query_results_df)))
      query_counts <- get_api2_query_counts(query_results_df)
      print(paste0("query_counts length: ", nrow(query_counts)))
      resultsdata <- resultsdata_add_query_hits(basedata, query_counts)
      resultsdata <- resultsdata_add_ecco_pages(resultsdata, query_results_df)
      resultsdata <- resultsdata_add_hits_per_page(resultsdata, pages_as_integers = TRUE, ecco_pages = TRUE)
      return(resultsdata)
    } else {
      empty_df <- data.frame(nodata = c("query did not validate"))
      return(empty_df)
    }
  })
  
     
  output$query_results <- renderDataTable({
    api2_query_results_df()
  }, options = list(orderClasses = TRUE))
  
  
  output$helper_text <- renderText({
    "Query must be at least 5 characters long to avoid freezing the API.
    Example query: +america +rebellion"
  })
})

