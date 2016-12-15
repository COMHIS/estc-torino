library('RCurl')
library('jsonlite')

terms_url <- "https://vm0175.kaj.pouta.csc.fi/ecco-search/terms"
terms_conf <- "&d=1&cp=1"
get_results_url <- "https://vm0175.kaj.pouta.csc.fi/ecco-search/search"
fields <- "&f=heading_index&f=metadata_pubDate&f=metadata_module&f=heading_frontmatter&f=metadata_documentType&f=contents_index&f=heading_backmatter&f=heading_body&f=contents_frontmatter&f=contents_TOC&f=metadata_notes&f=metadata_fullTitle&f=heading_TOC&f=contents_titlePage&f=contents_body&f=metadata_language&f=contents_backmatter"


json_to_dataframe <- function(json_data) {
  list_data <- fromJSON(json_data)
  col1 <- names(list_data)
  col2 <- unlist(list_data)
  resulting_dataframe <- data.frame(term = col1, count = col2)
  resulting_dataframe <- resulting_dataframe[order(resulting_dataframe$count,
                                                   decreasing = TRUE),]
}

shinyServer(function(input, output) {
  
  output$tableOut <- renderTable({
    search_text <- input$search_term
    search_text <- paste0("?q=", search_text)
    search_request <- paste0(terms_url, search_text, terms_conf, fields)
    request_result <- getURL(search_request) # returns JSON
    # request_result_fromjson <- fromJSON(request_result)
    request_result_as_df <- json_to_dataframe(request_result)
    return(request_result_as_df)
  })
})

library('RCurl')
library('jsonlite')


fields <- "&f=heading_index&f=metadata_pubDate&f=metadata_module&f=heading_frontmatter&f=metadata_documentType&f=contents_index&f=heading_backmatter&f=heading_body&f=contents_frontmatter&f=contents_TOC&f=metadata_notes&f=metadata_fullTitle&f=heading_TOC&f=contents_titlePage&f=contents_body&f=metadata_language&f=contents_backmatter"


test_rest_request <- "https://vm0175.kaj.pouta.csc.fi/ecco-search/search?q=monarchy&f=heading_index&f=heading_frontmatter&f=contents_index&f=heading_backmatter&f=heading_body&f=contents_frontmatter&f=contents_TOC&f=heading_TOC&f=contents_titlePage&f=contents_body&f=contents_backmatter&rf=metadata_ESTCID&mf=1"
test_result <- getURL(test_rest_request) 
test_rest_csv <- read.csv(test_rest_request, header = TRUE)
# read.table(test_result)

test_terms_url <- "https://vm0175.kaj.pouta.csc.fi/ecco-search/terms?q=south%20sea&d=1&cp=1&f=heading_frontmatter"
test_terms_result <- getURL(test_terms_url)
test_terms_result_fromjson <- jsonlite::fromJSON(test_terms_result)

testitabbeli <- as.table(request_result_fromjson)
tt <- table(request_result_fromjson)
renderTable(request_result_fromjson)
