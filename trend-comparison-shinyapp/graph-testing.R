library(shiny)
library(devtools)
load_all("../R/bibliographica")
load_all()
library(magrittr)
library(reshape2)
library(gridExtra)
library(knitr)
library(ggmap)
library(dplyr)
library(tidyr)
library(ggplot2)
library(sorvi)
library(tools)
library(RCurl)
library(jsonlite)
library(stringi)
library(plyr)

source("dataset_processing_functions.R")
source("query_ids_functions.R")
source("query_summary_functions.R")
source("get_query_set_list.R")
source("plot_functions.R")

input <- list(range_years = c(1705, 1799), 
              publication_place = "All",
              language = "any",
              document_type = "All")

dataset_from_rds <- readRDS("../inst/examples/data/estc_df.Rds")
dataset <- augment_original_data(dataset_from_rds)

query_ids <- get_query_ids_df_from_csv("comparables/politeness.csv")$id
filter_list <- list(id = query_ids,
                    publication_year = c(1705, 1799))
politeness_base <- get_filtered_dataset(filter_list, dataset)

politeness_pubs_yearly_all <-
  get_publications_yearly(politeness_base)

query_sets_list <- get_query_set_list(source_dir = "comparables/pol-civ",
                                      politeness_pubs_yearly_all,
                                      politeness_base)

plot <- plot_titlecount_relative(title = "politeness, civility and civilized",
                                 query_sets_list)
plot
