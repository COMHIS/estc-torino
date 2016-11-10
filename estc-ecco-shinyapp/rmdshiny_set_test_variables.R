time.window <- 5
min.year <- 1700
max.year <- 1800
selected.place <- "All"
nchar <- 40
selected_language <- "any"
selected_document_type <- "All"
ntop <- 20
idsource <- "data/equality.csv"

library(devtools)
load_all("R/bibliographica")
library(bibliographica)
load_all("R/estc")
library(estc)
library(magrittr)
library(reshape2)
library(gridExtra)
library(knitr)
library(ggmap)
#library(stringr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(sorvi)
source("R/report_plot_functions.R")
source("R/setup_functions.R")
source("R/get_subset.R")
source("R/get_custom_ids.R")
source("R/query_summary_functions.R")
source("R/get_top_authors.R")
source("R/get_top_titles.R")
source("R/get_top_publishers.R")
