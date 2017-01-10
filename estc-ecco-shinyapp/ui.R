
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # tags$head(tags$link(rel = "icon", type = "image/png", href = "www/favicon48.png")),
  includeCSS("styles.css"),  
  # Application title
  titlePanel("ECCO Explorer"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      textInput(inputId = "subcorpus", label = "Subcorpus:",
                placeholder = "leave blank for whole ECCO"),
      textInput("search_term",
                "Search string:",
                ""),
      selectInput("search_fields", "Search fields:",
                  choices = list("All fields" = "contents_headings_all",
                                 "Titlepages" = "contents_titlepage",
                                 "Headings all fields" = "headings_all",
                                 "Frontmatter" = "contents_headings_frontmatter",
                                 "Backmatter" = "contents_headings_backmatter"),
                  selected = "contents_headings_all"
                   ),
      sliderInput("api_min_hits",
                  "Min API hits:",
                  min = 1,
                  max = 50,
                  value = 1),
      sliderInput("range_years",
                  "Years:",
                  min = 1700,
                  max = 1799,
                  value = c(1700, 1799),
                  step = 5),
      selectInput("publication_place",
                  "Publication place:",
                  choices = list("All",
                                 "London",
                                 "Edinburgh",
                                 "Dublin",
                                 "Boston Ma",
                                 "Philadelphia Pa",
                                 "Oxford",
                                 "Glasgow",
                                 "New York NY",
                                 "Cambridge",
                                 "Newcastle"),
                  selected = "All"),
      selectInput("language",
                  "Language:",
                  choices = list("any",
                                 "English",
                                 "Latin",
                                 "French",
                                 "German",
                                 "Welsh"),
                  selected = "any"),
      selectInput("time_window",
                  "Time segment (years):",
                  choices = list(20,
                                 10,
                                 5,
                                 1),
                  selected = 10),
      selectInput("document_type",
                  "Document type:",
                  choices = list("All", 
                                 "Books",
                                 "Pamphlets"),
                  selected = "All"),
      # submitButton("Update View",
      #              icon("refresh"))
      actionButton(inputId = "submit_button", 
                   label = "Update View",
                   icon = icon("refresh"))
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      # textOutput("sanity_check"),
      tabsetPanel(
        tabPanel("Summary",
                 h3("Search summary",
                    class = "text-center"),
                 tableOutput("summary_table"),
                 textOutput("intro_text"),
                 textInput(inputId = "ids_identifier", label = "", placeholder = "input idfile name"),
                 actionButton(inputId = "save_button", 
                              label = "Save subcorpus",
                              icon = icon("refresh"))
                 # for styling: <div> id = summary_table
                 ),
        tabPanel("Books/Pamphlets",
                 h3("Paper consumption: books vs. pamphlets",
                    class = "text-center"),
                 plotOutput("books_vs_pamphlets_plot")
                 ),
        tabPanel("Author titlecount",
                 h3("Title count timeline for top 10 authors",
                    class = "text-center"),
                 plotOutput("title_count_top_10_authors_plot")
                 ),
        tabPanel("Top places",
                 # textOutput("top_places_description"),
                 h3("Top places", class = "text-center"),
                 p("The top places defined by title count and relation between
                   titlecount and query hits. Note that document lengths are 
                   not taken into account here."),
                 plotOutput("top_places_by_titlecount_plot"),
                 plotOutput("top_places_titlecount_and_query_hits_plot")
                 ),
        tabPanel("Top publishers",
                 h3("Top publishers", class = "text-center"),
                 p("The top publishers can be defined by title count or by the
                   total hits across all documents."),
                 plotOutput("top_publishers_plot"),
                 plotOutput("top_publishers_abs_hits_plot")
                 ),
        tabPanel("Top hits per title",
                 h3("Top hits per title", class = "text-center"),
                 p("The title length is limited up to 40 first characters."),
                 plotOutput("top_titlehits_plot"),
                 plotOutput("top_titlehits_edition_plot"),
                 plotOutput("top_titlehits_edition_by_edition_plot")
                 ),
        tabPanel("Top titles by titlecount",
                 h3("Top titles by titlecount", class = "text-center"),
                 plotOutput("top_titles_by_title_count_plot")
        ),
        tabPanel("Top authors",
                 h3("Top authors", class = "text-center"),
                 p("The top authors by title count, total hits across all
                   editions and by hits per edition."),
                 plotOutput("top_authors_total_hits_plot"),
                 plotOutput("top_authors_hits_per_edition_plot"),
                 plotOutput("top_authors_hits_per_edition_arranged_plot")
        ),
        tabPanel("Timeline for titlecount",
                 h3("Timeline for titlecount", class = "text-center"),
                 p("Timeline for titlecount and fraction of the ECCO query
                   compared to all selected documents (time, language). If
                   a specific place is selected, two bars are shown: one for
                   the selected place and one for all places."),
                 plotOutput("titlecount_timeline_plot"),
                 plotOutput("relative_titlecount_plot")
        ),
        tabPanel("Timeline for paper consumption",
                 h3("Timeline for paper consumption", class = "text-center"),
                 p("Timeline for paper consumption and fraction of the ECCO
                   query compared to all selected documents (time, language).
                   If a specific place is selected, two bars are shown: one for
                   the selected place and one for all places."),
                 plotOutput("paper_consumption_plot"),
                 plotOutput("relative_paper_consumption_plot")
        )
      )
    )
  )
))
