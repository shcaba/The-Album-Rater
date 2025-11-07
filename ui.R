library(shiny)
library(bslib)
library(DT)
library(readr)
library(plotly)

fluidPage(
  column(width = 12, class = "well",
titlePanel("The Album Rater"),
h4(p("This tool ranks albums from your individual song ratings.")),
h4(p("This is an objective ranking of albums taken from your subjective song ratings.")),
h4(p("This tool ranks albums from your individual song ratings.")),

sidebarLayout(
  sidebarPanel(
    h3("Choose file with song ratings."),
    fileInput("file", "CSV File",
              accept = c(".csv", ".CSV")),
    width=2,
    br(),
    h3("Choose the ranking weights"),
    fluidRow(
      column(width = 6, numericInput("wtMedian", "Median score", value = 0.4, min = 0, max = 1, step = 0.001))
    ),
    fluidRow(
      column(width = 6, numericInput("wttens", "Number of 10s",  value = 0.1, min = 0, max = 1, step = 0.001)),
      column(width = 6, numericInput("wt8plus", "Number >= 8",  value = 0.1, min = 0, max = 1, step = 0.00001))
    ),
    fluidRow(
      column(width = 6, numericInput("wtper10", "% 10s",  value = 0.2, min = 0, max = 1, step = 0.001)),
      column(width = 6, numericInput("wtper8plus", "% >=8",  value = 0.2, min = 0, max = 1, step = 0.00001))
    ),
    
    br(),

    h3("Choose the number of album clusters to show on plot"),
    h4("(Should be less than total number of albums, and usually 5 or less)"),
    numericInput("clust.in", "Number of clusters", value = 1, min = 1, max = 1000, step = 1),

    actionButton("run_rankings", strong("Run Rankings"),
                 width = "100%",
                 icon("circle-play"),
                 style = "font-size:120%;border:2px solid;color:#FFFFFF;background:#5D9741"
    )
  ),

  mainPanel(
    tabsetPanel(
      id = "tabs",    
      
      tabPanel("Album rankings",
#        h4("Album rankings. These are sortable by metric."),
        tableOutput("ranking_table"),
        downloadButton('download_table',"Download the album rankings"),
        br(),
        br(),
        tableOutput("artist_table"),
        downloadButton('download_artist_table',"Download the artist metrics"),
        br(),
        br(),
        h4("Final ranking compared to the ranking metric."),
        h5("Color indicate the clusterings based on each ranking metric, not the overall final ranking"),
        plotlyOutput("Comp_rank_plot")
      ),
  
      tabPanel("Between scorer comparisons",
      )
    ),
     width=10
   )
  )
 )
)
