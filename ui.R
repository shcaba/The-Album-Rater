require(shiny)
require(bslib)
require(DT)
require(readr)
require(plotly)
require(shinyWidgets)
require(shinycssloaders)

ui <- function(request) {
  shinyUI(fluidPage(
  tags$style(HTML(".dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing, .dataTables_wrapper .dataTables_paginate, .dataTables_wrapper .dataTables_paginate .paginate_button.current:hover {
                    color: #ffffff")),
  
  setBackgroundImage(src = "album"),
  #setBackgroundImage(src = "https://hediscount.ru/product/276214062134"), 
  
  column(width = 12, class = "well",
  titlePanel("The Album Rater"),
  h4(p("This tool ranks albums from your individual song ratings.")),
  h4(p("This is an objective album ranking taken from subjective song ratings.")),
  downloadLink(
    "download_csv",
    "Download example ratings file",
    class = "btn btn-secondary mb-3"
  ),
  
sidebarLayout(
  sidebarPanel(
    width=2,
    h3("Choose file with song ratings."),
    
    fileInput("file", "CSV File",
              accept = c(".csv", ".CSV")),
    
    h3("Choose the ranking weights"),
    fluidRow(
      column(width = 6, numericInput("wtMean", "Mean score", value = 0.5, min = 0, max = 1, step = 0.001))
    ),
    fluidRow(
      column(width = 6, numericInput("wttens", "Number of 10s",  value = 0.1, min = 0, max = 1, step = 0.001)),
      column(width = 6, numericInput("wt8plus", "Number 8+",  value = 0.05, min = 0, max = 1, step = 0.00001))
    ),
    fluidRow(
      column(width = 6, numericInput("wtper10", "% 10s",  value = 0.2, min = 0, max = 1, step = 0.001)),
      column(width = 6, numericInput("wtper8plus", "% 8+",  value = 0.15, min = 0, max = 1, step = 0.00001))
    ),
    
    br(),

    h3("Choose the number of album clusters to show on plot"),
    h4("(Should be less than total number of albums, and usually 5 or less)"),
    numericInput("clust.in", "Number of clusters", value = 1, min = 1, max = 1000, step = 1),

    actionButton("run_rankings", strong("Run Rankings"),
                 width = "100%",
                 icon("circle-play"),
                 style = "font-size:120%;border:2px solid;color:#FFFFFF;background:#0388fc"
    )
  ),

  mainPanel(
    tabsetPanel(
      id = "tabs",    
      
      tabPanel("Album rankings",
#        h4("Album rankings. These are sortable by metric."),
        withSpinner(tableOutput("ranking_table"),image="spinning-record.gif",image.height = "100px",image.width = "100px"),
        downloadButton('download_table',"Download the album rankings"),
        br(),
        br(),
        withSpinner(tableOutput("artist_table"),image="spinning-record.gif",image.height = "100px",image.width = "100px"),
        downloadButton('download_artist_table',"Download the artist metrics"),
        br(),
        br(),
        h4("Final ranking compared to the ranking metric."),
        h5("Color indicate the clusterings based on each ranking metric, not the overall final ranking"),
        withSpinner(plotlyOutput("Comp_rank_plot"),image="zootrope.gif",image.height = "100px",image.width = "100px"),
      ),
  
      tabPanel("Between scorer comparisons",
      )
    ),
     width=10
   )
  )
 )
)
)
}