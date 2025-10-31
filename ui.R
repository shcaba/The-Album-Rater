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
    h3("Choose file with your song ratings by artist & ablum."),
    fileInput("file", "CSV File",
              accept = c(".csv", ".CSV")),
    width=2
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
