require(shiny)
require(bslib)
require(DT)
require(readr)
require(plotly)
require(shinyWidgets)
require(shinycssloaders)

ui <- function(request) {
  shinyUI(fluidPage(
    tags$style(HTML(
      ".dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing, .dataTables_wrapper .dataTables_paginate, .dataTables_wrapper .dataTables_paginate .paginate_button.current:hover {
                    color: #ffffff"
    )),

    setBackgroundImage(src = "album"),
    #setBackgroundImage(src = "https://hediscount.ru/product/276214062134"),

    column(
      width = 12,
      class = "well",
      titlePanel("The Album Rater"),
      h4(p(
        "This tool ranks albums and artists from your individual song ratings."
      )),
      h4(p(
        "While your song rating is subjective, this is an objective album and artist ranking system."
      )),
      h4(p(
        "For an examplantion on how to rate songs, go to the ",
        tags$a(
          href = "https://github.com/shcaba/The-Album-Rater",
          "The Album Rater GitHub repo",
          target = "_blank"
        ),
        "and scroll down to the 'Rating songs' section."
      )),
      h4(p(
        "It also provides summary metrics for the number and percent of best songs in each year."
      )),
      downloadLink(
        "download_csv",
        "Download example ratings file",
        class = "btn btn-secondary mb-3"
      ),

      sidebarLayout(
        sidebarPanel(
          width = 2,
          h3("Upload album scores"),

          fileInput(
            "file",
            "CSV file with song ratings",
            accept = c(".csv", ".CSV")
          ),

          conditionalPanel(
            condition = "input.tabselected==1",
            h3("Choose album ranking weights"),
            fluidRow(
              column(
                width = 6,
                numericInput(
                  "wtMean",
                  "Mean score",
                  value = 0.5,
                  min = 0,
                  max = 1,
                  step = 0.001
                )
              )
            ),
            fluidRow(
              column(
                width = 6,
                numericInput(
                  "wttens",
                  "# of 10s",
                  value = 0.1,
                  min = 0,
                  max = 1,
                  step = 0.001
                )
              ),
              column(
                width = 6,
                numericInput(
                  "wt8plus",
                  "# 8+",
                  value = 0.05,
                  min = 0,
                  max = 1,
                  step = 0.00001
                )
              )
            ),
            fluidRow(
              column(
                width = 6,
                numericInput(
                  "wtper10",
                  "% 10s",
                  value = 0.2,
                  min = 0,
                  max = 1,
                  step = 0.001
                )
              ),
              column(
                width = 6,
                numericInput(
                  "wtper8plus",
                  "% 8+",
                  value = 0.15,
                  min = 0,
                  max = 1,
                  step = 0.00001
                )
              )
            ),
          ),

          conditionalPanel(
            condition = "input.tabselected==2",
            h3("Choose artist ranking weights"),
            fluidRow(
              column(
                width = 6,
                numericInput(
                  "wtMean_artist",
                  "Mean score",
                  value = 0.1,
                  min = 0,
                  max = 1,
                  step = 0.001
                )
              )
            ),
            fluidRow(
              column(
                width = 6,
                numericInput(
                  "wttens_artist",
                  "# of 10s",
                  value = 0.4,
                  min = 0,
                  max = 1,
                  step = 0.001
                )
              ),
              column(
                width = 6,
                numericInput(
                  "wt8plus_artist",
                  "# 8+",
                  value = 0.35,
                  min = 0,
                  max = 1,
                  step = 0.00001
                )
              )
            ),
            fluidRow(
              column(
                width = 6,
                numericInput(
                  "wtper10_artist",
                  "% 10s",
                  value = 0.1,
                  min = 0,
                  max = 1,
                  step = 0.001
                )
              ),
              column(
                width = 6,
                numericInput(
                  "wtper8plus_artist",
                  "% 8+",
                  value = 0.05,
                  min = 0,
                  max = 1,
                  step = 0.00001
                )
              )
            ),
          ),

          conditionalPanel(
            condition = "input.tabselected==3",
            h3("Choose the number of album clusters to show on plot"),
            h4(
              "(Should be less than total number of albums, and usually 5 or less)"
            ),
            numericInput(
              "clust.in",
              "Number of clusters",
              value = 1,
              min = 1,
              max = 1000,
              step = 1
            ),

            actionButton(
              "run_rankings",
              strong("Run Clusters"),
              width = "100%",
              icon("circle-play"),
              style = "font-size:120%;border:2px solid;color:#FFFFFF;background:#0388fc"
            )
          ),
        ),

        mainPanel(
          tabsetPanel(
            id = "tabselected",

            tabPanel(
              "Album rankings",
              value = 1,
              #        h4("Album rankings. These are sortable by metric."),
              withSpinner(
                tableOutput("ranking_table"),
                image = "spinning-record.gif",
                image.height = "100px",
                image.width = "100px"
              ),
              downloadButton('download_table', "Download the album rankings"),
            ),

            tabPanel(
              "Artist rankings",
              value = 2,
              h4(
                "Explore which artists have the most classics, best number of songs, and overall highest ranking based on user inputted weights."
              ),
              withSpinner(
                tableOutput("artist_table"),
                image = "spinning-record.gif",
                image.height = "100px",
                image.width = "100px"
              ),
              downloadButton(
                'download_artist_table',
                "Download the artist metrics"
              ),
            ),

            tabPanel(
              "Year summary",
              h4(
                "Explore which years have the most classic and great songs, as well as which years are the highest quality and have the most rated songs."
              ),
              h5(
                "Number above bars are the total tracks evaluate within the given year."
              ),
              withSpinner(
                plotlyOutput("years_summary_plot"),
                image = "spinning-record.gif",
                image.height = "100px",
                image.width = "100px"
              ),
              withSpinner(
                plotlyOutput("years_summary_per_plot"),
                image = "spinning-record.gif",
                image.height = "100px",
                image.width = "100px"
              ),
              downloadButton('download_year_info', "Download the year metrics"),
            ),

            tabPanel(
              "Cluster analysis",
              value = 3,
              h4("Form groups of albums with similar rankings."),
              h4(
                "Final ranking compared to the ranking metric. This comparison can reveal how closely scored albums are to each adjacent album."
              ),
              h5(
                "Color indicate the clusterings based on each ranking metric, not the overall final ranking"
              ),
              withSpinner(
                plotlyOutput("Comp_rank_plot"),
                image = "zootrope.gif",
                image.height = "100px",
                image.width = "100px"
              ),
            ),

            tabPanel("Between scorer comparisons", h4("Coming soon"), )
          ),
          width = 10
        )
      )
    )
  ))
}
