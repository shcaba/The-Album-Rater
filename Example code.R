library(dplyr)
library(spotifyr)
library(plotly)
library(ggplot2)

#Set up the API client
#id <- ‘your client ID’
#secret <- ‘your client secret’
#Sys.setenv(SPOTIFY_CLIENT_ID = id)
#Sys.setenv(SPOTIFY_CLIENT_SECRET = secret)
access_token <- get_spotify_access_token()

### Example of getting artist ID, then album ID, then track listings ###
#Go to artist profile on Spotify.
#Click the 3 dots or right-click your artist name.
#Select Share.
#Select Copy link to artist.

#REM#
#https://open.spotify.com/artist/4KWTAlx2RvbpseOGMEmROg?si=FvQR88omR2eEB3AaI2R15w
#4KWTAlx2RvbpseOGMEmROg is the artist ID
#Get album IDs
xx<-get_artist_albums(id="4KWTAlx2RvbpseOGMEmROg",include_groups = c("album","compilation"),limit=50)
if(length(xx$id)>20)
{
  xx_albums.1<-get_albums(xx$id[1:20])
  xx_albums.2<-get_albums(xx$id[21:length(xx$id)])
  xx_albums<-rbind(xx_albums.1,xx_albums.2)
} else {xx_albums<-get_albums(xx$id)}

xx_tracks<-get_album_tracks(xx_albums$id[2],limit=50)
xx_tracks$name #This is the list that would be scored


#Read in pre-scored 
library(ggplot2)
library(viridis)
library(Kmedians)

albums.dat<-read.csv("C:/Users/copej/OneDrive/Desktop/Album rater/REM_example.csv",header=F)
albums.list<-list()
albums.list[[1]]<-albums.dat[1,-1]
albums.list[[2]]<-albums.dat[2,-1]
albums.list[[3]]<-as.data.frame(sapply(albums.dat[4:nrow(albums.dat),-1],as.numeric)) #Turn characters into numeric
rownames(albums.list[[3]])<-albums.dat[4:nrow(albums.dat),1]
names(albums.list)<-c("Artist","Album","Tracks")


medians<-apply(albums.list$Tracks,2,median,na.rm=TRUE)
tens<-colSums(albums.list$Tracks==10,na.rm=TRUE)
eigh2ten<-colSums(albums.list$Tracks>=8,na.rm=TRUE)
numtracks<-colSums(albums.list$Tracks>0,na.rm=TRUE)
tens_per<-tens/numtracks
eigh2ten_per<-eigh2ten/numtracks

#Calcualte ranks
medians.rank<-rank(-medians,ties.method= "min")
tens.rank<-rank(-tens,ties.method= "min")
eigh2ten.rank<-rank(-eigh2ten,ties.method= "min")
tens_per.rank<-rank(-tens_per,ties.method= "min")
eigh2ten_per.rank<-rank(-eigh2ten_per,ties.method= "min")
all.ranks<-rbind(medians.rank,tens.rank,eigh2ten.rank,tens_per.rank,eigh2ten_per.rank)
colnames(all.ranks)<-albums.list$Album

rank.wt<-c(0.4,0.1,0.1,0.2,0.2)
rank.score<-colSums(all.ranks*rank.wt)
final.rank<-rank(rank.score,ties.method= "min")

#Clusters
kmeds<-Kmedians(t(all.ranks))
cluster.col<-viridis(max(kmeds$bestresult$cluster))

#Make ggplot dataframe
rank.plot.dat<-data.frame("Final rank"=final.rank,"Rank score"=rank.score,Ptcol=mapply(function(x) cluster.col[kmeds$bestresult$cluster[x]],x=1:length(kmeds$bestresult$cluster)))
rownames(rank.plot.dat)<-albums.list$Album



ggplot(rank.plot.dat,aes(Final.rank,Rank.score,col=Ptcol))+
  geom_point(size=4)+
  ylim(0,NA)+
  xlim(0,NA)+
  geom_abline(slope=1,intercept = 0)+
  theme_bw()+
  xlab("Final rank")+
  ylab("Rank score")+
  theme(legend.position = "none")

#abbreviate(albums.list[[2]])




library(shiny)
library(bslib)
library(DT)
library(ggplot2)
library(readr)

ui <- page_sidebar(
  title = "CSV Data Explorer",
  sidebar = sidebar(
    fileInput("file", "Choose CSV File",
              accept = c(".csv", ".CSV")),
    
    conditionalPanel(
      condition = "output.fileUploaded",
      br(),
      selectInput("x_var", "X Variable:", choices = NULL),
      selectInput("y_var", "Y Variable:", choices = NULL),
      selectInput("plot_type", "Plot Type:",
                  choices = list("Scatter Plot" = "scatter",
                                 "Line Plot" = "line",
                                 "Bar Plot" = "bar",
                                 "Histogram" = "histogram"))
    )
  ),
  
  layout_columns(
    card(
      card_header("Data Table"),
      DT::dataTableOutput("table")
    ),
    
    card(
      card_header("Data Visualization"),
      plotOutput("plot", height = "400px")
    )
  )
)

server <- function(input, output, session) {
  # Reactive value to store the uploaded data
  data <- reactive({
    req(input$file)
    
    tryCatch({
      df <- read_csv(input$file$datapath)
      return(df)
    }, error = function(e) {
      showNotification("Error reading CSV file", type = "error")
      return(NULL)
    })
  })
  
  # Update variable choices when data is loaded
  observe({
    req(data())
    numeric_vars <- names(select_if(data(), is.numeric))
    all_vars <- names(data())
    
    updateSelectInput(session, "x_var", choices = all_vars)
    updateSelectInput(session, "y_var", choices = numeric_vars)
  })
  
  # Output to check if file is uploaded (for conditional panel)
  output$fileUploaded <- reactive({
    return(!is.null(input$file))
  })
  outputOptions(output, "fileUploaded", suspendWhenHidden = FALSE)
  
  # Render data table
  output$table <- DT::renderDataTable({
    req(data())
    DT::datatable(data(), 
                  options = list(scrollX = TRUE, pageLength = 10),
                  class = 'cell-border stripe')
  })
  
  # Render plot
  output$plot <- renderPlot({
    req(data(), input$x_var)
    
    df <- data()
    
    if (input$plot_type == "histogram") {
      # For histogram, only use x variable
      req(is.numeric(df[[input$x_var]]))
      ggplot(df, aes_string(x = input$x_var)) +
        geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
        theme_minimal() +
        labs(title = paste("Histogram of", input$x_var))
      
    } else {
      # For other plots, need both x and y variables
      req(input$y_var)
      
      if (input$plot_type == "scatter") {
        ggplot(df, aes_string(x = input$x_var, y = input$y_var)) +
          geom_point(alpha = 0.6, color = "steelblue") +
          theme_minimal() +
          labs(title = paste("Scatter Plot:", input$y_var, "vs", input$x_var))
        
      } else if (input$plot_type == "line") {
        ggplot(df, aes_string(x = input$x_var, y = input$y_var)) +
          geom_line(color = "steelblue", size = 1) +
          geom_point(color = "darkblue", alpha = 0.6) +
          theme_minimal() +
          labs(title = paste("Line Plot:", input$y_var, "vs", input$x_var))
        
      } else if (input$plot_type == "bar") {
        # For bar plot, aggregate data by x variable
        df_agg <- df %>%
          group_by(!!sym(input$x_var)) %>%
          summarise(value = mean(!!sym(input$y_var), na.rm = TRUE), .groups = 'drop')
        
        ggplot(df_agg, aes(x = !!sym(input$x_var), y = value)) +
          geom_col(fill = "steelblue", alpha = 0.7) +
          theme_minimal() +
          labs(title = paste("Bar Plot: Mean", input$y_var, "by", input$x_var),
               y = paste("Mean", input$y_var))
      }
    }
  })
}

shinyApp(ui = ui, server = server)
