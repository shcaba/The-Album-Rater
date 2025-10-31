library(ggplot2)
library(viridis)
library(Kmedians)
library(tidyverse)
library(gt)

function(input, output, session) {
  # Reactive value to store the uploaded data
  data <- reactive({
    req(input$file)
    #browser()
    tryCatch({
      df <- data.frame(read_csv(input$file$datapath,col_names = FALSE))
      return(df)
    }, error = function(e) {
      showNotification("Error reading CSV file", type = "error")
      return(NULL)
    })
  })
  
  # Update variable choices when data is loaded
#  observe({
#    req(data())
#    numeric_vars <- names(select_if(data(), is.numeric))
#    all_vars <- names(data())
    
#    updateSelectInput(session, "x_var", choices = all_vars)
#    updateSelectInput(session, "y_var", choices = numeric_vars)
#  })
  
  # Output to check if file is uploaded (for conditional panel)
  #output$fileUploaded <- reactive({
   # return(!is.null(input$file))
  #})
  #outputOptions(output, "fileUploaded", suspendWhenHidden = FALSE)
  
  albums.list.out<-reactive({
    req(data())
    albums.dat=data()
    #Prep data
    albums.list<-list()
    albums.list[[1]]<-albums.dat[1,-1]
    albums.list[[2]]<-albums.dat[2,-1]
    albums.list[[3]]<-as.data.frame(sapply(albums.dat[4:nrow(albums.dat),-1],as.numeric)) #Turn characters into numeric
    rownames(albums.list[[3]])<-albums.dat[4:nrow(albums.dat),1]
    names(albums.list)<-c("Artist","Album","Tracks")
    albums.list
  })
  
  #Calculate ranking scores and rankings
  allranks.out<-reactive({
    req(albums.list.out())
    albums.list<-albums.list.out()
    #Caluculate scores
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
    all.ranks
  })  
  
  rank_score.out<-reactive({
    req(allranks.out())
    rank.wt<-c(0.4,0.1,0.1,0.2,0.2)
    rank.score<-colSums(allranks.out()*rank.wt)
    rank.score
  })

    
  finalranks.out<-reactive({
    req(rank_score.out())
    rank.score<-rank_score.out()
    final.rank<-rank(rank.score,ties.method= "min")
    final.rank
  })
  
  
  
  rank.table<-reactive({
    req(allranks.out(),albums.list.out(),rank_score.out(),finalranks.out())
    albums.list<-albums.list.out()
    all.ranks<-allranks.out()
    rank.score<-rank_score.out()
    final.rank<-finalranks.out()
    
    rank.table<-data.frame(t(all.ranks))
    rank.table<-data.frame(Artist=t(albums.list$Artist),Album=t(albums.list$Album),Final_rank=final.rank,Rank_score=rank.score,rank.table)
    colnames(rank.table)<-c("Artist","Album","Final rank","Rank score","Median","10s","8+","% 10s","% 8+")
    rank.table%>%
      arrange(`Final rank`)
    rank.table
  })
  

  # Render data table
  output$ranking_table <- render_gt({
    req(allranks.out(),albums.list.out(),rank_score.out(),finalranks.out())
    #albums.list<-albums.list.out()
    #all.ranks<-allranks.out()
    #rank.score<-rank_score.out()
    #final.rank<-finalranks.out()
    
    #rank.table<-data.frame(t(all.ranks))
    #rank.table<-data.frame(Artist=t(albums.list$Artist),Album=t(albums.list$Album),Final_rank=final.rank,Rank_score=rank.score,rank.table)
    #colnames(rank.table)<-c("Artist","Album","Final rank","Rank score","Median","10s","8+","% 10s","% 8+")
    #rank.table()%>%
    #  arrange(`Final rank`)
    
    gt(rank.table()) %>%
      tab_header(
        title = "Album rankings and rankings metrics",
        subtitle = ""
      ) %>%
      data_color(columns = c(3:9), method = "auto", palette = "viridis", reverse=TRUE) |>
      tab_style(style = list(cell_text(weight = "bold")),
                locations = cells_body(columns = c(Album,`Final rank`))) %>%
      opt_interactive(use_search = TRUE,
                      use_highlight = TRUE,
                      use_page_size_select = TRUE)
  })
  
  output$download_table <- downloadHandler(
    filename = function(){"album_rankings.csv"}, 
    content = function(fname){
      write.csv(rank.table(), fname)
    })
  
  # Render plot
  output$Comp_rank_plot <- renderPlotly({
    #browser()
    req(allranks.out(),albums.list.out(),rank_score.out(),finalranks.out())
    albums.list<-albums.list.out()
    all.ranks<-allranks.out()
    rank.score<-rank_score.out()
    final.rank<-finalranks.out()
    
    #Cluster analysis
    #browser()
    if(length(final.rank)>10)
    {
      kmeds<-Kmedians(t(all.ranks),nclust=1:length(final.rank)-1)
      cluster.col<-viridis(max(kmeds$bestresult$cluster))
      plot.cols<-mapply(function(x) cluster.col[kmeds$bestresult$cluster[x]],x=1:length(kmeds$bestresult$cluster))
    }
    else{plot.cols<-"blue"}
    
    rank.plot.dat<-data.frame("Final rank"=final.rank,"Rank score"=rank.score,Ptcol=plot.cols)
    rownames(rank.plot.dat)<-albums.list$Album
    max.ax<-max(c(rank.plot.dat$Final.rank,rank.plot.dat$Rank.score))
    
    ggplot(rank.plot.dat,aes(Final.rank,Rank.score,col=Ptcol))+
      geom_point(size=4)+
      ylim(0,max.ax)+
      xlim(0,max.ax)+
      geom_abline(slope=1,intercept = 0)+
      theme_bw()+
      xlab("Final rank")+
      ylab("Rank score")+
      theme(legend.position = "none")
  })
}
