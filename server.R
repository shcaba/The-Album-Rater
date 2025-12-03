require(ggplot2)
require(viridis)
require(Kmedians)
require(tidyverse)
require(gt)
require(shinycssloaders)
#require(wesanderson)

function(input, output, session) {
  output$download_csv <- downloadHandler(
    filename = function() {
      "Example_rankings.csv"
    },
    content = function(file) {
      # Copy the existing file from app directory to download location
      file.copy("Ex_rankings.csv", file)
    }
  )  
  
  
  observeEvent(input$run_rankings,{
    # Reactive value to store the uploaded data
    data <- reactive({
    req(input$file)
    
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
  all.metrics.out<-reactive({
    req(albums.list.out())
    albums.list<-albums.list.out()
    
    #Caluculate scores
    means<-round(apply(albums.list$Tracks,2,mean,na.rm=TRUE),2)
    tens<-colSums(albums.list$Tracks==10,na.rm=TRUE)
    eigh2ten<-colSums(albums.list$Tracks>=8,na.rm=TRUE)
    numtracks<-colSums(albums.list$Tracks>0,na.rm=TRUE)
    tens_per<-round(tens/numtracks,2)
    eigh2ten_per<-round(eigh2ten/numtracks,2)
    all.metrics<-rbind(means,tens,eigh2ten,tens_per,eigh2ten_per)
    rownames(all.metrics)<-c("Means","Tens","8plus","Tens_pct","8plus_pct")
    colnames(all.metrics)<-albums.list$Album
    all.metrics
  })    
  
    allranks.out<-reactive({
    req(albums.list.out(),all.metrics.out())

    albums.list<-albums.list.out()
#    #Caluculate scores
#    medians<-apply(albums.list$Tracks,2,median,na.rm=TRUE)
#    tens<-colSums(albums.list$Tracks==10,na.rm=TRUE)
#    eigh2ten<-colSums(albums.list$Tracks>=8,na.rm=TRUE)
#    numtracks<-colSums(albums.list$Tracks>0,na.rm=TRUE)
#    tens_per<-tens/numtracks
#    eigh2ten_per<-eigh2ten/numtracks
    
    
    #Calcualte ranks
#    medians.rank<-rank(-medians,ties.method= "min")
#    tens.rank<-rank(-tens,ties.method= "min")
#    eigh2ten.rank<-rank(-eigh2ten,ties.method= "min")
#    tens_per.rank<-rank(-tens_per,ties.method= "min")
#    eigh2ten_per.rank<-rank(-eigh2ten_per,ties.method= "min")
    means.rank<-rank(-all.metrics.out()[1,],ties.method= "min")
    tens.rank<-rank(-all.metrics.out()[2,],ties.method= "min")
    eigh2ten.rank<-rank(-all.metrics.out()[3,],ties.method= "min")
    tens_per.rank<-rank(-all.metrics.out()[4,],ties.method= "min")
    eigh2ten_per.rank<-rank(-all.metrics.out()[5,],ties.method= "min")
    
    all.ranks<-rbind(means.rank,tens.rank,eigh2ten.rank,tens_per.rank,eigh2ten_per.rank)
    colnames(all.ranks)<-albums.list$Album
    all.ranks
  })  
  
  rank_score.out<-reactive({
    req(allranks.out())
    rank.wt<-c(input$wtMean,input$wttens,input$wt8plus,input$wtper10,input$wtper8plus)
    rank.wt<-rank.wt/sum(rank.wt)
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
    all.metrics<-all.metrics.out()
    all.ranks<-allranks.out()
    rank.score<-rank_score.out()
    final.rank<-finalranks.out()
    
    rank.table<-data.frame(cbind(t(all.ranks),t(all.metrics)))
    rank.table<-data.frame(Artist=t(albums.list$Artist),Album=t(albums.list$Album),Final_rank=final.rank,Rank_score=rank.score,rank.table)
    colnames(rank.table)<-c("Artist","Album","Final rank","Rank score","Mean rank","10s rank","8+ rank","% 10s rank","% 8+ rank","Mean","10s","8+","% 10s","% 8+")
    rank.table%>%
      arrange(`Final rank`)
    rank.table
  })
  
  # Render ranking table
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
    
    gt.rank.table<-gt(rank.table()) %>%
      tab_header(
        title = "Album rankings and rankings metrics",
        subtitle = ""
      ) %>%
      data_color(columns = c(3:14), method = "auto", palette = "viridis", reverse=TRUE) |>
      tab_style(style = list(cell_text(weight = "bold")),
                locations = cells_body(columns = c(Album,`Final rank`))) %>%
      opt_interactive(use_search = TRUE,
                      use_highlight = TRUE,
                      use_page_size_select = TRUE)
    gt.rank.table
  })
  
  output$download_table <- downloadHandler(
    filename = function(){"album_rankings.csv"}, 
    content = function(fname){
      write.csv(rank.table(), fname)
    })


#Create artist summary
  artist.summary.out<-reactive({
    req(albums.list.out())
    #browser()
    albums.list<-albums.list.out()
    score.out.list<-mapply(function(x) data.frame(Track_score=albums.list$Tracks[,x],Artist=albums.list$Artist[1,x]),x=1:length(albums.list$Artist),SIMPLIFY=FALSE)
    score.out.df<-do.call(rbind, score.out.list)
    #Get Artist summaries
    summary.out<-score.out.df %>%
      group_by(Artist) %>%
      summarize(Ntracks=sum(Track_score>0,na.rm=TRUE),N_Tens=sum(Track_score==10,na.rm=TRUE),Pct_Tens=round(sum(Track_score==10,na.rm=TRUE)/sum(Track_score>0,na.rm=TRUE),2),N_8plus=sum(Track_score>=8,na.rm=TRUE),Pct_8plus=round(sum(Track_score>=8,na.rm=TRUE)/sum(Track_score>0,na.rm=TRUE),2),Mean=round(mean(Track_score,na.rm=TRUE),2),Median=round(median(Track_score,na.rm=TRUE)),q5=quantile(Track_score,probs=0.05,na.rm=TRUE),q95=quantile(Track_score,probs=0.95,na.rm=TRUE))

    #Get median ranks
    rank.df<-data.frame(Artist=rank.table()$Artist,Rank=rank.table()$'Final rank')
    rank.out<-rank.df%>%
      group_by(Artist) %>%
      summarize(Median_rank=round(median(Rank,na.rm=TRUE),2))
    
    rank.wt<-c(input$wtMean,input$wttens,input$wt8plus,input$wtper10,input$wtper8plus)
    rank.wt<-rank.wt/sum(rank.wt)
    artist.rank<-rank(rowSums(cbind(rank(summary.out$Mean,ties.method= "min"),
      rank(summary.out$N_Tens,ties.method= "min"),
      rank(summary.out$N_8plus,ties.method= "min"),
      rank(summary.out$Pct_Tens,ties.method= "min"),
      rank(summary.out$Pct_8plus,ties.method= "min"))*rank.wt),ties.method= "min")   
    
    summary.out
  })

  # Render artist summary table
  output$artist_table <- render_gt({
    req(artist.summary.out())
    #albums.list<-albums.list.out()
    #all.ranks<-allranks.out()
    #rank.score<-rank_score.out()
    #final.rank<-finalranks.out()
    
    #rank.table<-data.frame(t(all.ranks))
    #rank.table<-data.frame(Artist=t(albums.list$Artist),Album=t(albums.list$Album),Final_rank=final.rank,Rank_score=rank.score,rank.table)
    #colnames(rank.table)<-c("Artist","Album","Final rank","Rank score","Median","10s","8+","% 10s","% 8+")
    #rank.table()%>%
    #  arrange(`Final rank`)
    
    gt(artist.summary.out()) %>%
      tab_header(
        title = "Artist summary",
        subtitle = ""
      ) %>%
      data_color(columns = c(2:ncol(artist.summary.out())), method = "auto", palette = "viridis", reverse=TRUE) |>
      tab_style(style = list(cell_text(weight = "bold")),
                locations = cells_body(columns = c(Artist,Ntracks))) %>%
      opt_interactive(use_search = TRUE,
                      use_highlight = TRUE,
                      use_page_size_select = TRUE)
  })
  
  output$download_artist_table <- downloadHandler(
    filename = function(){"artist_rankings.csv"}, 
    content = function(fname){
      write.csv(rank.table(), fname)
    })
  
    
  # Render plot
  output$Comp_rank_plot <- renderPlotly({
    req(allranks.out(),albums.list.out(),rank_score.out(),finalranks.out())
    albums.list<-albums.list.out()
    all.metrics<-all.metrics.out()
    all.ranks<-allranks.out()
    rank.score<-rank_score.out()
    final.rank<-finalranks.out()
    #browser()
    #Cluster analysis
   if(length(final.rank)>input$clust.in & input$clust.in>1)
    {
      
    #  kmeds<-Kmedians(t(all.metrics),nclust=1:length(final.rank)-1)
    #  cluster.col<-viridis(max(kmeds$bestresult$cluster))
    #  plot.cols<-mapply(function(x) cluster.col[kmeds$bestresult$cluster[x]],x=1:length(kmeds$bestresult$cluster))
     kmeds<-Kmedians(t(all.metrics[c(1,4,5),]),nclust=1:input$clust.in)
     cluster.col<-viridis(input$clust.in)
     plot.cols<-mapply(function(x) cluster.col[kmeds$allresults[[input$clust.in]]$cluster[x]],x=1:length(kmeds$allresults[[input$clust.in]]$cluster))
    }
    else{plot.cols<-"#0388fc"}
    
    rank.plot.dat<-data.frame("Album"=t(albums.list$Album),"Final rank"=final.rank,"Rank score"=rank.score,Ptcol=plot.cols)
    colnames(rank.plot.dat)[1]="Album"
    #rownames(rank.plot.dat)<-albums.list$Album
    max.ax<-max(c(rank.plot.dat$Final.rank,rank.plot.dat$Rank.score))

    p<-ggplot(rank.plot.dat,aes(x=Final.rank,y=Rank.score,fill=plot.cols,album=Album))+
      geom_point(size=4)+
      ylim(0,max.ax)+
      xlim(0,max.ax)+
      geom_abline(slope=1,intercept = 0)+
      theme_bw()+
      xlab("Final rank")+
      ylab("Rank score")+
      theme(legend.position = "none")
    
    style(p, text = names(final.rank))
    ggplotly(p,tooltip = c("x", "y", "album"))
    p
    })
  })
}
