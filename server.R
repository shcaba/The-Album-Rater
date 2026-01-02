require(ggplot2)
require(viridis)
require(Kmedians)
require(tidyverse)
require(gt)
require(shinycssloaders)
require(wesanderson)
require(shinybusy)
require(shinyjs)
require(reshape2)

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
  
  
  observeEvent(input$file,{
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
  

  albums.list.out<-reactive({
    req(data())
    #browser()
    #albums.dat=data.frame(data())
    albums.dat=data.frame(t(data()))
    #Prep data
    albums.list<-list()
    albums.list[[1]]<-albums.dat[1,c(-1,-2)]
    albums.list[[2]]<-albums.dat[2,c(-1,-2)]
    #albums.list[[1]]<-albums.dat[1,c(-1)]
    #albums.list[[2]]<-albums.dat[2,c(-1)]
    albums.list[[3]]<-as.data.frame(sapply(albums.dat[4:nrow(albums.dat),-1],as.numeric)) #Turn characters into numeric
    albums.list[[4]]<-albums.dat[3,c(-1,-2)]
    
    #rownames(albums.list[[3]])<-albums.dat[4:nrow(albums.dat),1]
    
    tracknums<-c(1:dim(albums.list[[3]])[1])
    #tracknums<-albums.list[[3]][,1]
    albums.list[[3]]<-albums.list[[3]][,-1]
    rownames(albums.list[[3]])<-tracknums
    
    names(albums.list)<-c("Artist","Album","Tracks","Years")
    albums.list
  })
  
  #Calculate ranking scores and rankings
  all.metrics.out<-reactive({
    req(albums.list.out())
    albums.list<-albums.list.out()
    #browser()
    #Caluculate scores
    means<-round(apply(albums.list$Tracks,2,mean,na.rm=TRUE),2)
    tens<-colSums(albums.list$Tracks==10,na.rm=TRUE)
    eigh2ten<-colSums(albums.list$Tracks>=8,na.rm=TRUE)
    numtracks<-colSums(albums.list$Tracks>0,na.rm=TRUE)
    tens_per<-round(tens/numtracks,2)
    eigh2ten_per<-round(eigh2ten/numtracks,2)
    all.metrics<-rbind(means,tens,eigh2ten,tens_per,eigh2ten_per)
    rownames(all.metrics)<-c("Means","Tens","8plus","Tens_pct","8plus_pct")
    #colnames(all.metrics)<-albums.list$Album
    colnames(all.metrics)<-albums.list$Album
    all.metrics
  })    
  
    allranks.out<-reactive({
    req(albums.list.out(),all.metrics.out())

    albums.list<-albums.list.out()
#    #Calculate scores
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
    rank.table<-data.frame(Artist=t(albums.list$Artist),Album=t(albums.list$Album),Year=t(albums.list$Year),Final_rank=round(final.rank,2),Rank_score=rank.score,rank.table)
    colnames(rank.table)<-c("Artist","Album","Year","Final rank","Rank score","Mean rank","10s rank","8+ rank","% 10s rank","% 8+ rank","Mean","10s","8+","% 10s","% 8+")
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
      data_color(columns = c(4:15), method = "auto", palette = "viridis", reverse=TRUE) |>
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
    
    rank.wt<-c(input$wtMean_artist,input$wttens_artist,input$wt8plus_artist,input$wtper10_artist,input$wtper8plus_artist)
    rank.wt<-rank.wt/sum(rank.wt)
    #negative values used to rank the highest value as highest rank
    artist.rank<-rank(rowSums(t(t(cbind(rank(-summary.out$Mean,ties.method= "min"),
      rank(-summary.out$N_Tens,ties.method= "min"),
      rank(-summary.out$N_8plus,ties.method= "min"),
      rank(-summary.out$Pct_Tens,ties.method= "min"),
      rank(-summary.out$Pct_8plus,ties.method= "min")))*rank.wt)),ties.method= "min")   
    
    summary.out<-cbind(summary.out[,1],artist.rank,rank.out[,2],summary.out[,2:ncol(summary.out)])
    colnames(summary.out)[c(2,3)]<-c("Overall rank","Median rank")
    
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
  
  
  years.summary.out<-reactive({
    req(albums.list.out(),all.metrics.out())
    albums.list<-albums.list.out()
    all.metrics<-all.metrics.out()
    
    ##Numbers
    #Make initial numbers data frames for all year entries
    all.metrics.10<-data.frame("Year"=as.numeric(albums.list$Years),"Number"=as.numeric(all.metrics[2,]))
    all.metrics.8<-data.frame("Year"=as.numeric(albums.list$Years),"Number"=as.numeric(all.metrics[3,]))
    #all.metrics.10.8<-rbind(all.metrics.10,all.metrics.8)
    
    #Sum across all year entries to get a total for each year and metric
    Year.numbers.10<-reshape2::dcast(all.metrics.10,Year~1,sum)
    Year.numbers.8<-reshape2::dcast(all.metrics.8,Year~1,sum)
    #Prepare for plot
    Year.numbers.10.df<-data.frame("Year"=as.numeric(Year.numbers.10[,1]),"Number"=as.numeric(Year.numbers.10[,2]),Metric="10s")
    Year.numbers.8.df<-data.frame("Year"=as.numeric(Year.numbers.8[,1]),"Number"=as.numeric(Year.numbers.8[,2]),Metric="8+")
    Year.numbers.10.8<-rbind(Year.numbers.10.df,Year.numbers.8.df)
    
    ##Percent
    #Make initial percent data frames for all year entries
    all.metrics.10per<-data.frame("Year"=as.numeric(albums.list$Years),"Percent"=as.numeric(all.metrics[4,]))
    all.metrics.8per<-data.frame("Year"=as.numeric(albums.list$Years),"Percent"=as.numeric(all.metrics[5,]))
    #all.metrics.10.8<-rbind(all.metrics.10,all.metrics.8)
    
    #Sum across all year entries to summary a total for each year and metric
    Year.percent.10<-reshape2::dcast(all.metrics.10per,Year~1,sum)
    Year.percent.8<-reshape2::dcast(all.metrics.8per,Year~1,sum)
    #Prepare for plot
    Year.percent.10.df<-data.frame("Year"=as.numeric(Year.percent.10[,1]),"Percent"=as.numeric(Year.percent.10[,2]),Metric="%10s")
    Year.percent.8.df<-data.frame("Year"=as.numeric(Year.percent.8[,1]),"Percent"=as.numeric(Year.percent.8[,2]),Metric="%8+")
    Year.percent.10.8<-rbind(Year.percent.10.df,Year.percent.8.df)
    #browser()
    #Total albums and songs per year
    years.albums<-table(as.numeric(albums.list$Years))
    years.tracks<-mapply(function(x) sum(table(albums.list$Tracks[,x])),x=1:ncol(albums.list$Tracks),SIMPLIFY=TRUE)
    years.tracks<-data.frame(Year=as.numeric(albums.list$Years),Tracks=as.numeric(years.tracks))
    years.tracks<-dcast(years.tracks,Year~1,sum)
    years.albums.tracks<-data.frame(Year=as.numeric(names(years.albums)),Num_albums=as.numeric(years.albums),Num_tracks=as.numeric(years.tracks[,2]))
    
    Years.info.ls<-list()
    Years.info.ls[[1]]<-Year.numbers.10.8
    Years.info.ls[[2]]<-Year.percent.10.8
    Years.info.ls[[3]]<-years.albums.tracks
    Years.info.ls  
  })
  
  output$years_summary_plot<-renderPlotly({
    req(years.summary.out())
    years.summary.out<-years.summary.out()
    req(albums.list.out(),all.metrics.out())

    fill.in<-wes_palette(2,name="Moonrise2",type="discrete")
    
    p<-ggplot(years.summary.out[[1]],aes(Year,Number,fill=Metric))+
      geom_col()+
#      geom_text(aes(label = years.summary.out[[1]]$Num_tracks), vjust = -0.5)+
      scale_fill_manual(values=fill.in)+
      theme_bw()
      
    ggplotly(p)
  })

  output$years_summary_per_plot<-renderPlotly({
    req(years.summary.out())
    years.summary.out<-years.summary.out()

    fill.in<-wes_palette(2,name="FrenchDispatch",type="discrete")
    
    p<-ggplot(years.summary.out[[2]],aes(Year,Percent,fill=Metric))+
      geom_col()+
      scale_fill_manual(values=fill.in)+
      theme_bw()
    
    ggplotly(p)
  })
  
      
  observeEvent(input$run_rankings,{
  # Render cluster plot
  output$Comp_rank_plot <- renderPlotly({
    req(allranks.out(),albums.list.out(),rank_score.out(),finalranks.out())
    albums.list<-albums.list.out()
    all.metrics<-all.metrics.out()
    all.ranks<-allranks.out()
    rank.score<-rank_score.out()
    final.rank<-finalranks.out()
    
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
  })
}
