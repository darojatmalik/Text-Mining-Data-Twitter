library(shiny)
library(shinydashboard)
ptm <- proc.time()
library(twitteR)
library(tm)
library(NLP)
library(stringr)
source('stemm.R')
source('singkatan.R')
library(fpc)
library(base)
library(stats)
library(dplyr)
library(dplyr)
library(fpc)
library(cluster)
library(leaflet)
library(scales)
library(lattice)
library(htmltools)

function(input, output, session) {
  datatwitter1 <- globalenv()
  datatwitter2 <- globalenv()
  baru1 <- globalenv()
  analisis <- globalenv()
  tdm_total <- globalenv()
 
#-----------------------------------------------------Data Explorer------------------------------------------#
  observeEvent(input$input_action,{
    infile <- input$datafile
    datatwitter <- as.data.frame(read.csv(infile$datapath,header=TRUE,sep=","))
    #Praproses Data
    praproses <- function(datatwitter1){
     
      #membuat korpus pada datatwitter
      korpus <- Corpus(VectorSource(datatwitter$text))
      
      #membuat remove URL tm karakter
      hapusURL <- function(x) gsub("(http)[[:graph:]]*", "", x)
      korpus <- tm_map(korpus, content_transformer(hapusURL))
      enter <- function(x) gsub("\n", " ", x)
      korpus <- tm_map(korpus, content_transformer(enter))
      karakter1 <- function(x) gsub("\\S*(\\S)\\1\\1\\S*\\s?", "", x)  
      korpus <- tm_map(korpus, content_transformer(karakter1))
      karakter2 <- function(x) gsub("(@)[[:graph:]]+", "", x)
      korpus <- tm_map(korpus, content_transformer(karakter2))
      karakter4 <- function(x) gsub("&lt;-"," ", x)
      korpus <- tm_map(korpus, content_transformer(karakter4))
      karakter5 <- function(x) gsub("[[:punct:]]", " ", x)
      korpus <- tm_map(korpus, content_transformer(karakter5))
      karakter6 <- function(x) gsub("[[:digit:]]", " ", x)
      korpus <- tm_map(korpus, content_transformer(karakter6))
      
      #membuat korpus dengan library TM
      korpus <- tm_map(korpus, content_transformer(tolower))
      korpus <- tm_map(korpus, stripWhitespace)
      korpus <- tm_map(korpus, removeNumbers)
      korpus <- tm_map(korpus, removePunctuation)
      korpus <- tm_map(korpus, stripWhitespace)
      
      #membuat data frame
      dataframe<-data.frame(text=unlist(sapply(korpus, "content")))
      splitgue<-strsplit(as.character(dataframe$text), split = " ")
      unlistgue<-unlist(splitgue)
      data1 <- data.frame(unlistgue)
      
      #normalisasi dengan mengubah singkatan
      nbaris <- length(splitgue)
      for (i in 1:nbaris) {
        nkolom <- length(splitgue[i][[1]])
        if(nkolom != 0)
        {
          nkolom <- length(splitgue[i][[1]])
          for (j in 1:nkolom) {
            term <- splitgue[i][[1]][[j]]
            norm <- cari.singkatan(term)
            splitgue[i][[1]][[j]] <- norm
          }
        }
      }
      Length <- sapply(splitgue, length)
      max.length <-max(sapply(splitgue,length))
      
      #hasil normalisasi
      hasilnormal<-lapply(splitgue,function(v) {c (v, rep("",max.length-length(v)))})
      hasilnormal<-do.call(rbind,hasilnormal)
      hasilnormal<-data.frame(hasilnormal)
      print(hasilnormal)
      
      #menghapus stopwords
      dataframe1 <-data.frame(text=unlist(sapply(korpus, "content")))
      newdat <- data.frame(text=str_trim(do.call(paste, hasilnormal)), stringsAsFactors=FALSE)
      baru<-data.frame(newdat)
      names(baru)[names(baru)=="hasilnormalbaru"] <- "text"
      korpus1 <- Corpus(VectorSource(baru$text))
      korpus1 <- tm_map(korpus1, stripWhitespace)
      stopword<- read.csv("stopword.csv")
      stopword <- tolower(stopword[, 1])
      stopword <- c(stopword, "", "a", "-", "rt", "...", "&", "|", "ada")
      #stopwords
      korpus1=tm_map(korpus1,removeWords,stopword)
      korpus1 <- tm_map(korpus1, stripWhitespace)
      korpus1 <- tm_map(korpus1, removeNumbers)
      korpus1 <- tm_map(korpus1, removePunctuation)
      korpus1 <- tm_map(korpus1, content_transformer(tolower))
      korpus1 <- tm_map(korpus1, stripWhitespace)
      
      #membuat dataframe untuk stemming
      dataframe2<-data.frame(text=unlist(sapply(korpus1, "content")))
      splitgue1<-strsplit(as.character(dataframe2$text), split = " ")
      unlistgue1<-unlist(splitgue1)
      data2 <- data.frame(unlistgue1)
      
      #Stemming
      nbaris1 <- length(splitgue1)
      for (m in 1:nbaris1) {
        nkolom1 <- length(splitgue1[m][[1]])
          if(nkolom1 != 0)
          {
            for (n in 1:nkolom1) {
              #print(n)
              term_stem <- splitgue1[m][[1]][[n]]
              #print(term_stem)
              norm_stem <- stemming(term_stem)
              #print(norm_stem)
              splitgue1[m][[1]][[n]] <- norm_stem
            }
          }
        
      }
      Length <- sapply(splitgue1, length)
      max.length <-max(sapply(splitgue1,length))
      
      #hasil stemming
      hasilnormal1<-lapply(splitgue1,function(v) {c (v, rep("",max.length-length(v)))})
      hasilnormal1<-do.call(rbind,hasilnormal1)
      hasilnormal1<-data.frame(hasilnormal1)
      print(hasilnormal1)
      
      #pembuatan dataframe untuk Term Document Matrix
      library(stringr)
      newdat1 <- data.frame(text=str_trim(do.call(paste, hasilnormal1)), stringsAsFactors=FALSE)
      baru1<<-data.frame(newdat1)
      # names(baru1)[names(baru1)=="hasilnormalbaru"] <- "text"
      
      #save to temp folder
      # saveRDS(baru1, file="temp/baru1.rds")
     
      #marge data result of cluster to others datatwitter
      korpus_username <- Corpus(VectorSource(datatwitter$username))
      df_username<-data.frame(username=unlist(sapply(korpus_username, "content")))
      korpus_date <- Corpus(VectorSource(datatwitter$date))
      df_date<-data.frame(date=unlist(sapply(korpus_date, "content")))
      korpus_time <- Corpus(VectorSource(datatwitter$time))
      df_time<-data.frame(time=unlist(sapply(korpus_time, "content")))
      korpus_pusatkota <- Corpus(VectorSource(datatwitter$pusatkota))
      df_pusatkota<-data.frame(pusatkota=unlist(sapply(korpus_pusatkota, "content")))
      korpus_lokasi <- Corpus(VectorSource(datatwitter$lokasi))
      df_lokasi<-data.frame(lokasi=unlist(sapply(korpus_lokasi, "content")))
      korpus_longitude <- Corpus(VectorSource(datatwitter$longitude))
      df_longitude<-data.frame(longitude=unlist(sapply(korpus_longitude, "content")))
      korpus_latitude <- Corpus(VectorSource(datatwitter$latitude))
      df_latitude<-data.frame(latitude=unlist(sapply(korpus_latitude, "content")))
      df_total<-cbind(df_username,df_date, df_time, newdat1, df_pusatkota, df_lokasi, df_longitude, df_latitude)
      # datatwitter1 <<- df_total
      return(df_total)
      # print(datatwitter1)
    }
    
    
    #ProgressBar
    progress <- shiny::Progress$new(session, min=1, max=10)
    on.exit(progress$close())
    progress$set(message = 'Praproses in progress')
    
    for (i in 1:5) {
      progress$set(value = i)
      Sys.sleep(1)
    }
    #Save to global variabel
    datatwitter1 <<- praproses(datatwitter1)
    print(datatwitter1)
    
    #dataExplorer  
    #Output Table Praproses
    output$td <- renderDataTable({
      datatwitter1
    })

    #download Data
    output$downloadData <- downloadHandler(
      filename = function() { 
        paste("Hasil Praproses", '.csv', sep='') 
      },
      content = function(file) {
        progress <- shiny::Progress$new(session, min=1, max=5)
        on.exit(progress$close())
        progress$set(message = 'Downloading in progress')
        
        for (i in 1:5) {
          progress$set(value = i)
          Sys.sleep(1)
        }
        
        #write.csv(praproses(),file) -----------for cloud server-----------
        write.csv(datatwitter1,file)
      }
    )
  })
  
  #------------------------------Clustering With K-Means----------------------------------------------------#
  observeEvent(input$input_clustering,{
    #load data from praproses
    datatwitter <- datatwitter1
    baru1 <- baru1 %>%
      select(
       text = text
      )
    
    #fungsi K-Means
    praproses <- function(datatwitter2){
    #TDM
    baru1
    v <- Corpus(VectorSource(baru1$text))
    tdm <- TermDocumentMatrix(v)
    
    #tdm to matrix
    m <- as.matrix(tdm)
    #mereduksi terms dengan remove sparse term 
    s <- input$numberofsparse
    tdms <- removeSparseTerms(tdm, s)
    inspect(tdms)
    m1 <- as.matrix(tdms)

    #binding tdm before and after
    tdm_before <- dim(m)
    df_m<-as.data.frame(tdm_before)
    tdm_after <- dim(m1)
    df_m1<-as.data.frame(tdm_after)
    df_tdm <- cbind(df_m, df_m1)
    tdm_total <<- df_tdm
    
    #Clustering dengan K-Means
    library(fpc)
    #Tranpose Matrix m1 ke m2
    m2 <- t(m1)

    #count SSE
    s <- 2
    finish <- input$numberofk
    for(start in s : finish){
      set.seed(1000)
      hasil <- kmeans(m2, start)
      centers <- hasil$centers[hasil$cluster,,drop=FALSE]
      jarak <- sqrt((m2 - centers)^2)
      total <- sum(jarak)
      persen <- 100*(hasil$betweenss/hasil$totss)
      gab<- data.frame(start, total, hasil$tot.withinss, persen)
      if(start == 2) {
        sse <- gab
        sse <- as.matrix(sse)
      }
      else{
        sse <- rbind(sse, gab)
      }
    }
    names(sse) <- c("kelas", "sse", "tot.within", "persen")
    
    
    #program Bar K-Means
    progress <- shiny::Progress$new(session, min=1, max=5)
    on.exit(progress$close())
    progress$set(message = 'K-Means in progress')
    
    for (i in 1:5) {
      progress$set(value = i)
      Sys.sleep(1)
    }
    #write result of SSE
    analisis <<- sse
    print(analisis)
    
    # set a fixed random seed
    set.seed(122)
    #K-Means dengan memasukkan input kelas
    k <- input$numberofk
    kmeansResult <- kmeans(m2, k)
    round(kmeansResult$centers, digits=3)
    
    #marge data hasil cluster, hasil praproses data teks Twitter dengan data Twitter
    korpus_username <- Corpus(VectorSource(datatwitter$username))
    df_username<-data.frame(username=unlist(sapply(korpus_username, "content")))
    korpus_date <- Corpus(VectorSource(datatwitter$date))
    df_date<-data.frame(date=unlist(sapply(korpus_date, "content")))
    korpus_time <- Corpus(VectorSource(datatwitter$time))
    df_time<-data.frame(time=unlist(sapply(korpus_time, "content")))
    korpus_pusatkota <- Corpus(VectorSource(datatwitter$pusatkota))
    df_pusatkota<-data.frame(pusatkota=unlist(sapply(korpus_pusatkota, "content")))
    korpus_lokasi <- Corpus(VectorSource(datatwitter$lokasi))
    df_lokasi<-data.frame(lokasi=unlist(sapply(korpus_lokasi, "content")))
    korpus_longitude <- Corpus(VectorSource(datatwitter$longitude))
    df_longitude<-data.frame(longitude=unlist(sapply(korpus_longitude, "content")))
    korpus_latitude <- Corpus(VectorSource(datatwitter$latitude))
    df_latitude<-data.frame(latitude=unlist(sapply(korpus_latitude, "content")))
    #result of cluster for each document
    cluster <- kmeansResult$cluster
    df_cluster<-as.data.frame(cluster)
    df_total<-cbind(df_username,df_date, df_time, baru1, df_pusatkota, df_lokasi, df_longitude, df_latitude, df_cluster)
    return(df_total)
    }
    #save to global variabel
    datatwitter2 <<- praproses(datatwitter2)
    
    #table output after Clustering with K-Means
    output$td1 <- renderDataTable({
      datatwitter2
    })
    
    #Output SSE Clustering
    output$sse <- renderDataTable({
      analisis
    })
    
    #Output TDM
    output$tdm <- renderTable({
      tdm_total
    })
    
    #download Data
    output$downloadData1 <- downloadHandler(
      filename = function() { 
        paste("Hasil K-Means Clustering", '.csv', sep='') 
      },
      content = function(file) {
        progress <- shiny::Progress$new(session, min=1, max=5)
        on.exit(progress$close())
        progress$set(message = 'Downloading in progress')
        
        for (i in 1:5) {
          progress$set(value = i)
          Sys.sleep(1)
        }
        write.csv(datatwitter2,file)
      }
    )
  })
  
  
#------------------------------------------------Interactive Map------------------------------------------------#
  observeEvent(input$show_map,{
    #load data from K-means clustering
    tweet <- datatwitter2
    tweet$longitude <- as.numeric(as.character(tweet$longitude))
    tweet$latitude <- as.numeric(as.character(tweet$latitude))
    tweet$longitude <- jitter(tweet$longitude)
    tweet$latitude <- jitter(tweet$latitude)

    cleantable <- tweet %>%
      select(
         username = username,
         tanggal = date,
         waktu = time,
         kota = pusatkota,
         lokasi = lokasi,
         long = longitude,
         lat = latitude,
         teks = text,
         cluster=cluster
       )

    includeScript("gomap.js")
    output$map <- renderLeaflet({

      factpal <- colorFactor(topo.colors(1000), cleantable$cluster)

      usernamep<- (paste(cleantable$username, " : "))
      teksp<- (paste( " ' ", cleantable$teks, " ' "))
      ab <- (paste(usernamep, teksp ))
      waktup <-(paste("Time & Date : ", cleantable$waktu," & ",cleantable$tanggal))
      lokasip <-(paste("Pusat Kota & Lokasi : ", cleantable$kota, " & ", cleantable$lokasi))
      clusterp<-(paste("Cluster : ", cleantable$cluster))

      leaflet(cleantable) %>% addTiles() %>%
        #Add Marker
        addCircleMarkers(~long, ~lat, fillColor = factpal(cleantable$cluster), fillOpacity = 0.9, 
                         stroke = TRUE, color="#fff", weight=3, 
                         popup = ~htmlEscape(paste( ab,  waktup, 
                                                    lokasip, clusterp, sep=", "))
                        )%>%
        #View Map Indonesia
        setView(lng = 113.9213, lat = 0.7893, zoom = 5)
    })


    #Show Legend
    observe({
      proxy <- leafletProxy("map", data = cleantable)
      proxy %>% clearControls()
      if (input$legend) {
        #pal <- colorpal()
        factpal <- colorFactor(topo.colors(1000), cleantable$cluster)
        proxy %>% addLegend(position = "bottomleft",
                            pal = factpal, values = ~cluster
        )
      }
    })
    
    #Give value to row document
    dok <- sum(tweet$cluster > 0)
    set.seed(100)
    zipdata <- tweet[sample.int(nrow(tweet), dok),]
    print(zipdata)
    
    #Build Interactive Histogram
    zipsInBounds <- reactive({
      if (is.null(input$map_bounds))
        return(zipdata[FALSE,])
      bounds <- input$map_bounds
      latRng <- range(bounds$north, bounds$south)
      lngRng <- range(bounds$east, bounds$west)
      
      longitude <- NULL
      latitude <- NULL
      subset(zipdata,
             latitude >= latRng[1] & latitude <= latRng[2] &
               longitude >= lngRng[1] & longitude <= lngRng[2])
    })
    
    # Make Histogram to show frequency of cluster
    clusterBreaks <- hist(plot = FALSE, cleantable$cluster, breaks = 10)$breaks
    output$histCluster <- renderPlot({
      if (nrow(zipsInBounds()) == 0)
        return(NULL)
      hist(zipsInBounds()$cluster,
           breaks = clusterBreaks,
           main = "Cluster yang dapat dilihat",
           xlab = "Cluster",
           xlim = range(cleantable$cluster),
           col = '#00DD00',
           border = 'white')
    })
    
  })
    
  
#------------------------------------------------------About--------------------------------------------------#  
  output$about<- renderText({
    ("
     Clustering and Geovisualization Text Data Twitter with K-Means Algorithm for Forest Fire and Natural
     Disaster is a web based and framework shiny application for preprocess, K-Means Clustering, and Geovisualization
     Text Data Twitter.
      
     This application is made with the aim in order to fulfill the appliscation developers thesis in 
     Computer Science of Mathematics and Science Faculty of Bogor Agricultural University.

     Developer : Hamid Darojat
     Email     : malikdarojat@gmail.com
     
     ")
  })

  
  
  

  
}
