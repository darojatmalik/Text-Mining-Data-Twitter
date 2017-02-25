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

datatwitter <- read.csv(file = "datafinal2.csv", head = TRUE)
korpus <- Corpus(VectorSource(datatwitter$text))


#for(j in seq(myCorpus))

#{  myCorpus[[j]] <- gsub("\\S*(\\S)\\1\\1\\S*\\s?", "", myCorpus[[j]])  
#  myCorpus[[j]] <- gsub("(@)[[:graph:]]+", "", myCorpus[[j]])
#  myCorpus[[j]] <- gsub("(http)[[:graph:]]+", "", myCorpus[[j]])
#  myCorpus[[j]] <- gsub("(#)[[:graph:]]+", "", myCorpus[[j]])
#  myCorpus[[j]] <- gsub("&lt;-"," ", myCorpus[[j]])
#  myCorpus[[j]] <- gsub("&lt;-"," ", myCorpus[[j]])
#  myCorpus[[j]] <- gsub("[[:punct:]]", " ", myCorpus[[j]])
#  myCorpus[[j]] <- gsub("[[:digit:]]", " ", myCorpus[[j]]) }


#membuat remove URL tm karakter
hapusURL <- function(x) gsub("(http)[[:graph:]]*", "", x)
korpus <- tm_map(korpus, content_transformer(hapusURL))
enter <- function(x) gsub("\n", " ", x)
korpus <- tm_map(korpus, content_transformer(enter))
karakter1 <- function(x) gsub("\\S*(\\S)\\1\\1\\S*\\s?", "", x)  
korpus <- tm_map(korpus, content_transformer(karakter1))
karakter2 <- function(x) gsub("(@)[[:graph:]]+", "", x)
korpus <- tm_map(korpus, content_transformer(karakter2))
#karakter3 <- function(x) gsub("(#)[[:graph:]]+", "", x)
#korpus <- tm_map(korpus, content_transformer(karakter3))
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
#splitgue <- lapply(splitgue, unique)
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

hasilnormal<-lapply(splitgue,function(v) {c (v, rep("",max.length-length(v)))})
hasilnormal<-do.call(rbind,hasilnormal)
hasilnormal<-data.frame(hasilnormal)


#mencoba stopwords
dataframe1 <-data.frame(text=unlist(sapply(korpus, "content")))
newdat <- data.frame(text=str_trim(do.call(paste, hasilnormal)), stringsAsFactors=FALSE)
baru<-data.frame(newdat)
names(baru)[names(baru)=="hasilnormalbaru"] <- "text"


korpus1 <- Corpus(VectorSource(baru$text))
korpus1 <- tm_map(korpus1, stripWhitespace)
stopword<- read.csv("stopword.csv")
stopword <- tolower(stopword[, 1])
stopword <- c(stopword, "", "a", "-", "rt", "...", "&", "|", "ada")
korpus1=tm_map(korpus1,removeWords,stopword)
korpus1 <- tm_map(korpus1, stripWhitespace)

#for(j in seq(korpus1))
#{
# korpus1[[j]] <- gsub("^\\s+|\\s+$", "", korpus1[[j]])
#}

#membuat data frame 2
korpus1 <- tm_map(korpus1, stripWhitespace)
korpus1 <- tm_map(korpus1, removeNumbers)
korpus1 <- tm_map(korpus1, removePunctuation)
korpus1 <- tm_map(korpus1, content_transformer(tolower))
korpus1 <- tm_map(korpus1, stripWhitespace)


dataframe2<-data.frame(text=unlist(sapply(korpus1, "content")))
splitgue1<-strsplit(as.character(dataframe2$text), split = " ")
#splitgue1 <- lapply(splitgue1, unique)
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

hasilnormal1<-lapply(splitgue1,function(v) {c (v, rep("",max.length-length(v)))})
hasilnormal1<-do.call(rbind,hasilnormal1)
hasilnormal1<-data.frame(hasilnormal1)



#pembuatan TDM
library(stringr)
newdat1 <- data.frame(text=str_trim(do.call(paste, hasilnormal1)), stringsAsFactors=FALSE)
baru1<-data.frame(newdat1)
names(baru1)[names(baru1)=="hasilnormalbaru"] <- "text"


baru1
v <- Corpus(VectorSource(baru1$text))
tdm <- TermDocumentMatrix(v)
tdm

#tdm to matrix
m <- as.matrix(tdm)
dim(m)
#mereduksi terms 
# This makes a matrix that is ???% empty space, maximum. 
#tdms <- removeSparseTerms(tdm, 0.995)
tdms <- removeSparseTerms(tdm, 0.99)
#tdms <- removeSparseTerms(tdm, 0.985)



inspect(tdms)
m1 <- as.matrix(tdms)
dim(m1)
#write.csv(m1, file="dtm1.csv")
tdms
#K-Means Cara 2
library(fpc)
# transpose the matrix to cluster documents (tweets)
m2 <- t(m1)
# set a fixed random seed
set.seed(122)
# k-means clustering of tweets
k <-7
kmeansResult <- kmeans(m2, k)
# cluster centers
round(kmeansResult$centers, digits=3)

#clust <- clus[1][[1]]
write.csv(kmeansResult$cluster, file="hasil_cluster.csv")

library(cluster)
library(factoextra)
library(ggplot2)
clusplot(m2, kmeansResult$cluster, colot = TRUE, shade = TRUE, labels = 2, lines = 0)
fviz_cluster(kmeansResult, data = m2)

# 
# library(wordcloud)
# #m1 <- as.matrix(tdms)
# #calculate the frequency of words and sort it descendingly by frequency
# wordFreq <- sort(rowSums(m1), decreasing = TRUE)
# #word cloud
# set.seed(375) #to make it reproducible
# grayLevels <- gray((wordFreq + 10) / (max(wordFreq) + 10))
# wordcloud(words = names(wordFreq), freq=wordFreq, min.freq=3, random.order = F, colors = grayLevels)
