korpus_username <- Corpus(VectorSource(datatwitter$username))
df_username<-data.frame(username=unlist(sapply(korpus_username, "content")))

korpus_date <- Corpus(VectorSource(datatwitter$date))
df_date<-data.frame(date=unlist(sapply(korpus_date, "content")))

korpus_time <- Corpus(VectorSource(datatwitter$time))
df_time<-data.frame(time=unlist(sapply(korpus_time, "content")))

korpus_tweetID <- Corpus(VectorSource(datatwitter$tweetID))
df_tweetID<-data.frame(tweetID=unlist(sapply(korpus_tweetID, "content")))

korpus_placeID <- Corpus(VectorSource(datatwitter$placeID))
df_placeID<-data.frame(placeID=unlist(sapply(korpus_placeID, "content")))

korpus_pusatkota <- Corpus(VectorSource(datatwitter$pusatkota))
df_pusatkota<-data.frame(pusatkota=unlist(sapply(korpus_pusatkota, "content")))

korpus_lokasi <- Corpus(VectorSource(datatwitter$lokasi))
df_lokasi<-data.frame(lokasi=unlist(sapply(korpus_lokasi, "content")))

korpus_longitude <- Corpus(VectorSource(datatwitter$longitude))
df_longitude<-data.frame(longitude=unlist(sapply(korpus_longitude, "content")))

korpus_latitude <- Corpus(VectorSource(datatwitter$latitude))
df_latitude<-data.frame(latitude=unlist(sapply(korpus_latitude, "content")))

hasil_cluster <- read.csv(file = "hasil_cluster.csv", head = TRUE)
korpus_cluster <- Corpus(VectorSource(hasil_cluster$x))
df_cluster<-data.frame(cluster=unlist(sapply(korpus_cluster, "content")))

df_total<-cbind(df_username,df_date, df_time, newdat1,df_tweetID, df_placeID, df_pusatkota, df_lokasi, df_longitude, df_latitude, df_cluster)
write.csv(df_total, file="data_praproses/datafinal3.csv")