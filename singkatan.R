cari.singkatan<-function(str){
  library(RMySQL)
  library(gsubfn)
  library (stringr)
  con <- dbConnect(dbDriver("MySQL"), user = "root", password = "", dbname = "tbkatadasar")
  s <- fn$identity("select * from key_norm where sebelum='$str' ")
  rs <- dbGetQuery(con, s)
  if (nrow(rs)==1){
    dbDisconnect(con)
    return (rs$sesudah)
  } else{
    dbDisconnect(con)
    return (str)
  }
}
clean_regex<-function(str){
  str_new<-str
  
  regex.socmed=c("a+","b+","c+","d+","e+","f+","g+","h+","i+","j+","k+","l+","m
+","n+","o+","p+","q+","r+","s+","t+","u+","v+","w+","x+","y+","z+")
  
  alfabet=c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s",
            "t","u","v","w","x","y","z")
  j<-1
  for (i in regex.socmed){
    str_new<-gsub(i,alfabet[j], str_new)
    j<-j+1
  }
  return (str_new)
}