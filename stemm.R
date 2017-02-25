cek.database<-function(str){
  library(RMySQL)
  library(gsubfn)
  library (stringr)
  con <- dbConnect(dbDriver("MySQL"), user = "root", password = "", dbname = "tbkatadasar")
  s <- fn$identity("select * from tb_katadasar where katadasar='$str' ")
  rs <- dbGetQuery(con, s)
  #  d1 <- fetch(rs, n = -1)
  #  dbClearResult(dbListResults(con)[[1]])
  #dbDisconnect(con)
  if (nrow(rs)==1){
    dbDisconnect(con)
    return (TRUE)
  }
  else
    dbDisconnect(con)
    return (FALSE)
}

#menghilangkan -mu, -ku, -nya, -kah, -lah, dan -pun
del.inflection.suffix<-function(str){
  strAwal<-str
  if (str_detect(str,"([km]u|nya|[kl]ah|pun)$")){
    str_<-str_replace(str,"([km]u|nya|[kl]ah|pun)$","")
    return (str_ )
  }
  return (strAwal)
}

del.derivation.suffix<-function(str){
  strAwal<-str
  if (str_detect(str, "(i|an)$")){
    str_<-str_replace(str, "(i|an)$","")
    if (cek.database(str_)){
      #dbDisconnect(con)
      return (str_)
    }
    else if (str_detect(str, "(kan)$")){
      str_<-str_replace(str,"(kan)$","")
      if (cek.database(str_)){
        #dbDisconnect(con)
        return (str_)
      }
    }
  }
  
  return (strAwal)
}

del.derivation.prefix<-function(str){
  strAwal<-str
  
  #menghilangkan awalan di-, ke-, se- 
  if (str_detect(str,"^(di|[ks]e)")){
    str_<-str_replace(str,"^(di|[ks]e)","")
    if (cek.database(str_)){
      #dbDisconnect(con)
      return (str_)}
    
    str__<-del.derivation.suffix(str_)
    if (cek.database(str__)){
      #dbDisconnect(con)
      return (str__)}
    
    #menghilangkan awalan -diper
    if (str_detect(str, "^(diper)")) {
      str_<-str_replace(str, "^(diper)","")
      str__<-del.derivation.suffix(str_)
      if (cek.database(str__)){
        #dbDisconnect(con)
        return (str__)}
    }
    
    if (str_detect(str,"^(ke[bt]er)")){
      str_<-str_replace(str, "^(ke[bt]er)","")
      str__<-del.derivation.suffix(str_)
      if (cek.database(str__)){
        #dbDisconnect(con)
        return (str__)}
    }
  }
  #awalan be- ber-
  if (str_detect(str, "^([bt]e)")){
    str_<-str_replace(str, "^([bt]e)","")
    if (cek.database(str_)){
      #dbDisconnect(con)
      return (str_)}
    
    str_<-str_replace(str, "^([bt]e[lr])","")
    if (cek.database(str_)){
      #dbDisconnect(con)
      return (str_)}
    
    str__<-del.derivation.suffix(str_)
    if (cek.database(str__)){
      #dbDisconnect(con)
      return (str__)}
  }
  
  if(str_detect(str, "^([mp]e)")){
    str_<-str_replace(str, "^([mp]e)","")
    if (cek.database(str_)){
      #dbDisconnect(con)
      return (str_)}
    
    str__<-del.derivation.suffix(str_)
    if (cek.database(str__)){
      #dbDisconnect(con)
      return (str__)}
    
    if(str_detect(str, "^(memper)")){
      str_<-str_replace(str, "^(memper)","")
      if (cek.database(str_)){
        #dbDisconnect(con)
        return (str_)}
      
      str__<-del.derivation.suffix(str_)
      if (cek.database(str__)){
        #dbDisconnect(con)
        return (str__)}
      
    }
    
    if (str_detect(str, "^([mp]eng)")){
      str_<-str_replace(str, "^([mp]eng)","")
      if (cek.database(str_)){
        #dbDisconnect(con)
        return (str_)}
      
      str__<-del.derivation.suffix(str_)
      if (cek.database(str__)){
        #dbDisconnect(con)
        return (str__)}
      
      str_<-str_replace(str, "^([mp]eng)","k")
      if (cek.database(str_)){
        #dbDisconnect(con)
        return (str_)}
      
      str__<-del.derivation.suffix(str_)
      if (cek.database(str__)){
        #dbDisconnect(con)
        return (str__)}
    }
    
    if (str_detect(str, "^([mp]eny)")){   
      str_<-str_replace(str, "([mp]eny)","s")
      if (cek.database(str_)){
        #dbDisconnect(con)
        return (str_)}
      
      str__<-del.derivation.suffix(str_)
      if (cek.database(str__)){
        #dbDisconnect(con)
        return (str__)}
    }
    
    if (str_detect(str, "^([mp]e[lr])")){   
      str_<-str_replace(str, "([mp]e[lr])","")
      if (cek.database(str_)){
        #dbDisconnect(con)
        return (str_)}
      
      str__<-del.derivation.suffix(str_)
      if (cek.database(str__)){
        #dbDisconnect(con)
        return (str__)}
    }
    
    if (str_detect(str, "^([mp]en)")){
      str_<-str_replace(str, "([mp]en)","")
      if (cek.database(str_)){
        #dbDisconnect(con)
        return (str_)}
      
      str__<-del.derivation.suffix(str_)
      if (cek.database(str__)){
        #dbDisconnect(con)
        return (str__)}
      
      str_<-str_replace(str, "^([mp]en)","t")
      if (cek.database(str_)){
        #dbDisconnect(con)
        return (str_)}
      
      str__<-del.derivation.suffix(str_)
      if (cek.database(str__)){
        #dbDisconnect(con)
        return (str__)}
    }
    
    if (str_detect(str, "^([mp]em)")){
      str_<-str_replace(str, "([mp]em)","")
      if (cek.database(str_)){
        #dbDisconnect(con)
        return (str_)}
      
      str__<-del.derivation.suffix(str_)
      if (cek.database(str__)){
        #dbDisconnect(con)
        return (str__)}
      
      str_<-str_replace(str, "^([mp]em)","p")
      if (cek.database(str_)){
        #dbDisconnect(con)
        return (str_)}
      
      str__<-del.derivation.suffix(str_)
      if (cek.database(str__)){
        #dbDisconnect(con)
        return (str__)}
    }
    
  }
  return (strAwal)
}


stemming<-function(str){
  strAwal<-str
  if (cek.database(str)){
    #dbDisconnect(con)
    return (str)
  }
  else{
    str_<-del.inflection.suffix(str)
    if (cek.database(str_)){
      #dbDisconnect(con)
      return (str_)
    }
    
    str_<-del.derivation.suffix(str)
    if (cek.database(str_)){
      #dbDisconnect(con)
      return (str_)
    }
    
    str_<-del.derivation.prefix(str)
    if (cek.database(str_)){
      #dbDisconnect(con)      
      return (str_)
    }
  }
  return (strAwal)
}
