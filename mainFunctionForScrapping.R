gather <- function(path) {
  
  #path="./data/"
  datoteke<- list.files(path=path, pattern="*.Rda", full.names=FALSE)
  datoteke<-gsub(".Rda","",datoteke)
  try(rm(podatki_s), silent = TRUE)
  try(rm(podatki), silent = TRUE)
  
  
  podatki_s=lapply(datoteke,function(x) {
    filenm=paste(path,as.character(x),".Rda",sep="")
    
    if (file.exists(filenm))
    {
      print(filenm)
      load(filenm)
      return(dfrating)
      
    }
  })
  podatki=as.data.frame(try(do.call(rbind,podatki_s)))
  
  nrow(podatki)
  
  ##Obdržim samo unikatne vrstice
  podatki<-unique(podatki)
  
  
  
  #Izvržem prazne fullrev
  podatki<-podatki[!is.na(podatki$fullrev) & nchar(podatki$fullrev)>10,]
  
  
  #final<-podatki[complete.cases(podatki),]
  
  
  write.table(podatki, file = "TAOutput.txt", append = FALSE, quote = TRUE, sep = "|", row.names = FALSE)
  
  print("TAOUTPUT has been created!")
}

scrap <- function(x,start, end=NULL, path="./data/") {
  
  
  dir.create(file.path(path), showWarnings = FALSE)
  
  if (is.null(end)) {end=length(x)}
  
  hotelsid<-as.list(x)
  for (stevec in start:end) 
  {
    
    #stevec=1
    
    pickhotel = hotelsid[[stevec]]
    
    print("Hotel")
    print(pickhotel)
    
    
    ## Kreiranje linkov
    urllink = createLinks(datah[datah$hotelid == pickhotel, 1])
    
    if (!is.na(urllink)) {
      
      stkorakov=length(urllink)
      
      a <- 1:stkorakov
      
      if (stkorakov>100)
      {b <- a[seq(1, length(a), 100)]} else
      {b <- a[seq(1, length(a), stkorakov)]}
      
      b<-b[-1]
      korakSave<-unique(c(b,length(a)))
      
      
      
      if (stkorakov==0)
      { next 
      }
      
      
      ##Pobiranje podatkov
      dfrating=as.data.frame(NULL)
      
      dfrating.l = as.list(rep(NA, length(length(urllink))))
      
      
      for (i in 1:(length(urllink))) {
        
        #i=1
        ##if (1) {break}
        print("Step...")
        print(i)
        dfrating.l[[i]] = try(getTAdata(urllink[i]))
        
        
        ###Zaradi varnosti pri velikem številu korakov, snemam vsak 100 korak
        if (stkorakov>120)
        {
          
          if (is.element(i,korakSave))
          {
            
            dfrating = try(do.call(rbind, dfrating.l), silent=TRUE)
            
            dfrating = dfrating[!is.na(dfrating$id), ]
            
            dfrating=cbind(dfrating,datah[datah$hotelid==pickhotel,c(2,3,4,5)])
            
            print("Saving...")
            print(i)
            
            filenm = paste(path,"dfrating_", pickhotel, "_",i,".Rda", sep = "")
            save(dfrating, file = filenm)
            
            try(mem_change(rm("dfrating")), silent=TRUE)
            try(mem_change(rm("dfrating.l")), silent = TRUE)
            try(gc(), silent=TRUE)
            
            
            dfrating=as.data.frame(NULL)
            
            dfrating.l = as.list(rep(NA, length(length(urllink))))
            
          }
          
          
        }
        
        
        
      }
      
      
      
      
      
      if (stkorakov<=120 ) {
        
        
        
        dfrating = try(do.call(rbind, dfrating.l))
        names(dfrating)
        
        
        # removing NA
        dfrating = dfrating[!is.na(dfrating$id), ]
        
        dfrating=cbind(dfrating,datah[datah$hotelid==pickhotel,c(2,3,4,5)])
        head(dfrating)
        
        print("Saving.....")
        
        # save to Rdataset
        filenm = paste(path,"dfrating_", pickhotel, ".Rda", sep = "")
        save(dfrating, file = filenm)
        
      }
      
      try(mem_change(rm("dfrating")), silent=TRUE)
      try(mem_change(rm("dfrating.l")), silent=TRUE)
      try(gc(), silent=TRUE)
      
    } 
    
  }
}

getTAdata<-function(url)
{
 
  #url="https://www.tripadvisor.com/Hotel_Review-g644300-d668891-Reviews-or40-Hotel_Creina-Kranj_Upper_Carniola_Region.html#REVIEWS"
  
  
  
  reviews <- url %>% read_html() %>% html_nodes("#REVIEWS .innerBubble")
  
  
  id <- gsub("rn", "", reviews %>% html_node(".quote a") %>% html_attr("id"))
  
  if (length(id)>0)
  {
    
    
    
    quote <- reviews %>% html_node(".quote span") %>% html_text()
    
    rating <- reviews %>% html_node(".ui_bubble_rating") %>% gsub("<span class=\"ui_bubble_rating bubble_", "", 
             .)%>% gsub("\\D","",.)%>% as.integer()/10
    
    localTime<-Sys.getlocale("LC_TIME")
    
    Sys.setlocale("LC_TIME", "C")
      
    # Novi datumi
    date1 <- try(reviews %>% html_node(".relativeDate") %>% html_attr("title"), silent = TRUE)
    
    date1 <- try(as.Date(((gsub("\n", "", date1))), "%B %d, %Y"), silent = TRUE)
    
    if (class(date1) != "Date") {
      date1 <- NA
    }
    
    date1 <- date1[!is.na(date1)]
    
    
    
    # Za nazaj datumi
    date2 <- try(reviews %>% html_node(".ratingDate") %>% html_attr("title"), silent = TRUE)
    
    date2 <- try(as.Date(((gsub("\n", "", date2))), "%B %d, %Y"), silent = TRUE)
    
    if (class(date2) != "Date") {
      date2 <- NA
    }
    
    date2 <- date2[!is.na(date2)]
    
    
    date3 <- try(gsub("Reviewed ", "", reviews %>% html_node(".ratingDate") %>% html_text()), silent = TRUE)
    
    date3 <- try(as.Date(((gsub("\n", "", date3))), "%B %d, %Y"), silent = TRUE)
    
    
    if (class(date3) != "Date") {
      date3 <- NA
    }
    
    date3 <- date3[!is.na(date3)]
    
    # Združim datume
    if (identical(date1, date2)) {
      date = date1
    } else {
      date <- as.Date(c(date1, date2, date3))
    }
    
    Sys.setlocale("LC_TIME", localTime) 
    
    fullrev <- sapply(as.list(id), function(x) {
      
      urlfull <- urlPrepare(url, x)
      
      
      
      # extract node set containing full review
      revid = paste("review_", x, sep = "")
      qry = paste("//p[@id='", revid, "']", sep = "")
      
      ns_fullrev=urlfull %>% read_html() %>% html_nodes(xpath=qry)%>% html_text()
      
      ns_fullrev<-gsub("\n","",ns_fullrev)
      
      
      
      if (length(ns_fullrev)==0) {
        
        ns_fullrev=c("")
      }
      
      return (ns_fullrev)
      
      
      
    })
    
   
    

    
    
    tmpDF <- data.frame(id, quote, rating, date, fullrev, stringsAsFactors = FALSE)
   
    
    colnames(tmpDF) <- c("id","quote","rating","date","fullrev")
    
    
    
  }
  
  
  if (class(tmpDF)!="data.frame")
  {
    tmpDF=as.data.frame(NULL)
  } 
  
  
  return(tmpDF) 
  
  
}