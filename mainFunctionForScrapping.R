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
      return(dataToBeSaved)
      
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

scrap <- function(x,start, end=NULL, path="./data/", memberid=FALSE) {
  

  dir.create(file.path(path), showWarnings = FALSE)
  
  if (is.null(end)) {end=length(x)}
  
  hotelsid<-as.list(x)
  for (stevec in start:end) 
  {
    
    #stevec=2
    
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
      
    #  dfrating.l = as.list(rep(NA, length(length(urllink))))
      
      
      for (i in 1:(length(urllink))) {
        
        #i=1
        ##if (1) {break}
        print("Step...")
        print(i)
        #dfrating.l[[i]] = try(getTAdata(urllink[i], memberid))
        
        dataToBeSaved=""
        dataToBeSaved= try(getTAdata(urllink[i], memberid), silent = TRUE)
        
       try( if (length(dataToBeSaved)>0 & nrow(dataToBeSaved)) {
          
          # save to Rdataset
          filenm = paste(path,"dfrating_", pickhotel,"_",as.character(i),".Rda", sep = "")
          save(dataToBeSaved, file = filenm)
          
        }, silent = TRUE )
    
      }

      
      try(mem_change(rm("dataToBeSaved")), silent=TRUE)
     # try(mem_change(rm("dfrating.l")), silent=TRUE)
      try(gc(), silent=TRUE)
      
    } 
    
  
  }
}

getTAdata <- function(url, memberid)
{
  #url<-"https://www.tripadvisor.com/Hotel_Review-g274863-d506611-Reviews-or25-Garni_Hotel_Jadran-Bled_Upper_Carniola_Region.html#REVIEWS"
  
  reviews <-
    url %>% read_html() %>% html_nodes(xpath = "//*[contains(@class, 'hotels-community-tab-common-Card')]")
  
  
  quote <-
    reviews %>% html_nodes(xpath = "//*[contains(@class, 'reviewTitleText')]") %>%
    html_text()
  
  
  
  rating <-
    reviews %>% html_node(".ui_bubble_rating") %>% gsub("<span class=\"ui_bubble_rating bubble_", "",
                                                        .) %>% gsub("\\D", "", .) %>% as.integer() /
    10
  
  date <-
    reviews %>% html_nodes(xpath = "//span[contains(@class, 'EventDate')]") %>%
    html_text()
  
  date <- trimws(gsub("Date of stay:", "", date))
  
  date<-date[date!=""]
  
  localTime <- Sys.getlocale("LC_TIME")
  
  
  Sys.setlocale("LC_TIME", "English")
  
  date <- paste("01 ", date, sep = "")
  
  date <- dmy(date)
  
  Sys.setlocale("LC_TIME", localTime)
  
  
  fullrevlinks <-
    reviews %>% html_nodes(xpath = "//*[contains(@class, 'reviewTitleText')]") %>%
    html_attr("href")
  
  
  fullrev <- sapply(as.list(fullrevlinks), function(x) {
    urlfull <- paste("https://www.tripadvisor.com", x, sep = "")
    
    
    #print(urlfull)
    # extract node set containing full review
    #revid = paste("review_", x, sep = "")
    qry = paste(
      "//span[@class='partial_entry' or @class='fullText hidden' or @class='fullText' or @class='fullText ']",
      sep = ""
    )
    
    
    print("Getting full review from TripAdvisor....")
    ns_fullrev = urlfull %>% read_html() %>% html_nodes(xpath = qry) %>% html_text()
    
    ns_fullrev <- gsub("\n", "", ns_fullrev)
    
    
    
    if (length(ns_fullrev) == 0) {
      ns_fullrev = c("")
    }
    
    return (ns_fullrev)
    
    
    
  })
  #memberid=TRUE
  
  if (memberid == TRUE) {
    userID <- reviews %>% html_node(".member_info div") %>% html_attr("id")
    
    TAmemberIDandDist <- sapply(as.list(userID), function(x) {
      #x<-userID[[1]]
      
      urlTAmember <- urlPrepareMembers(x)
      
      memberPage <- urlTAmember %>% read_html()
      
      mTAid <- tryCatch(
        memberPage %>% html_node("a") %>%
          html_attr("href"),
        error = function(e) {
          return(NA)
          
        }
      )
      
      
      
      
      
      mTAid <- gsub("/members/", "", mTAid)
      
      Dist <-
        gsub(
          "\n",
          "",
          memberPage %>% html_nodes(".chartRowReviewEnhancements") %>%
            html_text()
        )
      
      
      
      Dist <- paste(Dist, collapse = ",")
      
      returndata <- data.frame(mTAid, Dist, stringsAsFactors = FALSE)
      
      return (returndata)
      
      
    })
    
    TAmemberID <- unlist(TAmemberIDandDist[1, ])
    ReviewsDist <- unlist(TAmemberIDandDist[2, ])
    
  } else {
    TAmemberID = NA
    ReviewsDist = NA
  }
  
 
  tmpDF <-
    data.frame(quote,
               rating,
               date,
               fullrev,
               TAmemberID,
               ReviewsDist,
               stringsAsFactors = FALSE)
  
  
  colnames(tmpDF) <-
    c("quote",
      "rating",
      "date",
      "fullrev",
      "memberID",
      "UserRatingsDistribution")
  

if (class(tmpDF) != "data.frame")
{
  tmpDF = as.data.frame(NULL)
}


return(tmpDF)


}