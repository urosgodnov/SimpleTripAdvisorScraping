

# Priprava URLja
urlPrepare <- function(url, id) {
    
    urllinkpost = strsplit(url, "Reviews-")[[1]][2]
    
    if (grepl("Hotel_Review", url)) {
        
        urllinkpre.rev = gsub("Hotel_Review", "ShowUserReviews", strsplit(url, "Reviews-")[[1]][1])
    } else if (grepl("Attraction_Review", url)) {
        
        urllinkpre.rev = gsub("Attraction_Review", "ShowUserReviews", strsplit(url, "Reviews-")[[1]][1])
    } else {
        urllinkpre.rev = gsub("Restaurant_Review", "ShowUserReviews", strsplit(url, "Reviews-")[[1]][1])
    }
    
    urlrev = paste(urllinkpre.rev, "rXX-", urllinkpost, sep = "")
    
    urlfullrevlist = gsub("XX", id, urlrev)
    
    return(urlfullrevlist)
    
}


urlPrepareMembers <- function(userID) {
  
 
  
  urllinkUID = strsplit(userID, "-")[[1]][1]
  urllinkSRC = strsplit(userID, "-")[[1]][2]
  
  urllinkUID=gsub("UID_","",urllinkUID)
  urllinkSRC=gsub("SRC_","",urllinkSRC)
  
  urlmember=paste("https://www.tripadvisor.com/MemberOverlay?Mode=owa&uid=",urllinkUID,sep="")
  urlmember=paste(urlmember,"&c=&src=",urllinkSRC,paste="")
  urlmember=paste(urlmember,"&fus=false&partner=false&LsoId=&metaReferer=ShowUserReviewsHotels",sep="")
  urlmember=gsub(" ","",urlmember)
  
  return(urlmember)
  
}

createLinks <- function(url)
  
{
  
  #url="https://www.tripadvisor.com/Hotel_Review-g644300-d668891-Reviews-Hotel_Creina-Kranj_Upper_Carniola_Region.html#REVIEWS"
  pages <- 
     url %>% read_html() %>% html_nodes(".pageNumbers")%>%html_nodes(".pageNum")%>%
       html_text()
  
  #pages <- max(as.integer(pages))
   
  urllink=NA
  
  if (length(pages)>0) {
  
  lastPage <-max(as.integer(pages))
  
  urlmainlist = url
  morepglist = seq(5, lastPage*5, 5)
  
  # url link for first search page
  urllinkmain = urlmainlist
  # counter for additional pages
  morepg = as.numeric(morepglist)
  
  urllinkpre = paste(strsplit(urllinkmain, "Reviews-")[[1]][1], "Reviews", sep = "")
  urllinkpost = strsplit(urllinkmain, "Reviews-")[[1]][2]
  end = "#REVIEWS"
  urllink = rep(NA, length(morepg) + 1)
  
  urllink[1] = urllinkmain
  for (i in 1:length(morepg)) {
    reviews = paste(urllinkpre, "-or", morepg[i], "-", urllinkpost, sep = "")
    urllink[i + 1] = paste(reviews, end, sep = "")
  }
  }
  return(urllink)
}


ObdelajTage<-function(podatki) {
  
  tagi<-podatki[!is.na(podatki$tags),c("tags")]
  tagi<-unlist(strsplit(tagi,","))
  tagi<-as.list(unique(str_trim(tagi, side=c("both"))))
  
  for (i in 1:length(tagi))
  {
    ime<-tagi[[i]]
    stevilo<-NA
    podatki<-cbind(podatki,stevilo)
    names(podatki)[ncol(podatki)]<-ime
    
  }
  
  for (i in 1:nrow(podatki))
  {
    
       #####################Obdelava tagov #####################################
    tagiVrstica<-podatki[i,c("tags")]
    tagiVrstica<-unlist(strsplit(tagiVrstica,","))
    tagiVrstica<-as.list(unique(str_trim(tagiVrstica, side=c("both"))))
    
    
    for (j in 1:length(tagi))
    {
      
      if (length(intersect(unlist(tagi),unlist(tagiVrstica)))>0)
      {
        if (length(intersect(tagi[[j]],unlist(tagiVrstica)))>0)
        {
          podatki[i,tagi[[j]]]<-1
          
        } else {
          
          podatki[i,tagi[[j]]]<-0
          
        }
        
      } else {
        
        podatki[i,tagi[[j]]]<-NA
      }
      
      
    }
  }
  
  return(podatki)
}

