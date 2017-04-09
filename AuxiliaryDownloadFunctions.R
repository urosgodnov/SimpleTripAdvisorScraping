# Funkcija za pridobitev datuma

monthR = function(x) 
{
    x = try(gsub("January", "Januar", x))
    x = try(gsub("February", "Februar", x))
    x = try(gsub("March", "Marec", x))
    x = try(gsub("May", "Maj", x))
    x = try(gsub("June", "Junij", x))
    x = try(gsub("July", "Julij", x))
    x = try(gsub("August", "Avgust", x))
    x = try(gsub("October", "Oktober", x))
    
    
    return(x)
}



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


createLinks <- function(url)
  
{
   pages <- url %>% read_html() %>% html_nodes("#REVIEWS .pageNumbers")
  
  urllink=NA
  
  if (length(pages)>0) {
  
  lastPage <-max(as.integer(pages %>% html_nodes("a" ) %>% html_attr("data-page-number")))
  
  urlmainlist = url
  morepglist = seq(10, lastPage*10, 10)
  
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

#Funkcija za obdelavo datumov

monthF<-function(x)
{
  if (grepl("Jan",x)) {
    r="1"
  } else if (grepl("Feb",x)) {
    r="2"
  } else if (grepl("Mar",x)) {
    r="3"
  } else if (grepl("Apr",x)) {
    r="4"
  } else if (grepl("May",x)) {
    r="5"
  } else if (grepl("Jun",x)) {
    r="6"
  } else if (grepl("Jul",x)) {
    r="7"
  } else if (grepl("Aug",x)) {
    r="8"
  } else if (grepl("Sep",x)) {
    r="9"
  } else if (grepl("Oct",x)) {
    r="10"
  } else if (grepl("Nov",x)) {
    r="11"
  } else if (grepl("Dec",x)) {
    r="12"
  } else
    r=NA
  
  return(r)     
}

ObdelajStarost<-function(x)
{
  
  m <- regexpr("[0-9][0-9]-[0-9][0-9]", x, perl=TRUE)
  r=ifelse((is.na(m) | m<0),NA,regmatches(x, m))
  
  return(r)
  
}

ObdelajDatume<-function(x)
{
  
  m1 <- regexpr("[0-9]...", x , perl=TRUE)
  leto<-regmatches(x, m1)
  mesec<-monthF(x)
  r<-ifelse(is.na(mesec),NA,paste(leto,"-",mesec,"-1",sep=""))
  
  return(r)
  
}


ObdelajPodatke <- function (podatki,worldcities) {
  
  ##Vmesno shranjevanje
  stkorakov=nrow(podatki)
  
  a <- 1:stkorakov
  
  b <- a[seq(1, length(a), 10000)]
    
  b<-b[-1]
  korakSave<-unique(c(b,length(a)))
  
  #naredim par izračunov
  ##najprej generiram nove stolpce
  tagi<-podatki[!is.na(podatki$Tags),c("Tags")]
  tagi<-unlist(strsplit(tagi,","))
  tagi<-as.list(unique(str_trim(tagi, side=c("both"))))
  
  for (i in 1:length(tagi))
  {
    ime<-tagi[[i]]
    stevilo<-NA
    podatki<-cbind(podatki,stevilo)
    names(podatki)[ncol(podatki)]<-ime
    
  }
  
  
  
  
  #starost in datumi
  podatki[,c("Age_of_reviewer")]<-NA
  podatki[,c("Reviews")]<-NA
  
  podatki[is.na(podatki$Hotel_reviews),c("Hotel_reviews")]<-0
  podatki[is.na(podatki$Attraction_reviews),c("Attraction_reviews")]<-0
  podatki[is.na(podatki$Restaurant_reviews),c("Restaurant_reviews")]<-0
  
  
  stanford<-list()
  afinn<-list()
  nrc<-list()
  bing<-list()
  noteSentiment<-list()
  
  sentimentAll=list()

  
  for (i in 1:nrow(podatki))
  {
    
     podatki[i,c("Age_of_reviewer")]<-ObdelajStarost(podatki[i,c("Gender")])
    podatki[i,c("Member_since")]<-ObdelajDatume(podatki[i,c("Member_since")])
    
    #####################Obdelava tagov #####################################
    tagiVrstica<-podatki[i,c("Tags")]
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
    
    #######################################################################################
    
    nrev<-max(podatki[i,c("Hotel_reviews")],podatki[i,c("Attraction_reviews")],podatki[i,c("Restaurant_reviews")], na.rm = TRUE)
    
    nrev<-ifelse(nrev==-Inf,0,nrev)
    
    podatki[i,c("Reviews")]<-nrev
    
    ############################Obdelava sentimenta Liu#######################################
    
    sentimentAll[[i]]<-getSentiment(podatki[i,c("fullrev")])
    
    
    ############################Obdelava sentimenta AFINN#######################################
  
    afinn[[i]]<-get_sentiment(as.vector(podatki[i,c("fullrev")]),"afinn")
    
    
    ##########################Sentiment NRC####################################
    nrc[[i]]<-get_sentiment(as.vector(podatki[i,c("fullrev")]),"nrc")

    
    ##########################Sentiment bing####################################
    bing[[i]]<-get_sentiment(as.vector(podatki[i,c("fullrev")]),"bing")
    
    ##########################Stanford####################################
    #stanford[[i]]<-get_sentiment  (as.vector(podatki[i,c("fullrev")]),"stanford","./lookups/stanford/")
    
    
    
    ########################Iskanje države#########################################3
    
   
    lokacija <- unlist(strsplit(podatki[i, "location"], "[,]"))
    lokacija <-as.list(str_trim(lokacija, side="both"))
        
        if (length(lokacija) > 0) {
          
          for (j in length(lokacija):1) 
          {
           
            #Najprej grem po državah, ker jih je manj
            city.index <- match(lokacija[[j]], worldcities[, 3])
            
            if (is.na(city.index)) {
              city.index <- 0
            }
            
            drzava <- ifelse(city.index > 0, worldcities[city.index, 3], "uknown")
           
            
            if (drzava != "uknown") {
              podatki[i, c("State")] <- drzava
              break
            }
            
          }
          #Če je država unknown, grem gledat po mestih
          if (drzava == "uknown")
          {
          for (n in 1:length(lokacija)) 
          {
           
            #Najprej grem po državah, ker jih je manj
            city.index <- match(lokacija[[n]], worldcities[, 1])
            
            if (is.na(city.index)) {
              city.index <- 0
            }
            
            drzava <- ifelse(city.index > 0, worldcities[city.index, 3], "Uknown")
            
            
            if (drzava != "uknown") {
              podatki[i, c("State")] <- drzava
              break
            }
            
          }
          }
    
        } else {
          podatki[i, c("State")] <- "uknown"
        }
  
    
   
    
    
    ####Vmesno shranjevanje
    if (is.element(i,korakSave))
    {

      
      podatki1<-head(podatki,i)
      
      podatki1<-cbind(podatki1,do.call(rbind,bing))
      
      podatki1<-cbind(podatki1,do.call(rbind,nrc))
      
      podatki1<-cbind(podatki1,do.call(rbind,afinn))
      
      podatki1<-cbind(podatki1,do.call(rbind,sentimentAll))
      
      podatki1[,c("Gender")]<-ifelse(grepl("female", podatki1[,c("Gender")]), "Female", ifelse(grepl("male", podatki1[,c("Gender")]),"Male",NA))
      
      podatki1[,c("Hotel_reviews")]<-NULL
      podatki1[,c("Attraction_reviews")]<-NULL
      podatki1[,c("Restaurant_reviews")]<-NULL
      podatki1[,c("Tags")]<-NULL
      podatki1[,c("Tip")]<-NULL
      stolpci<-ncol(podatki1) 
      #colnames(podatki)[stolpci-4]<-"Stanford"
      colnames(podatki1)[stolpci-4]<-"BING"
      colnames(podatki1)[stolpci-3]<-"NRC"
      colnames(podatki1)[stolpci-2]<-"AFINN"
      colnames(podatki1)[stolpci-1]<-"Liu"
      colnames(podatki1)[stolpci]<-"St_besed"
      
      
      final<-podatki1[complete.cases(podatki1),]
      
      write.table(final, file = paste("./outputs/TA_",i,".txt",sep=""), append = FALSE, quote = TRUE, sep = ";", row.names = FALSE)
      
      
      
    }
        
    
    
    
    print("Obdelava vrstice...")
    print(i)
  }
  
  #podatki<-cbind(podatki,do.call(rbind,stanford))
  
  podatki<-cbind(podatki,do.call(rbind,bing))
  
  podatki<-cbind(podatki,do.call(rbind,nrc))
  
  podatki<-cbind(podatki,do.call(rbind,afinn))
  
  podatki<-cbind(podatki,do.call(rbind,sentimentAll))
  
  podatki[,c("Gender")]<-ifelse(grepl("female", podatki[,c("Gender")]), "Female", ifelse(grepl("male", podatki[,c("Gender")]),"Male",NA))
  
  podatki[,c("Hotel_reviews")]<-NULL
  podatki[,c("Attraction_reviews")]<-NULL
  podatki[,c("Restaurant_reviews")]<-NULL
  podatki[,c("Tags")]<-NULL
  podatki[,c("Tip")]<-NULL
  stolpci<-ncol(podatki) 
 #colnames(podatki)[stolpci-4]<-"Stanford"
  colnames(podatki)[stolpci-4]<-"BING"
  colnames(podatki)[stolpci-3]<-"NRC"
  colnames(podatki)[stolpci-2]<-"AFINN"
  colnames(podatki)[stolpci-1]<-"Liu"
  colnames(podatki)[stolpci]<-"St_besed"
  
  return(podatki)
  
}

getSentiment=function(text)
{
  
  
  
  
  #Sentiment analiza
  ## Score sentiment for each Twitt
  results=score.sentiment(text,positive,negative)
  
  
  # print ("Povzetek sentimen analize")
  
  s=summary(results$score)
  nWords=results$nwords
  # numberSentences=length(results$text)
  
  #print(results$text)
  
  #    print( class(s))
  # 
  #    print (s)
  
  # Dodam rezultate v tabelo
  
  #r3=cbind.data.frame(s[1], s[4], s[6],numberSentences)
  
  #names(r3)=c("Min","Mean","Max","St. stavkov")
  
  return(data.frame(s[4],nWords))
  
}

score.sentiment = function(sentences, pos.words, neg.words, .progress="none")
{     
  require(stringr)   
  
  # we got a vector of sentences. plyr will handle a list or a vector as an "l" for us   
  
  # we want a simple array of scores back, so we use "l" + "a" + "ply" = laply: 
  
  scores = laply(sentences, function(sentence, pos.words, neg.words) 
  {  
    # clean up sentences with Rs regex-driven global substitute, gsub():	 
    
    sentence = gsub("[[:punct:]]","", sentence)	 
    sentence = gsub("[[:cntrl:]]","" , sentence)	 
    sentence = gsub("\\d+","" , sentence)	 
    
    # and convert to lower case:	 
    
    sentence = tolower(sentence)	 
    
    # split into words. str_split is in the stringr package	 
    
    word.list = str_split(sentence, "\\s+")	
    
    # sometimes a list() is one level of hierarchy too much	
    
    words = unlist(word.list)	
    
    nWords=length(words)
    # compare our words to the dictionaries of positive & negative terms	 
    
    pos.matches = match(words, pos.words) 
    neg.matches = match(words, neg.words)	 
    
    # match() returns the position of the matched term or NA	 
    # we just want a TRUE/FALSE:	 
    
    pos.matches = !is.na(pos.matches)	 
    neg.matches = !is.na(neg.matches)	 
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():	 
    
    score = sum(pos.matches) - sum(neg.matches)
    words.df=data.frame(score,nWords)
    return(words.df)	 
    
  }, pos.words, neg.words, .progress=.progress )	 
  
  scores.df = data.frame(score=scores$score, text=sentences,nwords=scores$nWords)	 
  
  return(scores.df)
}

getSentimentBayes=function(text)
{
  
  
  ###Priprava teksta
  
  # remove punctuation
  some_txt = gsub("[[:punct:]]", "", text)
  # remove numbers
  some_txt = gsub("[[:digit:]]", "", some_txt)
  # remove html links
  some_txt = gsub("http\\w+", "", some_txt)
  # remove unnecessary spaces
  some_txt = gsub("[ \t]{2,}", "", some_txt)
  some_txt = gsub("^\\s+|\\s+$", "", some_txt)
  
  # define "tolower error handling" function 
  try.error = function(x)
  {
    # create missing value
    y = NA
    # tryCatch error
    try_error = tryCatch(tolower(x), error=function(e) e)
    # if not an error
    if (!inherits(try_error, "error"))
      y = tolower(x)
    # result
    return(y)
  }
  # lower case using try.error with sapply 
  some_txt = sapply(some_txt, try.error)
  
  # remove NAs in some_txt
  some_txt = some_txt[!is.na(some_txt)]
  names(some_txt) = NULL
  
  
  ## Dejanska klasifikacija
  
  # classify emotion
  class_emo = classify_emotion(some_txt, algorithm="bayes", prior=1.0)
  # get emotion best fit
  emotion = class_emo[,7]
  # substitute NA's by "unknown"
  emotion[is.na(emotion)] = "unknown"
  
  # classify polarity
  class_pol = classify_polarity(some_txt, algorithm="bayes")
  # get polarity best fit
  polarity = class_pol[,4]
  
  
  sent_df = data.frame(text=some_txt, emotion=emotion,
                       polarity=polarity, stringsAsFactors=FALSE)
  
  # sort data frame
  sent_df = within(sent_df,
                   emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
  
  return(sent_df)
  
}

