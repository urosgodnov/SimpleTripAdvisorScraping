# load libraries
packages=c("dplyr","XML","rvest","stringr","plyr","xml2","pryr")
package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})


#odprem TAOUPUT.txt in preberem vse memberje

member<-read.table(file="TAOutput.txt", sep="|", header = TRUE)
member<-as.data.frame(member)
member<-member[,"memberID"]
dir.create(file.path("./memberData/"), showWarnings = FALSE)

if (length(member)>0)
{
  
  #kreiram listo
  memberList<-as.list(unique(member))
  
  #in gremonajpre naredit urlje
  urls<-sapply(memberList, function(x){
      
    url<-paste("https://www.tripadvisor.com/members/",x,sep="")
    
    return(url)
    
  })
  
  
  urlslist<-as.list(urls)
  
  for (i in 1:length(urlslist))
  {  
    
    x<- urlslist[[i]]
   
    print("Uporabnik.....")
    print(x)
    
    #x<-"https://www.tripadvisor.com/members/01773"
    
    mdata <- x %>% read_html()
    
    basic<-mdata %>% html_node("#MODULES_MEMBER_CENTER")
    
    #checking for basic data
    
    #since
    since<-basic %>% html_nodes(".since") %>% html_text()
    
    
    
    #Age - poberem vse dol in odstranim since
    age<-basic %>% html_nodes(".ageSince") %>% html_text()
    
    #Gender - check, če se kaj nahaja notri
    if (length(grep("female",age,fixed = TRUE))==1)
    {
      gender<-"Female"
      
      age<- gsub("female","",age) 
      } else if (length(grep("male",age,fixed = TRUE))==1)
      {
        gender<-"Male"
        
        age<- gsub("male","",age)
        
      } else {gender<-"Unknown"}
    
    
    age<-trimws(gsub(since,"",age),"both")
    
    age<-trimws(gsub("year old","",age),"both")
    
    since<-gsub("Since ","",since)
    
    #Hometown
    hometown<-basic %>% html_nodes(".hometown") %>% html_text()
    
    #Če obstaja hometown in če je vejica, razdelim po vejici in prvo dam kot
    #mesto, drugo pa kot državo
    if (nchar(hometown)>0 && length(grep(",",hometown))==1)
    {
      country<-strsplit(hometown,",")[[1]][2]
      hometown<-strsplit(hometown,",")[[1]][1]
      
    } else if (nchar(hometown)>0) {
      
      country<-hometown
    } else {hometown<-"Unknown"
            country<-"Uknown"}
    
    
      #Travel style
      tags<-paste(basic %>% html_nodes(".tagBlock")%>% 
      html_nodes(".tagBubble")%>% html_text(),collapse=",")
    
      #Member Points
       points1<-basic %>% html_nodes(".member-points")%>% 
       html_nodes("a")%>%html_text()
    
       #reviews
       reviews <-points1[grep("Reviews|Review",points1)]
       if (length(reviews)==1) {
       reviews<- as.numeric(gsub("[^\\d]","",reviews, perl = TRUE))
       } else {reviews<-0}
       
       #helpful
       helpful <-points1[grep("Helpful",points1)]
       if (length(helpful)==1) {
         helpful<- as.numeric(gsub("[^\\d]","",helpful, perl = TRUE))
       } else {helpful<-0}
       
       #Whole points
       points<- as.numeric(gsub(",","",basic %>% html_node(".points")%>%html_text()))
       
       #Contributor level
       contributorL<- as.numeric(gsub("[^\\d]","",basic %>% html_node(".level")%>%html_text(), perl = TRUE))
       
       #memberid
       memberID<-gsub("https://www.tripadvisor.com/members/","",x)
       
       tmpDF <- data.frame(memberID, gender, age, since, points, contributorL, reviews, helpful, hometown, country, tags, stringsAsFactors = FALSE)
       
       print("Saving.....")
       
       # save to Rdataset
       filenm = paste("./memberData/",memberID, ".Rda", sep = "")
       save(tmpDF, file = filenm)
  }
  
}


datoteke<- list.files(path="./memberData/", pattern="*.Rda", full.names=FALSE)
datoteke<-gsub(".Rda","",datoteke)
try(rm(podatki_s), silent = TRUE)
try(rm(podatki), silent = TRUE)


podatki_s=lapply(datoteke,function(x) {
  filenm=paste("./memberData/",as.character(x),".Rda",sep="")
  
  if (file.exists(filenm))
  {
    print(filenm)
    load(filenm)
    return(tmpDF)
    
  }
})
podatkiTmp=as.data.frame(try(do.call(rbind,podatki_s)))

nrow(podatkiTmp)

results<-ObdelajTage(podatkiTmp)
