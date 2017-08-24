# load libraries
packages=c("dplyr","XML","rvest","stringr","plyr","xml2","pryr")
package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

source("AuxiliaryDownloadFunctions.R")
source("mainFunctionForScrapping.R")


options(stringsAsFactors = FALSE, silent=TRUE)


# Hotels' data
# Check Hoteli1.csv for needed data
#Just read your file and leave all the rest alone

datah = read.csv(file = "Hoteli1.csv", sep = ";", header = TRUE, stringsAsFactors = FALSE)


###############################################################################################
hotelid = gsub(" ", "", paste(datah$Region,datah$Location, datah$Hotel, sep = ""), fixed = TRUE)
datah = cbind(datah, hotelid)

#Do we want to download memberid
memberid=FALSE

##Download data
scrap(hotelid,1,1,"./data/",memberid)


##Gather data and produce TAOUPUT.txt with | delimiter
#providing subdir to data
gather("./data/")













