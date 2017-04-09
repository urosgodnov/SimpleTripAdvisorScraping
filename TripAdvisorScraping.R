# load libraries
library(XML)
library(xml2)
library(pryr)
library(plyr)
library(rvest)
library(stringr)
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



##Download data
scrap(hotelid,1,NULL)


##Gather data and produce TAOUPUT.txt with | delimiter
#providing subdir to data
gather("./data/")












