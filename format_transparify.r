#last updated Friday March 21, 2014
#This file reads in an Excel file and transforms it into the proper format to do Krippendorff's analysis
#this program is designed to work with two directories one with the data files a directory call krip_data
#and the .r files in a directory called krip_code
rm(list=ls())
library(gdata) #not necessary with not .xls files, which we should have
library(irr)
library(reshape2)
library(plyr)

#The two raters to be removed were RS and ZT. 


dpath <- "/Volumes/Optibay-1TB/Consulting/Transparify/krip_code"

setwd(dpath)

file <- "transparify_20140324_2.csv"

files <- dir("../krip_data", pattern = file)

sprintf("There are %d files in the data directory", length(files)) 

dataFiles <- list()

for(file in 1:length(files)){
    #sprintf("loading file %s", file)
    tmpFile <- read.csv(paste0("../krip_data/", files[1]), header=TRUE, stringsAsFactors=FALSE)
    dataFiles[[file]] <- tmpFile
}

#get to work for one iteration
try1 <- dataFiles[[1]]

#clean up the data for statistical calculation
try1 <- try1[ ,1:7]
names(try1) <- c("Continent", "Countries", "Rater", "TTName", "EOA", "Score", "adjScore")
try1$Rater <- as.factor(try1$Rater)
unqRaters <- sort(levels(try1$Rater))
cat("There are the following unique raters\n")
cat(unqRaters)

#check for duplicates -- this should quit the program, if there are any duplicates, since
#there should be no calculations made on data with errors in it
## testDups <- aggregate(try1[!try1$Rater %in% c("adj")], by=list(try1$Rater, try1$TTName), length)
## Dups <- testDups[testDups$Rater>1, 1:3]
## names(Dups)[1:3] <- c("Rater", "Org", "NumDups")
## if(nrow(Dups) > 0){
##     sprintf("You have %d records, where you have duplicate ratings from the same rater", nrow(Dups))
##     print(Dups[1:3])
## }
#for the time being arbitrarily remove one duplicate
try2 <- try1[!duplicated(try1[, c("Rater", "TTName")]), ]
try2 <- try2[order(try2$TTName), ]
allOrgs <-  unique(try2$TTName)

#only run once and never again
if(!("uuid" %in% names(try2))) {
    uniqueIDs <- replicate(length(allOrgs), system("uuidgen", intern=TRUE))
    orgUUIDs <- as.data.frame(cbind(TTName=allOrgs, uuid = uniqueIDs), stringsAsFactors=FALSE)
    write.csv(file=paste0("../krip_data/","uniqueTTCodes", Sys.Date(), ".csv"), orgUUIDs)
    try2 <- join(try2, orgUUIDs, by="TTName", type="left")
    try2[try2$Score==-1,c("Score", "adjScore")] <- 0
    dataWide <- dcast(try2,Rater ~ TTName, value.var="Score", fill =NA)
}

orgData <- dcast(try2,uuid+TTName~Rater, value.var="Score", fill =NA)
#dataWide <- dcast(try1, Rater ~ TTName, value.var="Score", fill=NaN, fun.aggregate = length)
#dataWide[is.na(dataWide)] <- NA
write.csv(file=paste0("../krip_data/","cleanedTransparifyDataWide", Sys.Date(), ".csv"), dataWide)
write.csv(file=paste0("../krip_data/","cleanedTransparifyData", Sys.Date(), ".csv"), try2)
write.csv(file=paste0("../krip_data/","uniqueTransparifyTTs", Sys.Date(), ".csv"), orgData)

##This can be used for further iterations of the data which come in the this form as they should
##this should be separated into a separate R script
#get a unique data sheet with the raters
raters <- c( "AE", "AP", "AWT", "HG", "JL", "KE", "MG", "MOK", "MS", "NM", "NP", "RB", "RS",  "RS",  "TA",  "TB",  "TBD", "UB",  "VB",  "ZT")

#remove all with only one rating
orgData$totalnonAdjRatings <- apply(orgData[, raters], 1, function(x) length(na.omit(x)))
orgData$totalRatings <- apply(orgData[, c("adj",raters)], 1, function(x) length(na.omit(x)))

oneRateOrgs <- orgData[orgData$totalnonAdjRatings <2,"TTName"]
rateableTTs <- orgData$TTName[!(orgData$TTName %in% oneRateOrgs)]

##Rate orgs
krippDataAll <- as.matrix(dataWide[!(dataWide$Rater  %in% c("adj")),  rateableTTs])
krippDataGood <- as.matrix(dataWide[!(dataWide$Rater  %in% c("adj", "VB", "ZT")), rateableTTs ])
tpfyKrippAlphaAll <- kripp.alpha(x=krippDataAll, method ="ordinal")
tpfyKrippAlphaGood <- kripp.alpha(x=krippDataGood, method ="ordinal")

raterStats <- ddply(try2[!try2$Rater %in% c("adj"), ], .(Rater) ,c(function(x) sum(x$Score==x$adjScore, na.rm=TRUE), function(x) sum(x$Score!=x$adjScore, na.rm=TRUE)))
names(raterStats) <- c("Rater", "numRight", "numWrong")

raterStats$Total <- raterStats$numRight+raterStats$numWrong
raterStats$pctCorrect <- raterStats$numRight/raterStats$Total

sum(raterStats$numRight)/sum(raterStats$Total)

write.csv(file=paste0("../krip_data/","ratersStats", Sys.Date(), ".csv"), raterStats)


#write.csv(file=file.path("../krip_data/", paste0("transpartifyRating-", Sys.Date(), ".csv")))

mean(raterStats$pctCorrect)
sum(raterStats$pctCorrect*raterStats$Total)/sum(raterStats$Total)

#write.csv(file=file.path("../krip_data/", paste0("onlyOneRater-", Sys.Date(), ".csv")), orgData[orgData$totalnonAdjRatings <2,])
