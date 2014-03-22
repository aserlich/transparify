#last updated Friday March 21, 2014
#This file reads in an Excel file and transforms it into the proper format to do Krippendorff's analsys
#this program is designed to work with two directories one with the data files a directory call krip_data
#and the .r files in a directory called krip_code

library(gdata) #not necessary with not .xls files, which we should have
library(irr)
library(reshape2)

#setwd("/Volumes/Optibay-1TB/Consulting/Transparify/krip_code")
mypath <- getwd()

files <- dir("../krip_data", pattern = "*.csv")

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

dataWide <- dcast(try1,Rater ~ TTName, value.var="Score", fill =NA, fun.aggregate = length)
write.csv(file="../krip_data/transpartifyWide.csv", dataWide)
write.csv(file="../krip_data/transpartifyLong.csv", try1)
testDups <- aggregate(try1, by=list(try1$Rater, try1$TTName), length)

#check for duplicates -- this should quit the program, if there are any duplicates, since
#there should be no calculations made on data with errors in it
Dups <- testDups[testDups$Continent>1, 1:3]
names(Dups)[1:3] <- c("Rater", "Org", "NumDups")
if(nrow(Dups) > 0){
    sprintf("You have %d records, where you have duplicate ratings from the same rater", nrow(Dups))
    print(Dups[1:3])
}
#for the time being arbitrarily remove one duplicate
try2 <- try1[!duplicated(try1[, c("Rater", "TTName")]), ]
dataWide <- dcast(try2,Rater ~ TTName, value.var="Score", fill =NA)
write.csv(file="../krip_data/transpartifyWide.csv", dataWide)
krippData <- as.matrix(dataWide[dataWide$Rater != "adj", 2:ncol(dataWide) ])
tpfyKrippAlpha <- kripp.alpha(x=krippData, method ="ordinal") 
write.csv(file=file.path("../krip_data/", paste0("transpartifyRating-", Sys.Date(), ".csv")))
