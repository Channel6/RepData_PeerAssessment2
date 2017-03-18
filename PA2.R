####	       	Replicating Data -- Peer Assessment 1		 ###
##			     Glenn Kerbein			  ##
##			     March 4, 2017			  ##
####################################################################

## STEP 0: init environment
#clean the working environment
rm(list=ls(all=TRUE)) 

## STEP 1: get required packages
library(dplyr, quietly = T, warn.conflicts = F)
library(ggplot2, quietly = T, warn.conflicts = F)
# required variables
colorpalette <- c("#999999", "#E69F00", "#D55E00", "#0072B2", "#F0E442", "#009E73", "#000000", "#CC79A7","#56B4E9")

## STEP 2: init data for reading
# Get required files
datafile <- "storm.csv.bz2"
if(!file.exists(datafile)){
  url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
  download.file(
    url,
    destfile = datafile,
    method = "curl",
    quiet = TRUE
  )
}

stormdata <- read.csv(datafile, header = TRUE)

## STEP 3: clean data
stormdata.clean <- stormdata %>% select(STATE, EVTYPE, FATAlities, INJURIES, PROPDMGEXP, CROPDMG, CROPDMGEXP)

