####	       	Replicating Data -- Peer Assessment 1	          	####
##			                   Glenn Kerbein			                    ##
##              			     March 4, 2017		    	                ##
####################################################################

## STEP 0: init environment
#clean the working environment
rm(list=ls(all=TRUE)) 



## STEP 1: init data for reading
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

## STEP 2: clean data
stormdata.clean <- stormdata %>% select(STATE, EVTYPE, FATAlities, INJURIES, PROPDMGEXP, CROPDMG, CROPDMGEXP)

