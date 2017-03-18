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
#subset data
stormdata.clean <- stormdata[,c('EVTYPE','FATALITIES','INJURIES', 'PROPDMG', 'PROPDMGEXP', 'CROPDMG', 'CROPDMGEXP', 'STATE')]
#scale data appropriately
# Convert H, K, M, B units to calculate Property Damage 
stormdata.clean$PROPDMGNUM = 0
stormdata.clean[stormdata.clean$PROPDMGEXP == "H", ]$PROPDMGNUM = stormdata.clean[stormdata.clean$PROPDMGEXP == "H", ]$PROPDMG * 10^2
stormdata.clean[stormdata.clean$PROPDMGEXP == "K", ]$PROPDMGNUM = stormdata.clean[stormdata.clean$PROPDMGEXP == "K", ]$PROPDMG * 10^3
stormdata.clean[stormdata.clean$PROPDMGEXP == "M", ]$PROPDMGNUM = stormdata.clean[stormdata.clean$PROPDMGEXP == "M", ]$PROPDMG * 10^6
stormdata.clean[stormdata.clean$PROPDMGEXP == "B", ]$PROPDMGNUM = stormdata.clean[stormdata.clean$PROPDMGEXP == "B", ]$PROPDMG * 10^9
# Convert H, K, M, B units to calculate Crop Damage
stormdata.clean$CROPDMGNUM = 0
stormdata.clean[stormdata.clean$CROPDMGEXP == "H", ]$CROPDMGNUM = stormdata.clean[stormdata.clean$CROPDMGEXP == "H", ]$CROPDMG * 10^2
stormdata.clean[stormdata.clean$CROPDMGEXP == "K", ]$CROPDMGNUM = stormdata.clean[stormdata.clean$CROPDMGEXP == "K", ]$CROPDMG * 10^3
stormdata.clean[stormdata.clean$CROPDMGEXP == "M", ]$CROPDMGNUM = stormdata.clean[stormdata.clean$CROPDMGEXP == "M", ]$CROPDMG * 10^6
stormdata.clean[stormdata.clean$CROPDMGEXP == "B", ]$CROPDMGNUM = stormdata.clean[stormdata.clean$CROPDMGEXP == "B", ]$CROPDMG * 10^9
# Get EVTYPEs
event.type<-stormdata.clean %>% 
	group_by(EVTYPE) %>%
	summarize(n=n()) %>% 
	arrange(desc(n))
event.type
# rename events
stormdata.clean$Event <- gsub("^(HEAT).*", "HEAT", stormdata.clean$EVTYPE)
noaa.storm.clean$Event <- gsub("^(RECORD HEAT).*", "HEAT", noaa.storm.clean$Event)
noaa.storm.clean$Event <- gsub("^(EXTREME HEAT).*", "HEAT", noaa.storm.clean$Event)
noaa.storm.clean$Event <- gsub("^(Heat).*", "HEAT", noaa.storm.clean$Event)
noaa.storm.clean$Event <- gsub("^(EXCESSIVE HEAT).*", "HEAT", noaa.storm.clean$Event)
noaa.storm.clean$Event <- gsub("^(TSTM).*", "THUNDER STORM", noaa.storm.clean$Event)
noaa.storm.clean$Event <- gsub("^(THUNDERSTORM).*", "THUNDER STORM", noaa.storm.clean$Event)
noaa.storm.clean$Event <- gsub("^(TROPICAL STORM).*", "TROPICAL STORM", noaa.storm.clean$Event)
noaa.storm.clean$Event <- gsub("^(FLASH FLOOD).*", "FLOOD", noaa.storm.clean$Event)
noaa.storm.clean$Event <- gsub("^(WIND).*", "WIND", noaa.storm.clean$Event)
noaa.storm.clean$Event <- gsub("^(STRONG WIND).*", "WIND", noaa.storm.clean$Event)
noaa.storm.clean$Event <- gsub("^(HIGH WIND).*", "WIND", noaa.storm.clean$Event)
noaa.storm.clean$Event <- gsub("^(HURRICANE).*", "HURICCANE", noaa.storm.clean$Event)
noaa.storm.clean$Event <- gsub("^(SNOW).*", "SNOW", noaa.storm.clean$Event)
noaa.storm.clean$Event <- gsub("^(HEAVY SNOW).*", "SNOW", noaa.storm.clean$Event)
noaa.storm.clean$Event <- gsub("^(FIRE).*", "FIRE", noaa.storm.clean$Event)
noaa.storm.clean$Event <- gsub("^(WILD/FOREST FIRE).*", "FIRE", noaa.storm.clean$Event)
noaa.storm.clean$Event <- gsub("^(WILDFIRE).*", "FIRE", noaa.storm.clean$Event)
noaa.storm.clean$Event <- gsub("^(WILD FIRES).*", "FIRE", noaa.storm.clean$Event)
noaa.storm.clean$Event <- gsub("^(HAIL).*", "HAIL", noaa.storm.clean$Event)
noaa.storm.clean$Event <- gsub("^(BLIZZARD).*", "BLIZZARD", noaa.storm.clean$Event)
noaa.storm.clean$Event <- gsub("^(COLD).*", "COLD", noaa.storm.clean$Event)
noaa.storm.clean$Event <- gsub("^(WINTER WEATHER).*", "COLD", noaa.storm.clean$Event)
noaa.storm.clean$Event <- gsub("^(EXTREME COLD).*", "COLD", noaa.storm.clean$Event)
noaa.storm.clean$Event <- gsub("^(RIP).*", "RIP", noaa.storm.clean$Event)
noaa.storm.clean$Event <- gsub("^(FOG).*", "FOG", noaa.storm.clean$Event)
noaa.storm.clean$Event <- gsub("^(DENSE FOG).*", "FOG", noaa.storm.clean$Event)
noaa.storm.clean$Event <- gsub("^(AVALANCHE).*", "AVALANCHE", noaa.storm.clean$Event)
noaa.storm.clean$Event <- gsub("^(AVALANCE).*", "AVALANCHE", noaa.storm.clean$Event)
noaa.storm.clean$Event <- gsub("^(RAIN).*", "RAIN", noaa.storm.clean$Event)
noaa.storm.clean$Event <- gsub("^(HEAVY RAIN).*", "RAIN", noaa.storm.clean$Event)
noaa.storm.clean$Event <- gsub("^(HIGH SURF).*", "SURF", noaa.storm.clean$Event)
noaa.storm.clean$Event <- gsub("^(HEAVY SURF).*", "SURF", noaa.storm.clean$Event)
noaa.storm.clean$Event <- gsub("^(SURF).*", "SURF", noaa.storm.clean$Event)

