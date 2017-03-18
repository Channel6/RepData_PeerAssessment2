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
stormdata.clean$EVTYPE <- gsub("^(HEAT).*", "HEAT", stormdata.clean$EVTYPE)
stormdata.clean$EVTYPE <- gsub("^(RECORD HEAT).*", "HEAT", stormdata.clean$EVTYPE)
stormdata.clean$EVTYPE <- gsub("^(EXTREME HEAT).*", "HEAT", stormdata.clean$EVTYPE)
stormdata.clean$EVTYPE <- gsub("^(Heat).*", "HEAT", stormdata.clean$EVTYPE)
stormdata.clean$EVTYPE <- gsub("^(EXCESSIVE HEAT).*", "HEAT", stormdata.clean$EVTYPE)
stormdata.clean$EVTYPE <- gsub("^(TSTM).*", "THUNDER STORM", stormdata.clean$EVTYPE)
stormdata.clean$EVTYPE <- gsub("^(THUNDERSTORM).*", "THUNDER STORM", stormdata.clean$EVTYPE)
stormdata.clean$EVTYPE <- gsub("^(TROPICAL STORM).*", "TROPICAL STORM", stormdata.clean$EVTYPE)
stormdata.clean$EVTYPE <- gsub("^(FLASH FLOOD).*", "FLOOD", stormdata.clean$EVTYPE)
stormdata.clean$EVTYPE <- gsub("^(WIND).*", "WIND", stormdata.clean$EVTYPE)
stormdata.clean$EVTYPE <- gsub("^(STRONG WIND).*", "WIND", stormdata.clean$EVTYPE)
stormdata.clean$EVTYPE <- gsub("^(HIGH WIND).*", "WIND", stormdata.clean$EVTYPE)
stormdata.clean$EVTYPE <- gsub("^(HURRICANE).*", "HURICCANE", stormdata.clean$EVTYPE)
stormdata.clean$EVTYPE <- gsub("^(SNOW).*", "SNOW", stormdata.clean$EVTYPE)
stormdata.clean$EVTYPE <- gsub("^(HEAVY SNOW).*", "SNOW", stormdata.clean$EVTYPE)
stormdata.clean$EVTYPE <- gsub("^(FIRE).*", "FIRE", stormdata.clean$EVTYPE)
stormdata.clean$EVTYPE <- gsub("^(WILD/FOREST FIRE).*", "FIRE", stormdata.clean$EVTYPE)
stormdata.clean$EVTYPE <- gsub("^(WILDFIRE).*", "FIRE", stormdata.clean$EVTYPE)
stormdata.clean$EVTYPE <- gsub("^(WILD FIRES).*", "FIRE", stormdata.clean$EVTYPE)
stormdata.clean$EVTYPE <- gsub("^(HAIL).*", "HAIL", stormdata.clean$EVTYPE)
stormdata.clean$EVTYPE <- gsub("^(BLIZZARD).*", "BLIZZARD", stormdata.clean$EVTYPE)
stormdata.clean$EVTYPE <- gsub("^(COLD).*", "COLD", stormdata.clean$EVTYPE)
stormdata.clean$EVTYPE <- gsub("^(WINTER WEATHER).*", "COLD", stormdata.clean$EVTYPE)
stormdata.clean$EVTYPE <- gsub("^(EXTREME COLD).*", "COLD", stormdata.clean$EVTYPE)
stormdata.clean$EVTYPE <- gsub("^(RIP).*", "RIP", stormdata.clean$EVTYPE)
stormdata.clean$EVTYPE <- gsub("^(FOG).*", "FOG", stormdata.clean$EVTYPE)
stormdata.clean$EVTYPE <- gsub("^(DENSE FOG).*", "FOG", stormdata.clean$EVTYPE)
stormdata.clean$EVTYPE <- gsub("^(AVALANCHE).*", "AVALANCHE", stormdata.clean$EVTYPE)
stormdata.clean$EVTYPE <- gsub("^(AVALANCE).*", "AVALANCHE", stormdata.clean$EVTYPE)
stormdata.clean$EVTYPE <- gsub("^(RAIN).*", "RAIN", stormdata.clean$EVTYPE)
stormdata.clean$EVTYPE <- gsub("^(HEAVY RAIN).*", "RAIN", stormdata.clean$EVTYPE)
stormdata.clean$EVTYPE <- gsub("^(HIGH SURF).*", "SURF", stormdata.clean$EVTYPE)
stormdata.clean$EVTYPE <- gsub("^(HEAVY SURF).*", "SURF", stormdata.clean$EVTYPE)
stormdata.clean$EVTYPE <- gsub("^(SURF).*", "SURF", stormdata.clean$EVTYPE)

## Results
# plot number of fatalities with the most harmful event type
fatalities <- aggregate(FATALITIES ~ EVTYPE, data=stormdata.clean, sum)
fatalities <- fatalities[order(-fatalities$FATALITIES), ][1:10, ]
fatalities$EVTYPE <- factor(fatalities$EVTYPE, levels = fatalities$EVTYPE)

ggplot(fatalities, aes(x = EVTYPE, y = FATALITIES)) + 
    geom_bar(stat = "identity", fill = "blue") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    xlab("Event Type") + ylab("Fatalities") + ggtitle("Number of fatalities by top 10 Weather Events")

injuries <- aggregate(INJURIES ~ EVTYPE, data=tidyNOAA, sum)
injuries <- injuries[order(-injuries$INJURIES), ][1:10, ]
injuries$EVTYPE <- factor(injuries$EVTYPE, levels = injuries$EVTYPE)

ggplot(injuries, aes(x = EVTYPE, y = INJURIES)) + 
    geom_bar(stat = "identity", fill = "blue") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    xlab("Event Type") + ylab("Injuries") + ggtitle("Number of injuries by top 10 Weather Events")


#which types of events have the greatest economic consequences?

