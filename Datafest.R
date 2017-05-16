library(lubridate)
library(jsonlite)
library(data.table)
library(dplyr)
library(geosphere)
library(ggplot2)
library(scales)

setwd("~/Desktop/ASADataFest2017 Data")
mydata <- data.table::fread("data.txt")
subset <- read.csv("small subsample.csv", stringsAsFactors = FALSE)
dest <- data.table::fread("dest.txt")
events <- read.csv("events.csv", stringsAsFactors = FALSE)

# fix the events lisitng by adding just the city to the first one I dont want to do any extra computation 
events$CITYNAME <- NA
for (i in 1:nrow(events)) {
  events[i,]$CITYNAME <- strsplit(events[i,]$CITY, ",")[[1]][1]
}



# convert LAT and LNG to double for comparision 

getEvents <- function() {
  # url 
  BASE_URL <- "http://api.songkick.com/api/3.0/events/"
  
  # event id
  API_KEY = "io09K9l3ebJxmxe2"
  EVENT_ID = c("22024628", "20292773", "21622608", "23011618", "22658908", "22851278", "23751228", "21001483", "22598433", "22354733", "21998268", "22143828", "23332923", "20826788", "22576873", "22640928", "21837283", "20499298", "22168623", "21441583")
  
  API_URL <- paste(BASE_URL, EVENT_ID[1], ".json?apikey=", API_KEY, sep = "")
  
  # create data frame
  events <- data.frame(matrix(ncol = 9, nrow = 0))
  names(events) <- c("ID", "EVENT", "SDAY", "EDAY", "CITY", "LAT", "LNG", "POPULARITY", "TYPE")
  
  for ( i in 1:length(EVENT_ID) ) {
    API_URL <- paste(BASE_URL, EVENT_ID[i], ".json?apikey=", API_KEY, sep = "")
    res <- fromJSON(API_URL)
    events[i,1] <- res$resultsPage$results$event$id
    events[i,2] <- res$resultsPage$results$event$displayName
    events[i,3] <- res$resultsPage$results$event$start$date
    events[i,4] <- res$resultsPage$results$event$end$date
    events[i,5] <- res$resultsPage$results$event$location$city
    events[i,6] <- res$resultsPage$results$event$location$lat
    events[i,7] <- res$resultsPage$results$event$location$lng
    events[i,8] <- res$resultsPage$results$event$popularity
    events[i,9] <- res$resultsPage$results$event$type
    Sys.sleep(2)
  }
  
  write.csv(events, "events.csv")
}

deg2rad <- function(deg) {
  return (deg * (pi/180))
}

calcDistance <- function(lat1, lng1, lat2, lng2) {
  R <- 6371 # Radius of the earth in km
  dLat <- deg2rad(lat2-lat1)
  dLng <- deg2rad(lng2-lng1)
  a <- sin(dLat/2) * sin(dLat/2) + cos(deg2rad(lat1)) * cos(deg2rad(lat2)) * sin(dLng/2) * sin(dLng/2)
  c <- 2 * atan2(sqrt(a), sqrt(1-a)); 
  d <- R * c; 
  return (d)
}

# clean up the dates
subset$CI <- ymd(subset$srch_ci)
subset$CO <- ymd(subset$srch_co)

subset$dayDiff <- difftime(subset$CO, subset$CI, units = "days")
table(subset$dayDiff[subset$dayDiff > 0 & subset$is_booking == 1])
quantile(subset$dayDiff[subset$dayDiff > 0 & subset$is_booking == 1], na.rm = TRUE)
quantile(subset$dayDiff[subset$dayDiff > 0 & subset$is_booking == 0], na.rm = TRUE)

merged <- left_join(mydata, dest, by = "srch_destination_id") # main merge
smallMerged <- left_join(subset, dest, by = "srch_destination_id")

austinOnly <- merged[which(grepl("Austin", merged$srch_destination_name)),]

date1 <- as.POSIXct("2015-03-16")
date2 <- as.POSIXct("2015-03-23")
int <- interval(date1, date2)
austinDates <- austinOnly[as.Date(austinOnly$srch_ci) %within% int, ]

table(austinDates[austinDates$is_booking == 1, ]$is_package)
table(austinDates[austinDates$is_booking == 0, ]$is_package)

# analysis 

# buyers 

austinBuyers <- austinDates[which(austinDates$is_booking == 1),]
table(austinDates$is_booking)
hist(as.Date(austinDates$srch_ci), format = "%Y-%m-%d", breaks = 11, freq=TRUE, col=rgb(1,0,0,0.5))
hist(as.Date(austinBuyers$srch_ci, format="%Y-%m-%d"), breaks = 11, freq=TRUE, add=TRUE, col=rgb(0,0,1,0.5))


table(austinBuyers$is_mobile)
table(austinBuyers$srch_adults_cnt)
table(austinBuyers$hist_price_band)
table(austinBuyers$distance_band)

austinBuyers_M <- austinBuyers[which(austinBuyers$hist_price_band == "L"), ]
table(austinBuyers_M$distance_band)
table(austinBuyers$prop_is_branded)

# clickers
austinClickers <- austinDates[which(austinDates$is_booking == 0),]
table(austinClickers$is_mobile)
table(austinClickers$srch_adults_cnt)
table(austinClickers$hist_price_band)
table(austinClickers$distance_band)


# Vegas 
vegasOnly <- merged[which(grepl("Vegas", merged$srch_destination_name)),]
#June 19 - June 21
date1 <- as.POSIXct("2015-06-15")
date2 <- as.POSIXct("2015-06-25")
int <- interval(date1, date2)
vegasDates <- vegasOnly[as.Date(vegasOnly$srch_ci) %within% int, ]

vegasDates$score <- exp(vegasDates$popular_cultural_festivals)
vegasDates$score <- exp(vegasDates$popular_cultural_livemusic)

vegasDates %>% group_by(srch_ci) %>% summarise(mean(score, na.rm = TRUE))

table(vegasDates$is_booking)
table(vegasDates$is_package[vegasDates$is_booking == 1])

vegasBuyers <- vegasDates[vegasDates$is_booking == 1, ]

hist(as.Date(vegasDates$srch_ci), format = "%Y-%m-%d", breaks = 10, freq=TRUE, col=rgb(1,0,0,0.5))
hist(as.Date(vegasBuyers$srch_ci, format="%Y-%m-%d"), breaks = 10, freq=TRUE, add=TRUE, col=rgb(0,0,1,0.5))


# Cochella
cochellaOnly <- merged[which(grepl("Indio", merged$srch_destination_name)),]
date1 <- as.POSIXct("2015-04-9")
date2 <- as.POSIXct("2015-04-13")
int <- interval(date1, date2)

cochellaDates <- cochellaOnly[as.Date(cochellaOnly$srch_ci) %within% int, ]
table(cochellaDates$is_booking)
table(cochellaDates$is_package)

# Firefly 
fireflyOnly <- merged[which(grepl("Dover", merged$srch_destination_name)),]
date1 <- as.POSIXct("2015-06-17")
date2 <- as.POSIXct("2015-06-23")
int <- interval(date1, date2)

fireflyDates <- fireflyOnly[as.Date(fireflyOnly$srch_ci) %within% int, ]
table(fireflyDates$is_booking)
table(fireflyDates$srch_children_cnt)

# Ultra 
miamiOnly <- merged[which(grepl("Miami", merged$srch_destination_name)),]

date1 <- as.POSIXct("2015-03-26")
date2 <- as.POSIXct("2015-03-31")
int <- interval(date1, date2)
miamiDates <- miamiOnly[as.Date(miamiOnly$srch_ci) %within% int, ]

table(miamiDates$is_package)

miamiBuyers <- miamiDates[miamiDates$is_booking == 1, ]
hist(as.Date(miamiDates$srch_ci), format = "%Y-%m-%d", breaks = 11, freq=TRUE, col=rgb(1,0,0,0.5))
hist(as.Date(miamiBuyers$srch_ci, format="%Y-%m-%d"), breaks = 11, freq=TRUE, add=TRUE, col=rgb(0,0,1,0.5))


# Stage Coach
stageCoachOnly <- merged[which(grepl("Indio", merged$srch_destination_name)),]
date1 <- as.POSIXct("2015-04-23")
date2 <- as.POSIXct("2015-04-27")
int <- interval(date1, date2)

stageCoacDates <- stageCoachOnly[as.Date(stageCoachOnly$srch_ci) %within% int, ]
table(stageCoacDates$is_booking)
table(stageCoacDates$is_package)

# Vans Warped Tour San Antonio 
vansWarpedTour <- merged[which(grepl("San Antonio", merged$srch_destination_name)),]
date1 <- as.POSIXct("2015-06-27")
date2 <- as.POSIXct("2015-06-29")
int <- interval(date1, date2)

vansWarpedTourDates <- vansWarpedTour[as.Date(vansWarpedTour$srch_ci) %within% int, ]
table(vansWarpedTourDates$is_booking)

# SummerFest 
summerFest <- merged[which(grepl("Milwaukee", merged$srch_destination_name)),]
date1 <- as.POSIXct("2015-06-20")
date2 <- as.POSIXct("2015-07-07")
int <- interval(date1, date2)

summerFestDates <- summerFest[as.Date(summerFest$srch_ci) %within% int, ]
table(summerFestDates$is_booking)

summerFestDates$score <- exp(summerFestDates$popular_cultural_livemusic)

summerFestDates %>% group_by(srch_ci) %>% summarise(mean(score, na.rm = TRUE))


# Lollapalooza
Lollapalooza <- merged[which(grepl("Chicago", merged$srch_destination_name)),]
date1 <- as.POSIXct("2015-07-20")
date2 <- as.POSIXct("2015-08-10")
int <- interval(date1, date2)

LollapaloozaDates <- Lollapalooza[as.Date(Lollapalooza$srch_ci) %within% int, ]
lollapaloozaBuyers <- LollapaloozaDates[which(LollapaloozaDates$is_booking == 1), ]

hist(as.Date(LollapaloozaDates$srch_ci), format = "%Y-%m-%d", breaks = 21, freq=TRUE, col=rgb(1,0,0,0.5))
hist(as.Date(lollapaloozaBuyers$srch_ci, format="%Y-%m-%d"), breaks = 21, freq=TRUE, add=TRUE, col=rgb(0,0,1,0.5))



#######################
# GG PLOT 
#######################

# Lollapalooza 2015
Lollapalooza <- merged[which(grepl("Grant Park", merged$srch_destination_name)),]
dateMinus4 <- as.POSIXct("2015-07-26")
datePlus4 <- as.POSIXct("2015-08-6")
int <- interval(dateMinus4, datePlus4)

LollapaloozaInDates <- Lollapalooza[as.Date(Lollapalooza$srch_ci) %within% int, ]
LollapaloozaInDates$CI <- as.Date(LollapaloozaInDates$srch_ci, format="%Y-%m-%d")
lollapaloozaBuyers <- LollapaloozaInDates[LollapaloozaInDates$is_booking == 1, ]

ggplot(data = LollapaloozaInDates, aes(x = CI)) + 
  geom_histogram(stat = "count", fill = "#328CC1", alpha = "0.8") + 
  geom_histogram(data = lollapaloozaBuyers, aes(x = CI), stat = "count", fill = "#2ECC71", alpha = "1") +
  labs(title = "Lollapalooza Hotel Demand", x = "Days", y = "Freq") + 
  scale_x_date(date_labels="%d", date_breaks = "1 day") + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), plot.title = element_text(hjust = 0.5), text = element_text(size=15))


LollapaloozaInDatesOut <- Lollapalooza[as.Date(Lollapalooza$srch_co) %within% int, ]
LollapaloozaInDatesOut$CO <- as.Date(LollapaloozaInDatesOut$srch_co, format="%Y-%m-%d")
lollapaloozaBuyers_out <- LollapaloozaInDatesOut[LollapaloozaInDatesOut$is_booking == 1, ]

ggplot(data = LollapaloozaInDatesOut, aes(x = CO)) + 
  geom_histogram(stat = "count", fill = "#328CC1", alpha = "0.8") + 
  geom_histogram(data = lollapaloozaBuyers_out, aes(x = CO), stat = "count", fill = "#2ECC71", alpha = "1") +
  labs(title = "Lollapalooza Hotel Demand", x = "Days", y = "Freq") + 
  scale_x_date(date_labels="%d", date_breaks = "1 day") + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), plot.title = element_text(hjust = 0.5), text = element_text(size=15))

# analysis
table(lollapaloozaBuyers$is_mobile)
table(lollapaloozaBuyers$is_package)
table(lollapaloozaBuyers$srch_adults_cnt)
table(lollapaloozaBuyers$hist_price_band)
table(lollapaloozaBuyers$distance_band)

austinBuyers_M <- austinBuyers[which(austinBuyers$hist_price_band == "L"), ]
table(austinBuyers_M$distance_band)
table(austinBuyers$prop_is_branded)

# clickers
lollapaloozaClickers <- LollapaloozaInDates[LollapaloozaInDates$is_booking == 0, ]
table(lollapaloozaClickers$is_mobile)
table(lollapaloozaClickers$srch_adults_cnt)
table(lollapaloozaClickers$hist_price_band)
table(lollapaloozaClickers$distance_band)

LollapaloozaInDates$score <- exp(LollapaloozaInDates$popular_cultural_festivals)
LollapaloozaInDates %>% group_by(srch_ci) %>% summarise(mean(score, na.rm = TRUE))


memphisOnly <- merged[which(grepl("Memphis", merged$srch_destination_name)),]
#May 1 - May 3
date1 <- as.POSIXct("2015-04-25")
date2 <- as.POSIXct("2015-05-08")
int <- interval(date1, date2)
BSM_Memphis <- memphisOnly[as.Date(memphisOnly$srch_ci) %within% int, ]
BSM_Memphis_Dates <- as.Date(BSM_Memphis$srch_ci, format="%Y-%m-%d")

#BSM_Memphis %>% group_by(srch_ci) %>% summarise(mean_dates = mean(popular_cultural_livemusic))

BSM_Memphis_buyers <- BSM_Memphis[BSM_Memphis$is_booking==1, ]

hist(BSM_Memphis_Dates, breaks=21, freq=TRUE, col=rgb(1,0,0,0.5))
hist(BSM_Memphis_buyers$srch_ci, breaks=21, freq=TRUE, add=TRUE, col=rgb(0,0,1,0.5))


# main data
merged$score <- exp(merged$popular_cultural_festivals)
merged %>% group_by(srch_ci) %>% summarise(mean(score, na.rm = TRUE))




# lines data graph 


lines <- read.csv("try3_four_cities.csv")
lines$Day <- as.factor(lines$Day)
lines$Day <- factor(lines$Day, levels(lines$Day)[c(7, 4, 5, 2, 1, 3, 6)])
lines$Counts[lines$City == "Chicago"] <- lines$Counts[lines$City == "Chicago"] / 10



ggplot(data = lines, aes(x = Day, y = Counts, group = interaction(City, Buyers), color = City)) + geom_point() + geom_line(size=1) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), plot.title = element_text(hjust = 0.5), text = element_text(size=15))


ggplot(data = lines, aes(x = Day, y = Counts, group = interaction(City, Buyers), color = City)) + geom_point() + geom_line(size=1) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), plot.title = element_text(hjust = 0.5), text = element_text(size=15))