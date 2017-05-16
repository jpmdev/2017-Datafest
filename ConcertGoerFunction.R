### FUNCTIONS

# creates the custom event dataset
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

# Is user a festival goer
isFestivalGoer <- function(userDataRow, events) {
  
  score <- 0
  
  # need to see if they are going to a city within an event taking palce
  # is the user going in the same time frame
  event <- findEvent(userDataRow, events) # this will return all the necesary event data
  # check if is not empty
  if (class(event) == "data.frame") {
    # now we check if the user is going in the same time frame
    if (!is.atomic(event$SDAY)) {return (FALSE)}
    if (isSameTimeFrame(userDataRow$srch_ci, event)) {
      score <- score + 5 # they are going in the same time frame big addition
      # now we need to find out how close they are to the actual event 
      score <- score + howCloseToEvent(userDataRow$srch_destination_latitude, userDataRow$srch_destination_longitude, event$LAT, event$LNG)
      if (exp(userDataRow$popular_cultural_livemusic) > 0.1216942) {
        score <- score + 1
      }
    } else { 
      # then they arent going to the place and are not concert goers
      return(FALSE)
    }
  } else {
    # not going to the same city as the event
    return(FALSE)
  }
  
  return(score * event$POPULARITY)
}

# finds the matching event for user
findEvent <- function(hotelCity, data) {
  for (i in 1:nrow(data)) {
    if (grepl(data[i,]$CITYNAME, hotelCity$srch_destination_name) == TRUE) {
      return (data[i,])
    } 
  }
  return (FALSE)
}

# checks if user is in the same time frame as the event
isSameTimeFrame <- function(userCI, event) {
  eventCI <- as.Date(event$SDAY) - 2 # we add a plus and minus
  eventCO <- as.Date(event$EDAY) + 2 # this can be optimized later
  
  eventTimeFrame <- interval(eventCI, eventCO)
  userCI <- as.Date(userCI)
  if (userCI %within% eventTimeFrame) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# calculates how close the user's hotel is to the venue
deg2rad <- function(deg) {
  return (deg * (pi/180))
} # support function for 
howCloseToEvent <- function(hotelLat, hotelLng, eventLat, eventLng) {
  lat1 <- as.numeric(hotelLat)
  lng1 <- as.numeric(hotelLng)
  lat2 <- as.numeric(eventLat)
  lng2 <- as.numeric(eventLng)
  
  R <- 6371 # Radius of the earth in km
  dLat <- deg2rad(lat2-lat1)
  dLng <- deg2rad(lng2-lng1)
  a <- sin(dLat/2) * sin(dLat/2) + cos(deg2rad(lat1)) * cos(deg2rad(lat2)) * sin(dLng/2) * sin(dLng/2)
  c <- 2 * atan2(sqrt(a), sqrt(1-a)); 
  d <- R * c; 
  if ( d < 8.04672 ) {
    return(5)
  } else if ( d < 16.0934 ) {
    return(4)
  } else if ( d < 24.1402 ) {
    return(3)
  } else if ( d < 32.1869 ) {
    return(2)
  } else {
    return(1)
  }
}



######## INIT FILES

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
merged <- left_join(mydata, dest, by = "srch_destination_id") # main merge
smallMerged <- left_join(subset, dest, by = "srch_destination_id")

# fix the events lisitng by adding just the city to the first one I dont want to do any extra computation 
events$CITYNAME <- NA
for (i in 1:nrow(events)) {
  events[i,]$CITYNAME <- strsplit(events[i,]$CITY, ",")[[1]][1]
}

###### APPLICATION

# Lollapalooza 2015
Lollapalooza <- merged[which(grepl("Grant Park", merged$srch_destination_name)),]
dateMinus4 <- as.POSIXct("2015-07-29")
datePlus4 <- as.POSIXct("2015-08-04")
int <- interval(dateMinus4, datePlus4)
LollapaloozaInDates <- Lollapalooza[as.Date(Lollapalooza$srch_ci) %within% int, ]

#SAMPLE
sample <- LollapaloozaInDates[sample(1:nrow(LollapaloozaInDates), 50, replace = TRUE), ]
truth <- LollapaloozaInDates 
truth$SCORE <- 0

for (i in 1:nrow(truth)) {
  truth[i,]$SCORE <- isFestivalGoer(truth[i,], events)
}

table(truth$SCORE)


### VEGAS
vegasOnly <- merged[which(grepl("Vegas", merged$srch_destination_name)),]
date1 <- as.POSIXct("2015-06-15")
date2 <- as.POSIXct("2015-06-25")
int <- interval(date1, date2)
vegasDates <- vegasOnly[as.Date(vegasOnly$srch_ci) %within% int, ]
sample <- vegasDates[sample(1:nrow(vegasDates), 300, replace = TRUE), ]
sample$SCORE <- 0

for (i in 1:nrow(sample)) {
  if (!anyNA(sample[1,])) {
    sample[i,]$SCORE <- isFestivalGoer(sample[i,], events)
  }
}

sample$SCORE
table(sample$SCORE)


# Austin 

austinOnly <- merged[which(grepl("Austin", merged$srch_destination_name)),]
date1 <- as.POSIXct("2015-03-12")
date2 <- as.POSIXct("2015-03-28")
int <- interval(date1, date2)
austinDates <- austinOnly[as.Date(austinOnly$srch_ci) %within% int, ]
sample <- austinDates[sample(1:nrow(austinDates), 300, replace = TRUE), ]
sample$SCORE <- 0

for (i in 1:nrow(sample)) {
  if (!anyNA(sample[1,])) {
    sample[i,]$SCORE <- isFestivalGoer(sample[i,], events)
  }
}

table(sample$SCORE)


# Memphis 
memphisOnly <- merged[which(grepl("Memphis", merged$srch_destination_name)),]
date1 <- as.POSIXct("2015-04-27")
date2 <- as.POSIXct("2015-05-08")
int <- interval(date1, date2)
memphisDates <- memphisOnly[as.Date(memphisOnly$srch_ci) %within% int, ]
sample <- memphisDates[sample(1:nrow(memphisDates), 300, replace = TRUE), ]
sample$SCORE <- 0

for (i in 1:nrow(sample)) {
  if (!anyNA(sample[1,])) {
    sample[i,]$SCORE <- isFestivalGoer(sample[i,], events)
  }
}

table(sample$SCORE)

