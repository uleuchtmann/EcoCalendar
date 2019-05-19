lang <- "en"
myTimeZone <- "Europe/Berlin"
start <- substr(Sys.time(), 1, 16)
end <- substr(as.POSIXct(start, tz=myTimeZone) + 86400, 1, 16)

# assumes Bloomberg runs on 192.168.2.125
# assumes on that machine the following commands have been executed:
#   start C:\blp\DAPI\bbcomm.exe
#   socat TCP4-LISTEN:18194,fork TCP4:localhost:8194
options(blpAutoConnect=TRUE, blpHost="192.168.2.125", blpPort=18194L)
# if Bloomberg runs on localhost, use instead
#options(blpAutoConnect=TRUE)

library(Rblpapi)
library(timeSeries)

# create EcoList, a list of potential items for the calendar.
#   read the list of potential ECO items from 'ecoitems.csv'
EcoList <- read.csv("ecoitems.csv", stringsAsFactors = FALSE, row.names=1, comment.char = "#")
#   create a Bloomberg connection
con <- blpConnect()
#   import the necessary Bloomberg information 
bbgItems <- c("COUNTRY_ISO", "ECO_RELEASE_DT", "ECO_RELEASE_TIME", "ACTUAL_RELEASE",
              "OBSERVATION_PERIOD", "RT_BN_SURVEY_MEDIAN", "PREV_CLOSE_VAL",
              "PX_DISP_FORMAT_MAX_NUM_DEC")
EcoList <- cbind(EcoList, bdp(row.names(EcoList), bbgItems))
#   create proper date stamps
EcoList$release_date <- as.Date(EcoList$ECO_RELEASE_DT)
#   create proper time stamps (GMT)
createtime <- function(datestamp, timestamp){
  ifelse(is.null(datestamp) | timestamp=="",
         return(timeDate(NA)),
         return(timeDate(paste(datestamp, timestamp),
                format="%Y-%m-%d %H:%M:%S", zone=myTimeZone, FinCenter="GMT")))
}
unlist.timeDate <- function(x) {
  y <- x[[1]]
  for(i in 2:length(x)) y <- append(y, x[[i]])
  return(y)
}
EcoList$release_time <- unlist.timeDate(mapply(createtime, EcoList[,"ECO_RELEASE_DT"], EcoList[,"ECO_RELEASE_TIME"]))
#   create propper formatted strings from numerical variables 'RT_BN_SURVEY_MEDIAN' and 'PREV_CLOSE_VAL'
createnumber <- function(number, digits){
  ifelse(is.na(number),
         "-",
         sprintf(paste0("%.", digits, "f"), number))
}
EcoList$survey <- mapply(createnumber, EcoList[, "RT_BN_SURVEY_MEDIAN"], EcoList[, "PX_DISP_FORMAT_MAX_NUM_DEC"])
EcoList$last <- mapply(createnumber, EcoList[, "PREV_CLOSE_VAL"], EcoList[, "PX_DISP_FORMAT_MAX_NUM_DEC"])
#   clean up: delete unnecessary variables
EcoList <- EcoList[, !names(EcoList) %in% c("ECO_RELEASE_DT", "ECO_RELEASE_TIME", "RT_BN_SURVEY_MEDIAN", "PREV_CLOSE_VAL", "PX_DISP_FORMAT_MAX_NUM_DEC")]

# create EcoCalendar
#   define release start and end time of selection
start.time <- timeDate(start, zone=myTimeZone, FinCenter="GMT")
end.time <- timeDate(end, zone=myTimeZone, FinCenter="GMT")
#   select those items with 
#   - release_date not NA, and
#   - 'ACTUAL_RELEASE' is NA, and
#   - (when a release time is defined) release time between start.time and end.time or 
#     (when no release time is defined) release date between date of start.time and date of end.time.
selector <- !is.na(EcoList$release_date) &  
            is.na(EcoList$ACTUAL_RELEASE) &
            (
              ( is.na(EcoList$release_time) & EcoList$release_date>=as.Date(start.time) & EcoList$release_date<=as.Date(end.time)) |
              (!is.na(EcoList$release_time) & (EcoList$release_time>=start.time & EcoList$release_time<=end.time))
            )



