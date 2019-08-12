library(data.table)
library(ggplot2)
library(ggtext)
library(ggthemes)
library(gganimate)
library(plotly)
library(lubridate)
library(dplyr)
library(png)
library(grid)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
watermrk <- readPNG(source = "images/watermark.png")
watermark <- rasterGrob(watermrk, interpolate = TRUE)
rm(watermrk)

circuits <- fread("data/circuits.csv")
drivers <- fread("data/drivers.csv")
lapTimes <- fread("data/lapTimes.csv")
races <- fread("data/races.csv")
results <- fread("data/results.csv")

generateRaceDriverLaps <- function(raceIdEval) {
  raceLaps <- merge(lapTimes[raceId == raceIdEval], drivers[, .(driverId, surname)])
  raceLaps[, seconds := milliseconds / 1000]
  raceLaps[, totalRaceTime := cumsum(milliseconds), by = driverId]
  
  return(raceLaps)
}

raceLapsBoxPlotsMinMax <- function(race_Id) {
  # returns min and max laps times across all laps' boxplot.stats[1,5]
  driverLapsNormWhiskers <- lapTimes[raceId == race_Id, .(min = boxplot.stats(milliseconds)$stats[1],
                                                               max = boxplot.stats(milliseconds)$stats[5]),
                                       by = lap]
  minTime <- min(driverLapsNormWhiskers[, min])
  #maxTime <- max(driverLapsNormWhiskers[, max]) #+ (30 * 1000) # add 30 seconds
  maxTime <- 3*60*1000 #3 minutes
  minMax <- c(minTime - (minTime * 0.01), maxTime * 1.05)

  return(minMax)
}

driverLapTimesToolTip <- function(pp){
  for (i in 1:length(pp[["x"]][["data"]])){
    if (length(pp[["x"]][["data"]][[i]][["text"]]) > 0){
      #extract and format lap times
      dataList <- pp[["x"]][["data"]][[i]][["text"]]
      
      driverName <- stringr::str_extract(dataList, "factor\\(surname\\): [^<]*")
      driverName <- gsub("factor(surname):", "", driverName, fixed = TRUE)
      driverName <- gsub(" ", "", driverName)
      
      lapNumber <- stringr::str_extract(dataList, "lap: [^<]*")
      lapNumber <- gsub("lap:", "", lapNumber)
      lapNumber <- gsub(" ", "", lapNumber)
      
      laptimes <- stringr::str_extract(dataList, "[0-9]{1,4}\\.[0-9]{1,4}")
      laptimes <- lubridate::as.period(lubridate::as.duration(as.numeric(laptimes)))
      laptimes <- gsub("M ", ":", laptimes, fixed = TRUE)
      laptimes <- gsub("S", "", laptimes, fixed = TRUE)
      laptimes <- gsub("(:)([0-9]{1})(\\.)", "\\10\\2\\3", laptimes)
      laptimes <- substring(laptimes, 1, 8)
      # append 0's after decimal point so it always displays with at least 3 decimal places
      for (j in 1:length(laptimes)){
        if (!is.na(laptimes[j])){
          decimalPlaces <- stringr::str_extract(laptimes[j], "\\..*")
          if (is.na(decimalPlaces)){
            laptimes[j] <- paste(laptimes[j], "000", sep = "")
          } else if (nchar(decimalPlaces) == 3){
            laptimes[j] <- paste(laptimes[j], "0", sep = "")
          } else if (nchar(decimalPlaces) == 2){
            laptimes[j] <- paste(laptimes[j], "00", sep = "")
          } else if (nchar(decimalPlaces) == 1){
            laptimes[j] <- paste(laptimes[j], "000", sep = "")
          }
        }
      }
      
      mungedList <- paste("<b>Driver:</b> ", driverName,
                          "<br /><b>Lap:</b> ", lapNumber,
                          "<br /><b>Lap Time:</b> ", laptimes,
                          sep = "")
      
      # driver name label
      pp[["x"]][["data"]][[i]][["text"]] <- mungedList
    }
  }
  return(pp)
}

getTrackImage <- function(circuitId){
  returnImage <- watermark
  # Brazil GP
  if(circuitId == 18){
    img <- readPNG(source = "images/brazil.png") # need to setup data for these images
    returnImage <- rasterGrob(img, interpolate = TRUE)
  }
  
  return(returnImage)
}

convertLapTimeStringToSeconds <- function(lapTimeString){
  minutes <- as.numeric(stringr::str_extract(lapTimeString, "^[0-9]{1,2}"))
  seconds <- stringr::str_extract(lapTimeString, ":[0-9]{1,2}\\.[0-9]{1,3}")
  seconds <- as.double(gsub(":", "", seconds, fixed = TRUE))
  totalSeconds <- (minutes * 60) + seconds
  return(totalSeconds)
}
