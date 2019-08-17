library(data.table)
library(ggplot2)
library(ggmap)
library(ggtext)
library(ggthemes)
library(gganimate)
# library(magick)
library(plotly)
library(lubridate)
library(dplyr)
library(png)
library(grid)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
watermrk <- readPNG(source = "images/watermark.png")
watermark <- rasterGrob(watermrk, interpolate = TRUE)
rm(watermrk)

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

getTrackImage <- function(circuit_Id, transparencyVal){
  # have a default image to return in case one is not found
  returnImage <- watermark
  
  if (circuit_Id %in% circuits[!is.na(imageSource), circuitId]){
    img <- readPNG(source = circuits[circuitId == circuit_Id, imageSource])
    img2 <- matrix(rgb(img[,,1],img[,,2],img[,,3], img[,,4] * transparencyVal), nrow=dim(img)[1])
    returnImage <- rasterGrob(img2, interpolate = TRUE)
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

convertSecondsToDisplayTime <- function(secondsToConvert){
  totalMinutes <- floor(secondsToConvert / 60)
  remainingSeconds <- secondsToConvert - (totalMinutes * 60)
  if (remainingSeconds < 10){
    displayTime <- paste(totalMinutes, ":0", sprintf("%.3f", round(remainingSeconds,3)), sep = "")
  } else {
    displayTime <- paste(totalMinutes, ":", sprintf("%.3f", round(remainingSeconds,3)), sep = "")  
  }
  return(displayTime)
}

generateCircuitWorldMap <- function(circuit_Id){
  worldMap <- borders("world", colour = "gray40", fill = "gray42")
  
  circuitMap <- ggplot(circuits[circuitId == circuit_Id]) + 
    worldMap +
    coord_cartesian(ylim = c(-50, 90)) +
    geom_point(aes(x = lng, y = lat), colour = "red3", size = 10, shape = 10, stroke = 3) +
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          panel.border = element_blank(),
          panel.background = element_rect(fill = "transparent"),
          plot.background = element_rect(fill = "transparent"))
  
  trackMapLocation <- "images/tempTrackMap.png"
  ggsave(plot = circuitMap, trackMapLocation, device = "png", height = 9, width = 16, bg = "transparent")
  
  img <- readPNG(source = trackMapLocation)
  returnImage <- rasterGrob(img, interpolate = TRUE)
  
  return(returnImage)
}

loadF1Data <- function(){
  circuits <<- fread("data/circuits.csv")
  names(circuits) <<- c("circuitId","circuitRef","name","location","country","lat","lng","alt","url")
  
  drivers <<- fread("data/driver.csv")
  names(drivers) <<- c("driverId","driverRef","number","code","forename","surname","dob","nationality","url")
  
  lapTimes <<- fread("data/lap_times.csv")
  names(lapTimes) <<- c("raceId","driverId","lap","position","time","milliseconds")
  
  races <<- fread("data/races.csv")
  names(races) <<- c("raceId","year","round","circuitId","name","date","url")
  
  results <<- fread("data/results.csv")
  names(results) <<- c("resultId","raceId","driverId","constructorId","number","grid","position","positionText","positionOrder","points","laps","time","milliseconds","fastestLap","rank","fastestLapTime","fastestLapSpeed","statusId")
  
  ### milliseconds displayed as seconds makes lubridate functions easier
  lapTimes[, seconds := milliseconds / 1000]
  
  ### create new table for fastest laps, because some of these are missing from core data
  ### note: this throws warning: "In eval(jsub, SDenv, parent.frame()) : NAs introduced by coercion"
  ### but it's fine, it's just a warning, intended behavior here.
  racesWithTimes <- unique(lapTimes[, raceId])
  raceSpeeds <- results[raceId %in% racesWithTimes][, fastestLapSpeed := as.numeric(fastestLapSpeed)]
  raceSpeeds <- raceSpeeds[!is.na(fastestLapSpeed), .(highestSpeed = max(fastestLapSpeed)), by = raceId]
  
  fastestLaps <<- lapTimes[, .(milliseconds = min(milliseconds)), by = raceId]
  fastestLaps[, seconds := milliseconds / 1000]
  for (i in 1:nrow(fastestLaps)) {
    fastestLaps[i, displayTime := convertSecondsToDisplayTime(seconds)]
  }
  fastestLaps[, fastestLapSpeed := as.numeric()]
  for (i in raceSpeeds[, raceId]) {
    speedVal <- raceSpeeds[raceId == i, highestSpeed]
    fastestLaps[raceId == i, fastestLapSpeed := speedVal]
  }
  fastestLaps[, fastestLapSpeed := sprintf("%.3f", round(fastestLapSpeed,3))]
  fastestLaps[is.na(fastestLapSpeed), fastestLapSpeed := "No Data"]
  
  ### fix circuit names
  circuits[circuitId == 18, name := "Autódromo José Carlos Pace"]
  circuits[circuitId == 20, name := "Nürburgring"]
  
  ### attach circuit image source
  if (!"imageSource" %in% names(circuits)){
    circuits[, imageSource := character()]
  }
  circuits[circuitId == 1, imageSource := "images/tracks/Albert_Park.png"]
  circuits[circuitId == 2, imageSource := "images/tracks/Sepang.png"]
  circuits[circuitId == 3, imageSource := "images/tracks/Sakhir1.png"]
  circuits[circuitId == 4, imageSource := "images/tracks/CircuitDeCatalunya.png"]
  circuits[circuitId == 6, imageSource := "images/tracks/Monte_Carlo_Formula_1_track_map.png"]
  circuits[circuitId == 7, imageSource := "images/tracks/CircuitGillesVilleneuve.png"]
  circuits[circuitId == 8, imageSource := "images/tracks/Magny-Cours.png"]
  circuits[circuitId == 9, imageSource := "images/tracks/SilverstoneArena2010.png"]
  circuits[circuitId == 10, imageSource := "images/tracks/Hockenheimring2002.png"]
  circuits[circuitId == 11, imageSource := "images/tracks/Hungaroring.png"]
  circuits[circuitId == 13, imageSource := "images/tracks/Track_map_of_Spa-Francorchamps_in_Belgium.png"]
  circuits[circuitId == 14, imageSource := "images/tracks/Monza2000.png"]
  circuits[circuitId == 15, imageSource := "images/tracks/Singapore_street_circuit_v4.png"]
  circuits[circuitId == 17, imageSource := "images/tracks/ShanghaiCircuit1.png"]
  circuits[circuitId == 18, imageSource := "images/tracks/Interlagos1990.png"]
  circuits[circuitId == 20, imageSource := "images/tracks/Nurburgring2002.png"]
  circuits[circuitId == 21, imageSource := "images/tracks/Imola1995.png"]
  circuits[circuitId == 22, imageSource := "images/tracks/SuzukaCircuit2005.png"]
  circuits[circuitId == 24, imageSource := "images/tracks/Circuit_Yas-Island.png"]
}

loadF1Data()