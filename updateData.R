### TODO: create function to fill missing fastest lap times in results

circuits <- fread("data/circuits.csv")
drivers <- fread("data/drivers.csv")
lapTimes <- fread("data/lapTimes.csv")
races <- fread("data/races.csv")
results <- fread("data/results.csv")

newCircuits <- fread("newerData/circuits.csv")
if (nrow(circuits) != nrow(newCircuits)){
  newDataOffset <- nrow(circuits) + 1
  updatedData <- rbind(circuits, newCircuits[newDataOffset:nrow(newCircuits)], use.names = FALSE)
  write.csv(updatedData, file = "circuits.csv", row.names = FALSE)
  rm (newDataOffset, updatedData)
}
rm(newCircuits)

newDrivers <- fread("newerData/driver.csv")
if (nrow(drivers) != nrow(newDrivers)){
  newDataOffset <- nrow(drivers) + 1
  updatedData <- rbind(drivers, newDrivers[newDataOffset:nrow(newDrivers)], use.names = FALSE)
  write.csv(updatedData, file = "drivers.csv", row.names = FALSE)
  rm (newDataOffset, updatedData)
}
rm(newDrivers)

newLapTimes <- fread("newerData/lap_times.csv")
if (nrow(lapTimes) != nrow(newLapTimes)){
  newDataOffset <- nrow(lapTimes) + 1
  updatedData <- rbind(lapTimes, newLapTimes[newDataOffset:nrow(newLapTimes)], use.names = FALSE)
  write.csv(updatedData, file = "lapTimes.csv", row.names = FALSE)
  rm (newDataOffset, updatedData)
}
rm(newLapTimes)

newRaces <- fread("newerData/races.csv")
if (nrow(races) != nrow(newRaces)){
  newDataOffset <- nrow(races) + 1
  updatedData <- rbind(races, newRaces[newDataOffset:nrow(newRaces)], use.names = FALSE)
  write.csv(updatedData, file = "races.csv", row.names = FALSE)
  rm (newDataOffset, updatedData)
}
rm(newRaces)

newResults <- fread("newerData/results.csv")
if (nrow(results) != nrow(newResults)){
  newDataOffset <- nrow(results) + 1
  updatedData <- rbind(results, newResults[newDataOffset:nrow(newResults)], use.names = FALSE)
  write.csv(updatedData, file = "results.csv", row.names = FALSE)
  rm (newDataOffset, updatedData)
}
rm(newResults)


### fix circuit names
circuits[circuitId == 18, name := "Autódromo José Carlos Pace"]
circuits[circuitId == 20, name := "Nürburgring"]

### attach circuit image source
if (!"imageSource" %in% names(circuits)){
  circuits[, imageSource := character()]
}
circuits[circuitId == 1, imageSource := "images/Albert_Park.png"]
circuits[circuitId == 2, imageSource := "images/Sepang.png"]
circuits[circuitId == 3, imageSource := "images/Sakhir1.png"]
circuits[circuitId == 4, imageSource := "images/CircuitDeCatalunya.png"]
circuits[circuitId == 6, imageSource := "images/Monte_Carlo_Formula_1_track_map.png"]
circuits[circuitId == 7, imageSource := "images/CircuitGillesVilleneuve.png"]
circuits[circuitId == 8, imageSource := "images/Magny-Cours.png"]
circuits[circuitId == 9, imageSource := "images/SilverstoneArena2010.png"]
circuits[circuitId == 10, imageSource := "images/Hockenheimring2002.png"]
circuits[circuitId == 11, imageSource := "images/Hungaroring.png"]
circuits[circuitId == 13, imageSource := "images/Track_map_of_Spa-Francorchamps_in_Belgium.png"]
circuits[circuitId == 14, imageSource := "Monza2000.png"]
circuits[circuitId == 15, imageSource := "images/Singapore_street_circuit_v4.png"]
circuits[circuitId == 17, imageSource := "images/ShanghaiCircuit1.png"]
circuits[circuitId == 18, imageSource := "images/Interlagos1990.png"]
circuits[circuitId == 20, imageSource := "images/Nurburgring2002.png"]
circuits[circuitId == 21, imageSource := "Imola1995.png"]
circuits[circuitId == 22, imageSource := "SuzukaCircuit2005.png"]
circuits[circuitId == 24, imageSource := "Circuit_Yas-Island.png"]