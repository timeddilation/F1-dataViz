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