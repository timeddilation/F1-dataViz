### Documented issue with lap time data, this greatl effects all stats with lap times
### It seems that some lap times in the data remove the pit stop time, and other do not
### Delow is a demonstration, where in the 2 races viewed below, there are inconsistencies
### In the first case, subtracting the stop time from the lap time results in a "normal" lap time
### In the second case, subtracting the stop time from the lap time results in "impossible" times
### Until this is fixed, all stats will be skewed.
### It currently appears older races do not account for stops, and new races do account for stops

pitStops <<- fread("data/pit_stops.csv")
names(pitStops) <- c("raceId","driverId","stop","lap","time","duration","milliseconds")

lapTimesWithoutPits <- copy(lapTimes)
lapTimesWithoutPits <- merge(lapTimes,
                             pitStops[, .(raceId, driverId, lap, stopDuration = duration)],
                             by = c("raceId", "driverId", "lap"),
                             all.x = TRUE)
lapTimesWithoutPits[, stopDuration := as.double(stopDuration)][is.na(stopDuration), stopDuration := 0]
lapTimesWithoutPits[, secondsLessStop := seconds - stopDuration]

View(lapTimesWithoutPits[raceId == 841])
View(lapTimesWithoutPits[raceId == 1021])
