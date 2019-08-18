### Documented issue with lap time data, this greatly effects all stats with lap times
### It seems that some lap times in the data remove the pit stop time, and others do not
### Below is a demonstration, where in the 2 races viewed, there are inconsistencies
### In the first case, subtracting the stop time from the lap time results in a "normal" lap time
### In the second case, subtracting the stop time from the lap time results in "impossibly fast" times
### Until this is fixed, all stats will be skewed.
### It currently appears older races do not account for stops, and new races do account for stops
### If I new when this change in the database begins, I could write a tool to fix the data, however...
### The fix assumes that simply subtracting the pit duration from the lap time is an "accurate" lap time
library(data.table)

lapTimes <<- fread("data/lap_times.csv")
names(lapTimes) <- c("raceId","driverId","lap","position","time","milliseconds")
lapTimes[, seconds := milliseconds / 1000]

pitStops <<- fread("data/pit_stops.csv")
names(pitStops) <- c("raceId","driverId","stop","lap","time","duration","milliseconds")

lapTimesWithoutPits <- copy(lapTimes)
lapTimesWithoutPits <- merge(lapTimes,
                             pitStops[, .(raceId, driverId, lap, stopDuration = duration)],
                             by = c("raceId", "driverId", "lap"),
                             all.x = TRUE)
lapTimesWithoutPits[, stopDuration := as.double(stopDuration)][is.na(stopDuration), stopDuration := 0]
lapTimesWithoutPits[, secondsLessStop := seconds - stopDuration]

View(lapTimesWithoutPits[raceId == 841]) # Australia 2011 - subtracting stop time looks correct
View(lapTimesWithoutPits[raceId == 1021]) # Hungary 2019 - subtracting stop time results in impossibly fast times
