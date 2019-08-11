###   Stats By Race ID  ###
raceEvaluating <- 841
raceDriverLaps <- generateRaceDriverLaps(raceEvaluating)

minMax <- raceLapsBoxPlotsMinMax(raceEvaluating) / 1000

# make a scatter plot for a race's laptimes across all drivers
gg <- ggplot(raceDriverLaps,
             aes(x = lap, y = lubridate::milliseconds(milliseconds))) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = minMax) +
  scale_y_time() +
  labs(title = races[raceId == raceEvaluating, paste(as.character(year), name, sep = " ")],
       colour = "Drivers") +
  xlab("Race Lap") + ylab("Lap Time") +
  geom_point(aes(colour = factor(surname)))

pp <- ggplotly(gg)
rm(gg)
pp <- driverLapTimesToolTip(pp)
pp

# line graph of lap times
gg <- ggplot(raceDriverLaps,
             aes(x = lap, y = lubridate::milliseconds(milliseconds), colour = factor(surname))) +
  #geom_point() +
  #geom_line() +
  geom_smooth(se = FALSE) +
  scale_y_time() +
  coord_cartesian(ylim = minMax) +
  labs(title = races[raceId == raceEvaluating, paste(as.character(year), name, sep = " ")],
       colour = "Drivers") +
  xlab("Race Lap") + ylab("Lap Time")

pp <- ggplotly(gg)
rm(gg)
pp <- driverLapTimesToolTip(pp)
pp




# position distnace by lap, top 6
lapEval <- 50
singeraceDriverLaps <- raceDriverLaps[lap == lapEval & position <= 6][order(totalRaceTime)]
lastPlaceTime <- max(singeraceDriverLaps[, totalRaceTime])
singeraceDriverLaps[, secondsAhead := (lastPlaceTime - totalRaceTime) / 1000]
singeraceDriverLaps

gg <- ggplot(singeraceDriverLaps,
       aes(x = reorder(surname,secondsAhead), y = secondsAhead)) + 
  geom_bar(stat = "identity") +
  scale_y_time()
pp <- ggplotly(gg)
pp

# race results
raceResults <- results[raceId == raceEvaluating]
