### animate that shit ###
# set the circuit to evaluate
# raceCounts <- races[raceId %in% unique(lapTimes[, raceId]), .(races = .N), by = circuitId][order(races)]
# mostRacesCircuitId <- as.numeric(raceCounts[nrow(raceCounts), circuitId])
mostRacesCircuitId <- 18 # set the circuit ID to evaluate here!!!
# only pull races that have lapTimes data
racesWithTimes <- unique(lapTimes[, raceId])
racesForCircuit <- races[circuitId == mostRacesCircuitId][raceId %in% racesWithTimes][order(year)]
# pull all lap times for races, and create a "seconds" column for the lap time
allCircuitLapTimes <- lapTimes[raceId %in% racesForCircuit[, raceId]]
allCircuitLapTimes[, seconds := milliseconds / 1000]
# limit lap times to those under 3 minutes, removes pit stop laps and exceedingly slow laps that might throw off intended resutls
anaimateLapTimesData <- allCircuitLapTimes[seconds <= 180]
anaimateLapTimesData <- merge(anaimateLapTimesData, races[, .(raceId, name, year)])
# retrieve race results for all races on circuit
racesResults <- results[raceId %in% unique(anaimateLapTimesData[, raceId])]
racesResults <- merge(racesResults, races[, .(raceId, year)])
racesResults[, fastestLapTimeSeconds := convertLapTimeStringToSeconds(fastestLapTime)]
racesResults[, fastestLapSpeed := as.numeric(fastestLapSpeed)]
# group race results for fastest lap time and fastest lap speed
racesResults <- racesResults[, .(fastestLapSpeed = max(fastestLapSpeed, na.rm = TRUE), fastestLapTime = min(fastestLapTimeSeconds, na.rm = TRUE)), by = year]
racesResults[, fastestLapSpeed := as.character(fastestLapSpeed)][, fastestLapTime := as.character(fastestLapTime)]
racesResults[fastestLapSpeed == "-Inf", fastestLapSpeed := "No Data"][fastestLapTime == "Inf", fastestLapTime := "No Data"]
racesResults[fastestLapTime == "", fastestLapTime := "No Data"]
racesResults[, raceToolTip := paste("<span style='font-size:16; color:black'>",
                                    "**Fastest Lap:** ", fastestLapTime, 
                                    "<br>**Fastest Lap Speed:** ", as.character(fastestLapSpeed),
                                    "</span>",
                                    sep = "")]
# boxplot stats for static reference lines
circuitBoxPlotStats <- boxplot.stats(anaimateLapTimesData[, seconds])
# vlines <- as.data.table(circuitBoxPlotStats$stats[1:5])
vlines <- data.table(# metric = c("First Quartile", "Second Quartile", "Average", "Third Quartile", "Fourth Quartile"),
                    metric = c("Q1", "Q2", "Median", "Q3", "Q4"),
                     value = circuitBoxPlotStats$stats[1:5],
                     metricColor = c("darkgreen", "blue", "brown2", "blue", "darkgreen"))
# circuitMedianFastestSpeed <- circuitBoxPlotStats[3]

grandPrixName <- anaimateLapTimesData[1, name]
circuitImg <- getTrackImage(mostRacesCircuitId, 0.2)
rm(circuitBoxPlotStats, mostRacesCircuitId, racesForCircuit,racesWithTimes, raceCounts)

ggani <- ggplot(anaimateLapTimesData, aes(x = seconds)) + 
  geom_density(data = allCircuitLapTimes[seconds <= 180], adjust = 4,
               aes(x = seconds, color = "green", fill = "green", alpha = 0.5)) +
  geom_density(aes(color = "purple", fill = "purple", alpha = 0.5),
               adjust = 4) +
  geom_vline(data = vlines, aes(xintercept = value, color = metricColor), linetype = "dashed") +
  geom_text(data = vlines, aes(x = value + 0.5, y = 0.3, label = metric, color = metricColor), 
            angle = 270) +
  xlim(45,180) +
  ylim(0,0.35) +
  labs(title = paste(grandPrixName, "{frame_time}"),
       subtitle = "Lap Times Density Over Years") +
  xlab("Lap Time (seconds)") +
  ylab("Density") +
  theme_wsj() +
  # legend formatting
  scale_fill_identity(name = "", guide = "legend",
                      labels = c(paste("All", grandPrixName), paste("This", grandPrixName))) +
  scale_alpha_identity(element_blank()) +
  scale_colour_identity(element_blank()) +
  theme(legend.position = c(0.75,0.95), legend.direction = "horizontal",
        axis.title=element_text(size=12)) +
  # watermark, track image, and fastest lap/speed
  # annotation_custom(circuitImg, xmin = 45, xmax = 160, ymin = 0, ymax = 0.3) +
  annotation_custom(circuitImg, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  annotate(geom = "text", x = 165, y = 0.015, size = 5, label = "github.com/timeddilation") +
  geom_rich_text(data = racesResults[, .(year, raceToolTip)], aes(x = 150, y = 0.3, label = raceToolTip),
                 fill = NA, label.color = NA, hjust = 0) +
  # gganimate stuff
  transition_time(year) +
  enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in-out')
ggani

### You may need to run this (it will fail) to set the proper dimension of the gif
### After running (and failing to run), run the code after this to actually create the gif with the correct dimensions
# animate(plot = ggani, nframes = 110, end_pause = 10, fps = 10, duration = 10, detail = 1,
#         options(gganimate.dev_args = list(width = 960, height = 540)))

# setup some params to generate the final gif
gps <- length(unique(anaimateLapTimesData[, raceId]))
framesPerGp <- 16
totalFrames <- (gps * framesPerGp) + 15
rm(gps, framesPerGp, allCircuitLapTimes, anaimateLapTimesData, vlines, grandPrixName, racesResults)

animate(ggani, start_pause = 5, end_pause = 10, nframes = totalFrames, detail = 4)
rm(totalFrames, ggani, circuitImg)