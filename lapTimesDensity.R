### animate that shit ###
evalCircuit_Id <- 6 # set the circuit ID to evaluate here!!!

# only pull races that have lapTimes data
racesWithTimes <- unique(lapTimes[, raceId])
racesForCircuit <- races[circuitId == evalCircuit_Id][raceId %in% racesWithTimes][order(year)]
# pull all lap times for races, and create a "seconds" column for the lap time
allCircuitLapTimes <- lapTimes[raceId %in% racesForCircuit[, raceId]]
# remove laps for drivers that had pit stops
allCircuitLapTimes <- merge(allCircuitLapTimes, 
                            pitStops[, .(raceId, driverId, lap, stopDuration = duration)],
                            by = c("raceId", "driverId", "lap"), all.x = TRUE)
allCircuitLapTimes <- allCircuitLapTimes[is.na(stopDuration)]
allCircuitLapTimes$stopDuration <- NULL
# limit lap times to those under 3 minutes, removes pit stop laps and exceedingly slow laps that might throw off intended resutls
anaimateLapTimesData <- allCircuitLapTimes[seconds <= 180]
anaimateLapTimesData <- merge(anaimateLapTimesData, races[, .(raceId, name, year)], by = "raceId")
# retrieve race results for all races on circuit
racesResults <- unique(races[raceId %in% unique(anaimateLapTimesData[, raceId]), .(raceId, year)])
racesResults <- merge(racesResults, fastestLaps, by = "raceId")
racesResults <- merge(racesResults, anaimateLapTimesData[, .(medianLapTime = median(seconds)), by = raceId], by = "raceId")
# setup color gradients
colorsGrab <- colorRampPalette(c("darkgreen","firebrick4"))
# add color gradient for fastest lap time
fastestLapColors <- racesResults[, .(raceId, seconds)][order(seconds)]
fastestLapColors[, colorGradient := colorsGrab(nrow(racesResults))]
racesResults <- merge(racesResults, fastestLapColors[, .(raceId, colorGradient)], by = "raceId")
# add color gradient for median lap time
medianLapColors <- racesResults[, .(raceId, medianLapTime)][order(medianLapTime)]
medianLapColors[, medianColorGradient := colorsGrab(nrow(racesResults))]
racesResults <- merge(racesResults, medianLapColors[, .(raceId, medianColorGradient)], by = "raceId")
rm(colorsGrab, fastestLapColors, medianLapColors)
# add race tooltip to display fastest lap
racesResults[, raceToolTip := paste("<span style='font-size:16'>",
                                    "**Fastest Lap:** <span style = 'color:", colorGradient, "'>", 
                                    displayTime, "</span>",
                                    # "<br>**Fastest Lap Speed:** ", as.character(fastestLapSpeed),
                                    "</span>",
                                    sep = "")]
# generate the breaks for the median lap times metric
# min/max should be in increments of 5, breaks/labels should be in increments of 15
medianMin <- floor(min(racesResults[, medianLapTime]))
medianMin <- medianMin - medianMin %% 5
medianMax <- ceiling(max(racesResults[, medianLapTime]))
medianMax <- medianMax + (5 - medianMax %% 5)
medianBreaks <- secondsDisplay[secondsInt %% 15 == 0][secondsInt >= medianMin & secondsInt <= medianMax]
# boxplot stats for static reference lines
colorsGrab <- colorRampPalette(c("darkgreen","firebrick4"))
circuitBoxPlotStats <- boxplot.stats(anaimateLapTimesData[, seconds])
vlines <- data.table(metric = c("Q1", "Q2", "Median", "Q3", "Q4"),
                     value = circuitBoxPlotStats$stats[1:5],
                     metricColor = colorsGrab(5))

circuitName <- circuits[circuitId == evalCircuit_Id, name]
circuitImg <- getTrackImage(evalCircuit_Id, 0.2)
trackWorldMap <- generateCircuitWorldMap(evalCircuit_Id)
rm(circuitBoxPlotStats, evalCircuit_Id, racesForCircuit, racesWithTimes, colorsGrab)

### define plot size
xMin <- 45
xMax <- 180
yMin <- 0
yMax <- 0.35
# density large plot
denAnim <- ggplot(anaimateLapTimesData, aes(x = seconds)) + 
  annotation_custom(trackWorldMap, xmin = 35, xmax = 75, ymin = 0.22, ymax = 0.4) +
  geom_density(data = allCircuitLapTimes[seconds <= 180], adjust = 4,
               aes(x = seconds, color = "gray50", fill = "gray70", alpha = 0.5)) +
  geom_density(aes(color = "red2", fill = "red2", alpha = 0.5),
               adjust = 4) +
  geom_vline(data = vlines, aes(xintercept = value, color = metricColor), linetype = "dashed") +
  ylim(yMin,yMax) +
  scale_x_time(limits = c(xMin, xMax),
               breaks = secondsDisplay[secondsInt %% 5 == 0 & secondsInt >= 45, secondsInt], 
               labels = secondsDisplay[secondsInt %% 5 == 0 & secondsInt >= 45, secondsDisp]) +
  labs(title = paste(circuitName, "{frame_time}"),
       subtitle = "Lap Times Density Over Years") +
  xlab("Lap Time") +
  ylab("Density") +
  theme_wsj() +
  # legend formatting
  scale_fill_identity(name = "", guide = "legend",
                      labels = c("All Grands Prix", "Year's Grand Prix")) +
  scale_alpha_identity(element_blank()) +
  scale_colour_identity(element_blank()) +
  theme(legend.position = c(0.88,1), legend.direction = "vertical",
        legend.background = element_blank(),
        legend.text = element_text(size = 14, family = "mono", colour = ),
        axis.title=element_text(size=12)) +
  # watermark, track image, and fastest lap/speed
  annotation_custom(circuitImg, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  annotate(geom = "text", x = 165, y = 0.015, size = 5, label = "github.com/timeddilation") +
  geom_rich_text(data = racesResults[, .(year, raceToolTip)], aes(x = 150, y = 0.31, label = raceToolTip),
                 fill = NA, label.color = NA, hjust = 0, family = "mono") +
  # gganimmate stuff
  transition_time(year) +
  enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in-out')
# animate(denAnim, width = 960, height = 440)

# field spread (boxplot)
spreadAnim <- ggplot(anaimateLapTimesData[seconds <= 180]) +
  geom_tufteboxplot(aes(y = seconds, size = 5), show.legend = FALSE, color = "gray35",
                    median.type = "line", hoffset = 0, voffset = 0, width = 3, whisker.type = "point") +
  geom_hline(data = vlines, aes(yintercept = value, color = metricColor), linetype = "dashed") +
  ylim(xMin,xMax) +
  theme_wsj() +
  labs(title = "Lap Times Spread") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(size = 12),
        legend.position = "none") +
  scale_y_time(limits = c(xMin, xMax),
               breaks = secondsDisplay[secondsInt %% 5 == 0 & secondsInt >= 45, secondsInt], 
               labels = secondsDisplay[secondsInt %% 5 == 0 & secondsInt >= 45, secondsDisp]) +
  coord_flip() +
  transition_time(year) +
  enter_fade() +
  exit_shrink() +
  ease_aes('sine-in-out')
# animate(spreadAnim, height = 100, width = 480)

# median lap time (line)
medianAnim <- ggplot(racesResults, aes(x = year, y = medianLapTime)) +
  annotate("text", x = max(racesResults[, year]), y = median(allCircuitLapTimes[seconds <= 180, seconds]),
           label = "median", color = "red", hjust = 1, vjust = -0.2) +
  geom_hline(yintercept = median(allCircuitLapTimes[seconds <= 180, seconds]),
             linetype = "dashed", color = "red") +
  geom_line(color = "gray35") +
  geom_point(aes(group = seq_along(year))) +
  labs(title = "Median Lap Times") +
  ylab("Lap Time (seconds)") +
  theme_wsj() +
  theme(plot.title = element_text(size = 12),
        axis.title.x = element_blank()) +
  scale_y_time(limits = c(medianMin, medianMax), 
               breaks = medianBreaks[, secondsInt],
               labels = medianBreaks[, secondsDisp]) +
  transition_reveal(year) +
  enter_fade() +
  exit_shrink() +
  ease_aes('sine-in-out')
# animate(medianAnim, height = 100, width = 480)

# setup some params to generate the final gif
gps <- length(unique(anaimateLapTimesData[, raceId]))
framesPerGp <- 16
totalFrames <- (gps * framesPerGp) + 15

denAnimGif <- animate(denAnim, start_pause = 5, end_pause = 10, nframes = totalFrames, 
                      detail = 4, width = 960, height = 440)
spreadAnimGif <- animate(spreadAnim, start_pause = 5, end_pause = 10, nframes = totalFrames, 
                         detail = 4, width = 480, height = 100)
medianAnimGif <- animate(medianAnim, start_pause = 5, end_pause = 10, nframes = totalFrames, 
                         detail = 4, width = 480, height = 100)

anim_save("gifFiles/density.gif", denAnimGif)
anim_save("gifFiles/spread.gif", spreadAnimGif)
anim_save("gifFiles/median.gif", medianAnimGif)

rm(gps, framesPerGp, allCircuitLapTimes, anaimateLapTimesData, vlines, circuitName, 
   racesResults, circuitImg, trackWorldMap, totalFrames, xMin, xMax, yMin, yMax,
   medianMin, medianMax, medianBreaks)
rm(denAnim, spreadAnim, medianAnim)
