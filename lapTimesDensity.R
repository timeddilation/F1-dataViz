# raceCounts <- races[raceId %in% unique(lapTimes[, raceId]), .(races = .N), by = circuitId][order(races)]
# mostRacesCircuitId <- as.numeric(raceCounts[nrow(raceCounts), circuitId])
# rm(raceCounts)
# mostRacesCircuitId <- 18
# 
# racesWithTimes <- unique(lapTimes[, raceId])
# racesForCircuit <- races[circuitId == mostRacesCircuitId][raceId %in% racesWithTimes][order(year)]
# racesForCircuit
# 
# allCircuitLapTimes <- lapTimes[raceId %in% racesForCircuit[, raceId]]
# allCircuitLapTimes[, seconds := milliseconds / 1000]
# circuitBoxPlotStats <- boxplot.stats(allCircuitLapTimes[seconds <= 180, seconds])
# 
# # median <- circuitBoxPlotStats$stats[3]
# vlines <- as.data.table(circuitBoxPlotStats$stats[1:5])
# ### in loop ###
# i <- 1
# 
# raceEvaluating <- as.numeric(racesForCircuit[i, raceId])
# raceDriverLaps <- generateRaceDriverLaps(raceEvaluating)
# 
# 
# img <- readPNG(source = "images/brazil.png")
# img2 <- rasterGrob(img, interpolate = TRUE)
# #raceDriverLaps <-
# # single track speed density over years
# gg <- ggplot(raceDriverLaps[seconds <= 180],
#        aes(x = seconds)) +
#   geom_density(data = allCircuitLapTimes[seconds <= 180], aes(x = seconds, color = "green", fill = "green", alpha = 0.5),
#                adjust = 4,
#                ) +
#   geom_density(aes(color = "purple", fill = "purple", alpha = 0.5),
#                adjust = 4) +
#   geom_vline(data = vlines, aes(xintercept = V1, color = "red"), linetype = "dashed") +
#   xlim(45,180) +
#   ylim(0,0.35) +
#   labs(title = paste(racesForCircuit[raceId == raceEvaluating, name],
#                      racesForCircuit[raceId == raceEvaluating, year])) +
#   xlab("Lap Time (seconds)") +
#   ylab("Density") +
#   theme_wsj() +
#   scale_fill_identity(name = "", guide = "legend",
#                       labels = c(paste("All",racesForCircuit[raceId == raceEvaluating, name]),
#                                  paste("This",racesForCircuit[raceId == raceEvaluating, name]))) +
#   scale_alpha_identity(element_blank()) +
#   scale_colour_identity(element_blank()) +
#   theme(legend.position = c(0.75,0.95), legend.direction = "horizontal",
#         axis.title=element_text(size=12)) +
#   annotation_custom(img2, xmin = 160, xmax = 180, ymin = 0.2, ymax = 0.3)
# 
# gg
# 
# i <- i + 1
  
### animate that shit ###
raceCounts <- races[raceId %in% unique(lapTimes[, raceId]), .(races = .N), by = circuitId][order(races)]
mostRacesCircuitId <- as.numeric(raceCounts[nrow(raceCounts), circuitId])
mostRacesCircuitId <- 18 #static, doing brazil GP for test

racesWithTimes <- unique(lapTimes[, raceId])
racesForCircuit <- races[circuitId == mostRacesCircuitId][raceId %in% racesWithTimes][order(year)]
allCircuitLapTimes <- lapTimes[raceId %in% racesForCircuit[, raceId]]
allCircuitLapTimes[, seconds := milliseconds / 1000]

anaimateLapTimesData <- allCircuitLapTimes[seconds <= 180]
anaimateLapTimesData <- merge(anaimateLapTimesData, races[, .(raceId, name, year)])

circuitBoxPlotStats <- boxplot.stats(anaimateLapTimesData[, seconds])
vlines <- as.data.table(circuitBoxPlotStats$stats[1:5])

grandPrixName <- anaimateLapTimesData[1, name]
img <- readPNG(source = "images/brazil.png") # need to setup data for these images
img2 <- rasterGrob(img, interpolate = TRUE)
rm(circuitBoxPlotStats, mostRacesCircuitId, racesForCircuit,racesWithTimes, raceCounts, img)

ggani <- ggplot(anaimateLapTimesData, aes(x = seconds)) + 
  geom_density(data = allCircuitLapTimes[seconds <= 180], aes(x = seconds, color = "green", fill = "green", alpha = 0.5),
               adjust = 4,
  ) +
  geom_density(aes(color = "purple", fill = "purple", alpha = 0.5),
               adjust = 4) +
  geom_vline(data = vlines, aes(xintercept = V1, color = "red"), linetype = "dashed") +
  xlim(45,180) +
  ylim(0,0.35) +
  labs(title = paste(grandPrixName, "{frame_time}")) +
  xlab("Lap Time (seconds)") +
  ylab("Density") +
  theme_wsj() +
  scale_fill_identity(name = "", guide = "legend",
                      labels = c(paste("All", grandPrixName), paste("This", grandPrixName))) +
  scale_alpha_identity(element_blank()) +
  scale_colour_identity(element_blank()) +
  theme(legend.position = c(0.75,0.95), legend.direction = "horizontal",
        axis.title=element_text(size=12)) +
  annotation_custom(img2, xmin = 160, xmax = 180, ymin = 0.2, ymax = 0.3) +
  transition_time(year) +
  enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in-out')

# animate(plot = ggani, nframes = 110, end_pause = 10, ref_frame = 1, fps = 10, duration = 10, detail = 1,
#         options(gganimate.dev_args = list(width = 960, height = 540)))
gps <- length(unique(anaimateLapTimesData[, raceId]))
framesPerGp <- 16
totalFrames <- (gps * framesPerGp) + 15
rm(gps, framesPerGp, allCircuitLapTimes, anaimateLapTimesData, vlines, grandPrixName)

animate(ggani, start_pause = 5, end_pause = 10, nframes = totalFrames, detail = 4)
rm(totalFrames, ggani)