### animate that shit ###
evalCircuit_Id <- 69 # set the circuit ID to evaluate here!!!

# only pull races that have lapTimes data
racesForCircuit <- racesForCircuit <- circuitRacesWithLapTimes(evalCircuit_Id)
# pull all qualifying runs for races
allQualifyingTimes <- qualifiers[raceId %in% racesForCircuit[, raceId]]
# melt down qualifiers
q1times <- allQualifyingTimes[!is.na(q1sec), .(qualifyId, raceId, 
                                               qualifyingTime = q1sec,
                                               qualifyingRound = "q1")]
q2times <- allQualifyingTimes[!is.na(q2sec), .(qualifyId, raceId, 
                                               qualifyingTime = q2sec,
                                               qualifyingRound = "q2")]
q3times <- allQualifyingTimes[!is.na(q3sec), .(qualifyId, raceId, 
                                               qualifyingTime = q3sec,
                                               qualifyingRound = "q3")]
allQualifyingTimes <- rbind(q1times, q2times, q3times)
animateQualifyingTimes <- merge(allQualifyingTimes, races[, .(raceId, name, year)], by = "raceId")
rm(q1times, q2times, q3times)
# get fastest qualifyig run for a given year
qualifyingRunResults <- animateQualifyingTimes[, .(fastestRun = min(qualifyingTime),
                                                   medianTime = median(qualifyingTime)), by = year]
qualifyingRunResults[, fastestRunDisplay := convertSecondsToDisplayTime(fastestRun)]
# setup color gradients
colorsGrab <- colorRampPalette(c("darkgreen","firebrick4"), bias = 5)
# add color gradient for fastest lap time
fastestQualifyingRunColors <- qualifyingRunResults[, .(year, fastestRun)][order(fastestRun)]
fastestQualifyingRunColors[, colorGradient := colorsGrab(nrow(fastestQualifyingRunColors))]
qualifyingRunResults <- merge(qualifyingRunResults, 
                               fastestQualifyingRunColors[, .(year, colorGradient)],
                               by = "year")
# add race tooltip to display fastest lap
qualifyingRunResults[, toolTip := paste("<span style='font-size:16'>",
                                         "**Fastest Run: <span style = 'color:", colorGradient, "'>", 
                                         fastestRunDisplay, "**</span>",
                                         "</span>",
                                         sep = "")]

# generate the breaks for the median lap times metric
# min/max should be in increments of 5, breaks/labels should be in increments of 15
medianMin <- floor(min(qualifyingRunResults[, medianTime]))
medianMin <- medianMin - medianMin %% 5
medianMax <- ceiling(max(qualifyingRunResults[, medianTime]))
medianMax <- medianMax + (5 - medianMax %% 5)
medianBreaks <- secondsDisplay[secondsInt %% 15 == 0][secondsInt >= medianMin & secondsInt <= medianMax]

# boxplot stats for static reference lines
colorsGrab <- colorRampPalette(c("darkgreen","firebrick4"))
circuitBoxPlotStats <- boxplot.stats(animateQualifyingTimes[, qualifyingTime])
vlines <- data.table(metric = c("Q1", "Q2", "Median", "Q3", "Q4"),
                     value = circuitBoxPlotStats$stats[1:5],
                     metricColor = colorsGrab(5))

circuitImg <- getTrackImage(evalCircuit_Id, 0.5)
circuitName <- circuits[circuitId == evalCircuit_Id, name]

rm(circuitBoxPlotStats, evalCircuit_Id, racesForCircuit, colorsGrab)
### define plot size
xMin <- 45
xMax <- 180
yMin <- 0
yMax <- 0.35

denAnim <- ggplot(animateQualifyingTimes, aes(x = qualifyingTime)) + 
  geom_density(data = allQualifyingTimes, adjust = 1,
               aes(x = qualifyingTime, color = "gray50", fill = "gray70", alpha = 0.5)) +
  geom_density(aes(color = "red2", fill = "red2", alpha = 0.5),
               adjust = 4) +
  geom_vline(data = vlines, aes(xintercept = value, color = metricColor), linetype = "dashed") +
  ylim(yMin,yMax) +
  scale_x_time(limits = c(xMin, xMax),
               breaks = secondsDisplay[secondsInt %% 5 == 0 & secondsInt >= 45, secondsInt], 
               labels = secondsDisplay[secondsInt %% 5 == 0 & secondsInt >= 45, secondsDisp]) +
  labs(title = paste(circuitName, "{frame_time}"),
       subtitle = "Qualifying Times Density Over Years") +
  xlab("Qualifying Time") +
  ylab("Density") +
  theme_wsj() +
  # legend formatting
  scale_fill_identity(name = "", guide = "legend",
                      labels = c("All Grands Prix", "Year's Grand Prix")) +
  scale_alpha_identity(element_blank()) +
  scale_colour_identity(element_blank()) +
  theme(legend.position = c(0.88,1), legend.direction = "vertical",
        legend.background = element_blank(),
        legend.text = element_text(size = 14, family = "mono"),
        axis.title=element_text(size=12)) +
  # watermark, track image, and fastest lap/speed
  annotation_custom(circuitImg, xmin = 150, xmax = 190, ymin = 0.12, ymax = 0.3) +
  annotate(geom = "text", x = 170, y = 0.015, size = 5, label = "github.com/timeddilation") +
  geom_rich_text(data = qualifyingRunResults[, .(year, toolTip)], aes(x = 150, y = 0.31, label = toolTip),
                 fill = NA, label.color = NA, hjust = 0, family = "mono") +
  # gganime stuff
  transition_time(year) +
  enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in-out')

# animate(denAnim, width = 960, height = 440)

spreadAnim <- ggplot(animateQualifyingTimes) +
  geom_tufteboxplot(aes(y = qualifyingTime, size = 5), show.legend = FALSE, color = "gray35",
                    median.type = "line", hoffset = 0, voffset = 0, width = 3, whisker.type = "point") +
  geom_hline(data = vlines, aes(yintercept = value, color = metricColor), linetype = "dashed") +
  ylim(xMin,xMax) +
  theme_wsj() +
  labs(title = "Qualifying Times Spread") +
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

medianAnim <- ggplot(qualifyingRunResults, aes(x = year, y = medianTime)) +
  geom_line(color = "gray35") +
  geom_point(aes(group = seq_along(year))) +
  labs(title = "Median Qualifying Times") +
  ylab("Qualifying Time (seconds)") +
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
gps <- length(unique(animateQualifyingTimes[, raceId]))
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

rm(gps, framesPerGp, allQualifyingTimes, animateQualifyingTimes, vlines, circuitName, 
   qualifyingRunResults, circuitImg, totalFrames, xMin, xMax, yMin, yMax,
   medianMin, medianMax, medianBreaks)
rm(denAnim, spreadAnim, medianAnim)