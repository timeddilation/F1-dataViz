### this generates pit stop windows for particular pit stop strategies
### given a circuit, it will cycle through all races in order by year
### output of function is pit windows for a given strategy, ie. 3-stop

evalCircuit_Id <- 17 # set the circuit ID to evaluate here!!!
racesForCircuit <- circuitRacesWithLapTimes(evalCircuit_Id)
lapsInRace <- merge(lapTimes, racesForCircuit, by = "raceId")
lapsInRace <- max(lapsInRace[, lap])
rm(evalCircuit_Id)

# pull pit stop data for race
pitStrategies <- pitStops[, .(stopStrategy = .N), by = c("raceId", "driverId")]
pitStrategies <- merge(pitStrategies, racesForCircuit[, .(raceId, year)], by = "raceId")
racePits <- merge(pitStops, pitStrategies, by = c("raceId", "driverId"))
# only review pit strategies for drivers who finished
validResultStatuses <- c("Finished",
                         "+1 Lap", "+2 Laps","+3 Laps",
                         "+4 Laps","+5 Laps","+6 Laps",
                         "+7 Laps","+8 Laps","+9 Laps")
validResultStatuses <- statuses[status %in% validResultStatuses]
racePits <- merge(racePits, results[, .(raceId, driverId, statusId)], 
                  by = c("driverId", "raceId"))
racePits <- racePits[statusId %in% validResultStatuses[, statusId]][stopStrategy <= 5]
# get max laps in race to set xlim in plot
racePits[, stopStrategy := factor(stopStrategy, levels = c(1,2,3,4,5))]

gganimStopWindow <- function(stop_Strategy) {
  
  stopWindows <- ggplot(racePits[stopStrategy == stop_Strategy], aes(x = lap)) +
    geom_density(adjust = 0.2, color = "gray35") +
    labs(subtitle = paste(stop_Strategy, "-Stop Window(s)", sep = "")) +
    # xlab("Lap") +
    theme_wsj() +
    theme(axis.title = element_text(size = 12, family = "mono"),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.x = element_blank()) +
    scale_x_continuous(limits = c(0,lapsInRace),
                       breaks = seq(from = 0, to = lapsInRace, by = 5)) +
    theme(legend.position = "none") +
    # gganimate stuff
    transition_time(year) +
    ease_aes('sine-in-out')
  
  # set frames in gif
  gps <- length(unique(racePits[, raceId]))
  framesPerGp <- 16
  totalFrames <- (gps * framesPerGp) + 15
  
  anim <- animate(stopWindows, start_pause = 5, end_pause = 10, nframes = totalFrames,
                  detail = 4, width = 400, height = 108)
  # anim <- animate(stopWindows, width = 400, height = 108)
  return(anim)
}

oneStop <- gganimStopWindow(1)
twoStop <- gganimStopWindow(2)
threeStop <- gganimStopWindow(3)
fourStop <- gganimStopWindow(4)
fiveStop <- gganimStopWindow(5)

rm(racesForCircuit, pitStrategies, racePits, validResultStatuses, lapsInRace, gganimStopWindow)
