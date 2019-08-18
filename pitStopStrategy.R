race_Id <- 1010
# pull pit stop data for race
pitStrategies <- pitStops[, .(stopStrategy = .N), by = c("raceId", "driverId")]
racePits <- pitStops[raceId == race_Id]
racePits <- merge(racePits, pitStrategies, by = c("raceId", "driverId"))
# only review pit strategies for drivers who finished
validResultStatuses <- c("Finished",
                         "+1 Lap", "+2 Laps","+3 Laps",
                         "+4 Laps","+5 Laps","+6 Laps",
                         "+7 Laps","+8 Laps","+9 Laps")
validResultStatuses <- statuses[status %in% validResultStatuses]
racePits <- merge(racePits, results[, .(raceId, driverId, statusId)], 
                  by = c("driverId", "raceId"))
racePits[statusId %in% validResultStatuses[, statusId]]
# get max laps in race to set xlim in plot
lapsInRace <- max(lapTimes[raceId == race_Id, lap])

ggplot(racePits[stopStrategy <= 5], aes(x = lap, group = stopStrategy)) +
  stat_density(aes(colour = as.factor(stopStrategy)),
               geom = "line", position = "identity", adjust = 0.2) +
  labs(title = "Pit Strategies: Stop Laps") +
  xlab("Lap") +
  theme_wsj() +
  theme(axis.title = element_text(size = 12, family = "mono"),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),) +
  scale_x_continuous(limits = c(0,lapsInRace),
                     breaks = seq(from = 0, to = lapsInRace, by = 5)) +
  # legend formatting
  scale_colour_colorblind(name = "",
                          labels = c("1 Stop", "2 Stop", "3 Stop", "4 Stop", "5 Stop")) +
  theme(legend.position = c(0.88,1),
        legend.direction = "vertical",
        legend.background = element_blank(),
        legend.text = element_text(size = 14, family = "mono"))



rm(pitStrategies, racePits, validResultStatuses, lapsInRace)