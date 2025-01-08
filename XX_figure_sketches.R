# playing around a bit

data <- modStatsAll[modStatsAll$model!="modIntOnly",]

# creating factor for seasons based on yday
seasons <- data.frame(
  season = c("Winter", "Winter","Spring", "Summer", "Fall"),
  xmin = c(355,1, 80, 172, 264),   # Approx start of seasons
  xmax = c(365, 80, 172, 264, 355)   # Approx end of seasons
)


# looking into polar projection for plots

ggplot(data=data, aes(x=yday, y=dRMSE, color=landcover)) + facet_grid(landcover~model) +
  geom_rect(data = seasons,
            aes(xmin = xmin, xmax = xmax, ymin = min(data$dRMSE), ymax = max(data$dRMSE), fill = season),
            inherit.aes = FALSE, alpha = 0.2) +
  geom_line() +
  scale_y_reverse() +
  scale_fill_manual(values = c("Winter" = "blue", "Spring" = "green", 
                               "Summer" = "yellow", "Fall" = "orange")) +
  coord_polar()
  
  
ggplot(data=data, aes(x=yday, y=dRMSE, color=landcover)) + facet_wrap(~model) +
  geom_rect(data = seasons,
            aes(xmin = xmin, xmax = xmax, ymin = min(data$dRMSE), ymax = max(data$dRMSE), fill = season),
            inherit.aes = FALSE, alpha = 0.2) +
  geom_line() +
  scale_y_reverse() +
  scale_fill_manual(values = c("Winter" = "blue", "Spring" = "green", 
                               "Summer" = "yellow", "Fall" = "orange")) +
  coord_polar()

