
#Using pitchesDataFrame as data: Non NA launch angle and launch speed, either hits or non hits

data_labeled <- pitchesDataFrame %>%
  mutate(Single = ifelse(events == "single", 1, 0),
         Double = ifelse(events == "double", 1, 0),
         Triple = ifelse(events == "triple", 1, 0),
         HR = ifelse(events == "home_run", 1, 0),
         NotH = ifelse(events %in% c("single", "double", "triple", "home_run"), 0, 1))

#Generalized Additive Model for probability of Single, Double, Triple, and HR (Binary Model)
bm_Single <- gam(Single ~ s(launch_speed, launch_angle), family = binomial, data = data_labeled)
bm_Double <- gam(Double ~ s(launch_speed, launch_angle), family = binomial, data = data_labeled)
bm_Triple <- gam(Triple ~ s(launch_speed, launch_angle), family = binomial, data = data_labeled)
bm_HR <- gam(HR ~ s(launch_speed, launch_angle), family = binomial, data = data_labeled)
bm_NotH <- gam(NotH ~ s(launch_speed, launch_angle), family = binomial, data = data_labeled)

#Add probabilities of each events to data
data_labeled$prob_Single <- predict(bm_Single, data_labeled, type = "response")
data_labeled$prob_Double <- predict(bm_Double, data_labeled, type = "response")
data_labeled$prob_Triple <- predict(bm_Triple, data_labeled, type = "response")
data_labeled$prob_HR <- predict(bm_HR, data_labeled, type = "response")
data_labeled$prob_NotH <- predict(bm_NotH, data_labeled, type = "response")

#ewoba = (woba factor for single * probability of single + woba factor for double * probability of double + etc) / sum of probabilities
data_labeled_2 <- data_labeled %>%
  mutate(sum_prob = prob_Single + prob_Double + prob_Triple + prob_HR + prob_NotH,
         ewoba = (0 * prob_NotH + 0.9 * prob_Single + 1.25 * prob_Double + 1.6 * prob_Triple + 2.0 * prob_HR) / sum_prob)



#Break up data into bins of launch angle, exit velocity.
#Find the average woba within each bins.

#Make grid of zone
#For each portion of grid, find la and ev
#Find the bin that contains the launch angle and exit velocity inputs and find corresponding ewoba
#Repeat process for all zones and create tile plot of the resulting ewobas.



woba_color_contour <- ggplot(data_labeled_2) +
  geom_point(aes(x = launch_angle, y = launch_speed, color = ewoba)) +
  scale_colour_gradientn(colours = (rainbow(20)))+
  xlab("Launch Angle (deg)") + ylab("Exit Velocity (mph)") + labs(title = "ewoba")


woba_color_contour
