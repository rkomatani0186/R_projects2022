
# Heat plot of batters for MLB data
heat_plot <- function(player, d, HR=FALSE){
  # define the strike zone
  topKzone <- 3.5
  botKzone <- 1.6
  inKzone <- -0.95
  outKzone <- 0.95
  kZone <- data.frame(
    x=c(inKzone, inKzone, outKzone, outKzone, inKzone),
    y=c(botKzone, topKzone, topKzone, botKzone, botKzone)
  )
  # only consider events that are official at-bats
  d <- d %>%
    filter(!events %in% c("walk", "hit_by_pitch", ""))
  if(HR == FALSE) d <- mutate(d, 
                                 Hit=ifelse(events %in% c("single", "double", "triple", "home_run"),
                                            1, 0)) else
                                              d <- mutate(d, Hit=ifelse(events == "home_run",
                                                                              1, 0))
  
  # implement the GAM fit (logistic link)
  pdata <- filter(d, player_name==player)
  fit <- gam(Hit ~ s(plate_x, plate_z), family=binomial, data=pdata)
  
  # find predicted probabilities over a 50 x 50 grid
  x <- seq(-1.5, 1.5, length.out=50)
  y <- seq(0.5, 5, length.out=50)
  data.predict <- data.frame(plate_x = c(outer(x, y * 0 + 1)),
                             plate_z = c(outer(x * 0 + 1, y)))
  lp <- predict(fit, data.predict)
  data.predict$Probability <- exp(lp) / (1 + exp(lp))
  
  # construct the plot
  type <- ifelse(HR==TRUE, "HR", "HIT")
  ggplot(kZone, aes(x, y)) +
    geom_tile(data=data.predict, 
              aes(x=plate_x, y=plate_z, fill= Probability)) +
    scale_fill_distiller(palette = "Spectral") +
    geom_path(lwd=1.5, col="black") +
    coord_fixed() +
    ggtitle(paste(player, type))
}


heat_plot("Harper, Bryce", HData)



# Heat plot of batters for uiuc data
heat_plot2 <- function(player, d, HR=FALSE){
  # define the strike zone
  topKzone <- 3.5
  botKzone <- 1.6
  inKzone <- -0.95
  outKzone <- 0.95
  kZone <- data.frame(
    x=c(inKzone, inKzone, outKzone, outKzone, inKzone),
    y=c(botKzone, topKzone, topKzone, botKzone, botKzone)
  )
  d <- d %>%
    filter(play_result %in% c("Single", "Double", "Triple", "Home Run", "Error", 
                              "Contact Out", "Swinging Strikeout", "Called Strikeout"))
  if(HR == FALSE) d <- mutate(d, 
                              Hit=ifelse(play_result %in% c("Single", "Double", "Triple", "Home Run"),
                                         1, 0)) else
                                           d <- mutate(d, Hit=ifelse(events == "Homerun",
                                                                     1, 0))
  
  # implement the GAM fit (logistic link)
  pdata <- filter(d, batter_name==player)
  fit <- gam(Hit ~ s(plate_x, plate_z), family=binomial, data=pdata)
  
  # find predicted probabilities over a 50 x 50 grid
  x <- seq(-1.5, 1.5, length.out=50)
  y <- seq(0.5, 5, length.out=50)
  data.predict <- data.frame(plate_x = c(outer(x, y * 0 + 1)),
                             plate_z = c(outer(x * 0 + 1, y)))
  lp <- predict(fit, data.predict)
  data.predict$Probability <- exp(lp) / (1 + exp(lp))
  
  # construct the plot
  type <- ifelse(HR==TRUE, "HR", "HIT")
  ggplot(kZone, aes(x, y)) +
    geom_tile(data=data.predict, 
              aes(x=plate_x, y=plate_z, fill= Probability)) +
    scale_fill_distiller(palette = "Spectral") +
    geom_path(lwd=1.5, col="black") +
    coord_fixed() +
    ggtitle(paste(player, type))
}

heat_plot2("Justin Janas", sc)

