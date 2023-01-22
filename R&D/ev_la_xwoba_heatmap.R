#install_github("BillPetti/baseballr")

#install_github("bayesball/CalledStrike")
# Run the above before running app

#Import libraries
library(RMySQL)
library(tidyverse)
library(mgcv)
library(CalledStrike)
library(ggplot2)

######################################################
######################################################
######################################################
######################################################
#### Setup
######################################################
######################################################
######################################################
######################################################

#Get data from trackman
connect_db = function(db) {
  con = dbConnect(MySQL(),
                  user="mbaadmin",
                  password="gUZSaQAAwgDRhqZPPCqa",
                  dbname=db,
                  host="prodbaseballmysqlohio.ciurqw3b29ox.us-east-2.rds.amazonaws.com")
  
  return(con)
}
query = "SELECT * FROM tm_pitches
         WHERE Season = 2023 OR Season = 2022
         AND TaggedPitchType != 'Undefined';"
con = connect_db("uiuc")
pitches = dbGetQuery(con, query)
dbDisconnect(con)

######################################################
######################################################
######################################################
######################################################
#### Functions
######################################################
######################################################
######################################################
######################################################

# Exit Velocity Heatmap: inputs are data frame, pitcher name, and which pitch types to filter as vector
EV_heatmap = function(df, pitcher, pitch_types) {
  
  #filtering data to pitcher and pitch types
  df = df %>% 
    select(Pitcher, TaggedPitchType, ExitSpeed, PlateLocSide, PlateLocHeight) %>%
    filter(Pitcher == pitcher,
           TaggedPitchType %in% pitch_types) %>%
    mutate(plate_x = PlateLocSide,
           plate_z = PlateLocHeight) %>% select(-c(PlateLocSide, PlateLocHeight)) %>%
    na.omit()
  
  #checking if data set too small, if zero then returns "No data", if != 0 binds sample to data
  #MCMC resampling
  if (nrow(df) < 100) {
    if (nrow(df) == 0) {
      return("No data")
    }
    set.seed(1)
    df_sample = sample_n(df, size = 100 - nrow(df), replace = TRUE)
    df_sample$plate_x = jitter(df_sample$plate_x)
    df_sample$plate_z = jitter(df_sample$plate_z)
    
    df = rbind(df_sample, df)
  }
  
  #fitting GAM model to data
  ls_gam_fit <- function(d){
    gam(ExitSpeed ~ s(plate_x, plate_z),
        data = d)
  }
  
  #predicting EV based on location
  grid_predict <- function(fit){
    grid <- expand.grid(plate_x = seq(-1.5, 1.5, length = 100),
                        plate_z = seq(1, 4, length = 100))
    grid$lp <- predict(fit, grid, type = "response")
    grid
  }
  
  #inner workings of plot
  ls_plot <- function(dFrame, title = "Launch Velocity") {
    fit <- ls_gam_fit(dFrame)
    grid <- grid_predict(fit)
    plot <- tile_plot(grid, title)
    plot
  }

  #plot
  ls_plot(df, title = paste(toString(pitcher),
                            "Exit Velo Heatmap",
                            sep = " "))
}

#Launch Angle Heatmap
LA_heatmap = function(df, pitcher, pitch_types) {
  #filtering data to pitcher and pitch types
  df = df %>% 
    select(Pitcher, TaggedPitchType, Angle, PlateLocSide, PlateLocHeight) %>%
    filter(Pitcher == pitcher,
           TaggedPitchType %in% pitch_types) %>%
    mutate(plate_x = PlateLocSide,
           plate_z = PlateLocHeight) %>% select(-c(PlateLocSide, PlateLocHeight)) %>%
    na.omit()
  
  #checking if data set too small, if zero then returns "No data", if != 0 binds sample to data
  #MCMC resampling
  if (nrow(df) < 100) {
    if (nrow(df) == 0) {
      return("No data")
    }
    set.seed(1)
    df_sample = sample_n(df, size = 100 - nrow(df), replace = TRUE)
    df_sample$plate_x = jitter(df_sample$plate_x)
    df_sample$plate_z = jitter(df_sample$plate_z)
    
    df = rbind(df_sample, df)
  }
  
  #fitting GAM model to data
  la_gam_fit <- function(d){
    gam(Angle ~ s(plate_x, plate_z),
        data = d)
  }
  
  #predicting LA based on location
  grid_predict <- function(fit){
    grid <- expand.grid(plate_x = seq(-1.5, 1.5, length = 100),
                        plate_z = seq(1, 4, length = 100))
    grid$lp <- predict(fit, grid, type = "response")
    grid
  }
  
  #inner workings of plot
  la_plot <- function(dFrame, title = "Launch Angle") {
    fit <- la_gam_fit(dFrame)
    grid <- grid_predict(fit)
    plot <- tile_plot(grid, title)
    plot
  }
  
  #plot
  la_plot(df, title = paste(toString(pitcher),
                            "Launch Angle Heatmap",
                            sep = " "))
}


#xwOBA heatmap
xwoba_heatmap <- function(df, pitcher, pitch_types) {
  
  #Generate indicator for Single, Double, Triple, and HR
  df <- df %>%
    mutate(Single = ifelse(PlayResult == "Single", 1, 0),
           Double = ifelse(PlayResult == "Double", 1, 0),
           Triple = ifelse(PlayResult == "Triple", 1, 0),
           HR = ifelse(PlayResult == "HomeRun", 1, 0),
           NotH = ifelse(PlayResult %in% c("Single", "Double", "Triple", "HomeRun"), 0, 1))
  
  #Generalized Additive Model for probability of Single, Double, Triple, and HR (Binary Model)
  bm_Single <- gam(Single ~ s(ExitSpeed, Angle), family = binomial, data = df)
  bm_Double <- gam(Double ~ s(ExitSpeed, Angle), family = binomial, data = df)
  bm_Triple <- gam(Triple ~ s(ExitSpeed, Angle), family = binomial, data = df)
  bm_HR <- gam(HR ~ s(ExitSpeed, Angle), family = binomial, data = df)
  bm_NotH <- gam(NotH ~ s(ExitSpeed, Angle), family = binomial, data = df)
  
  #Add probabilities of each PlayResult to data
  df$prob_Single <- predict(bm_Single, df, type = "response")
  df$prob_Double <- predict(bm_Double, df, type = "response")
  df$prob_Triple <- predict(bm_Triple, df, type = "response")
  df$prob_HR <- predict(bm_HR, df, type = "response")
  df$prob_NotH <- predict(bm_NotH, df, type = "response")
  
  #ewoba = (woba factor for single * probability of single + woba factor for double * probability of double + etc) / sum of probabilities
  df <- df %>%
    mutate(sum_prob = prob_Single + prob_Double + prob_Triple + prob_HR + prob_NotH,
           ewoba = (0 * prob_NotH + 0.9 * prob_Single + 1.25 * prob_Double + 1.6 * prob_Triple + 2.0 * prob_HR) / sum_prob)
  
  #filtering data to pitcher and pitch types
  df = df %>% 
    select(Pitcher, TaggedPitchType, Angle, PlateLocSide, PlateLocHeight, ewoba) %>%
    filter(Pitcher == pitcher,
           TaggedPitchType %in% pitch_types) %>%
    mutate(plate_x = PlateLocSide,
           plate_z = PlateLocHeight) %>% select(-c(PlateLocSide, PlateLocHeight)) %>%
    na.omit()
  
  #checking if data set too small, if zero then returns "No data", if != 0 binds sample to data
  #MCMC resampling
  if (nrow(df) < 100) {
    if (nrow(df) == 0) {
      return("No data")
    }
    set.seed(1)
    df_sample = sample_n(df, size = 100 - nrow(df), replace = TRUE)
    df_sample$plate_x = jitter(df_sample$plate_x)
    df_sample$plate_z = jitter(df_sample$plate_z)
    
    df = rbind(df_sample, df)
  }
  
  #fitting GAM model to data
  ewoba_gam_fit <- function(d){
    gam(ewoba ~ s(plate_x, plate_z),
        data=d)
  }
  
  #predicting woba based on location
  grid_predict <- function(fit){
    grid <- expand.grid(plate_x = seq(-1.5, 1.5, length = 100),
                        plate_z = seq(1, 4, length = 100))
    grid$lp <- predict(fit, grid, type = "response")
    grid
  }
  
  #inner workings of plot
  ewoba_plot <- function(df, title = "Expected wOBA"){
    fit <- ewoba_gam_fit(df)
    grid <- grid_predict(fit)
    plot <- tile_plot(grid, title)
    plot
  }
  
  #plot
  ewoba_plot(df, title = paste(toString(pitcher),
                            "xwoba Heatmap",
                            sep = " "))
}


######################################################
######################################################
######################################################
######################################################
#### Testing
######################################################
######################################################
######################################################
######################################################

EV_heatmap(pitches, "Wenninger, Jack", c("Changeup", "Fastball"))
EV_heatmap(pitches, "Wenninger, Jack","Fastball")
LA_heatmap(pitches, "Wenninger, Jack", "Fastball")
xwoba_heatmap(pitches, "Wenninger, Jack", c("Changeup", "Fastball"))



