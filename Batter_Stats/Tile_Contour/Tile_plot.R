#List of tile and contour plots for batters including spray angle, exit velo, launch angle, and batting average

#import libraries
library(CalledStrike)
library(ggplot2)
library(tidyverse)
library(DBI)
library(RMySQL)
library(ggpubr)
library(mgcv)
library(dplyr)



#obtain data from uiuc database
con <- connect_db("uiuc")
sc <- dbGetQuery(con, paste("select * from fs_pitches where season in (2020, 2021)"))
dbDisconnect(con)


#Spray Angle tile plot
setup_inplay2 <- function(sc){
  inplay_sc <- sc %>% filter(play_result %in% c("Contact Out", "Single", "Double", "Triple", "Home Run", "Error"))
  sl <- inplay_sc %>%  
    group_by(batter_name) %>%
    summarize(count = n()) %>%
    filter(count > 1)
  sl_batters <- sl$batter_name
  sl_sc <- inplay_sc %>% filter(batter_name %in% sl_batters)
  sl_sc %>%
    mutate(HR = ifelse(play_result == "Home Run", 1, 0),
           H = ifelse(play_result %in% c("Single",
                                    "Double", "Triple", "Home Run"), 1, 0),
           Count = paste(balls, strikes, sep="-"))
}

sa_gam_fit <- function(d){
  gam(spray_angle ~ s(plate_x, plate_z),
      data=d)
}

grid_predict <- function(fit){
  grid <- expand.grid(plate_x = seq(-1.5, 1.5, length=50),
                      plate_z = seq(1, 4, length=50))
  grid$lp <- predict(fit, grid, type = "response")
  grid
}


sa_plot2 <- function(df, title = "Spray Angle", NCOL = 2){
  if(is.data.frame(df) == TRUE) {
    df <- list(df)
    names(df) <- "Group"
  }
  N_df <- length(df)
  if(is.list(df) == TRUE){
    if(length(names(df)) == 0){
      names(df) <- paste("Group", 1:N_df)
    }
  }
  df_p <- NULL
  for(j in 1:N_df){
    df[[j]] %>%
      setup_inplay2() %>%
      sa_gam_fit() %>%
      grid_predict() %>%
      mutate(Group = names(df)[j]) -> df_c
    df_p <- rbind(df_p, df_c)
  }
  
  if(N_df == 1){
    tile_plot(df_p, title)
  } else {
    tile_plot(df_p, title) +
      facet_wrap(~ Group, ncol = NCOL)
  }
  
}


R_sa_plot <- sa_plot2(sc %>% filter(batter_hand == "R"), title = "Righty Spray Angle")
L_sa_plot <- sa_plot2(sc %>% filter(batter_hand == "L"), title = "Lefty Spray Angle")
ggarrange(R_sa_plot, L_sa_plot)

comia_sa_plot <- sa_plot2(sc %>% filter(batter_name == "Branden Comia"), title = "Comia Spray Angle")
ggarrange(R_sa_plot, comia_sa_plot)

#-------------------------------------------------------------------------------------------------------
#Exit Velo

ls_plot2 <- function(df,
                     title = "Launch Velocity",
                     NCOL = 2){
  if(is.data.frame(df) == TRUE) {
     df <- list(df)
     names(df) <- "Group"
     }
  N_df <- length(df)
  if(is.list(df) == TRUE){
    if(length(names(df)) == 0){
      names(df) <- paste("Group", 1:N_df)
    }
  }
  df_p <- NULL
  for(j in 1:N_df){
    df[[j]] %>%
      setup_inplay2() %>%
      ls_gam_fit2() %>%
       grid_predict() %>%
      mutate(Group = names(df)[j]) -> df_c
     df_p <- rbind(df_p, df_c)
  }
    
  if(N_df == 1){
    tile_plot(df_p, title)
  } else {
    tile_plot(df_p, title) +
      facet_wrap(~ Group, ncol = NCOL)
  }
    
}

ls_gam_fit2 <- function(d){
  gam(exit_velocity ~ s(plate_x, plate_z),
      data=d)
}


R_ls_plot <- ls_plot2(sc %>% filter(batter_hand == "R"), title = "Righty Exit Velo")
L_ls_plot <- ls_plot2(sc %>% filter(batter_hand == "L"), title = "Lefty Exit Velo")
ggarrange(R_ls_plot, L_ls_plot)

comia_ls_plot <- ls_plot2(sc %>% filter(batter_name == "Branden Comia"), title = "Comia Exit Velo")
ggarrange(R_ls_plot, comia_ls_plot)

#-------------------------------------------------------------------------------------------------
#Launch Angle 
la_plot2 <- function(df,
                    title = "Launch Angle",
                    NCOL = 2){
  
  if(is.data.frame(df) == TRUE) {
    df <- list(df)
    names(df) <- "Group"
  }
  N_df <- length(df)
  if(is.list(df) == TRUE){
    if(length(names(df)) == 0){
      names(df) <- paste("Group", 1:N_df)
    }
  }
  df_p <- NULL
  for(j in 1:N_df){
    df[[j]] %>%
      setup_inplay2() %>%
      la_gam_fit() %>%
      grid_predict() %>%
      mutate(Group = names(df)[j]) -> df_c
    df_p <- rbind(df_p, df_c)
  }
  
  if(N_df == 1){
    tile_plot(df_p, title)
  } else {
    tile_plot(df_p, title) +
      facet_wrap(~ Group, ncol = NCOL)
  }
}

R_la_plot <- la_plot2(sc %>% filter(batter_hand == "R"), title = "Righty Launch Angle")
L_la_plot <- la_plot2(sc %>% filter(batter_hand == "L"), title = "Lefty Launch Angle")
ggarrange(R_la_plot, L_la_plot)

comia_la_plot <- la_plot2(sc %>% filter(batter_name == "Branden Comia"), title = "Comia Launch Angle")
ggarrange(R_la_plot, comia_la_plot)

#---------------------------------------------------------------------------------
#Homerun
home_run_plot2 <- function(df,
                          title = "Probability of Home Run",
                          NCOL = 2){
  
  if(is.data.frame(df) == TRUE) {
    df <- list(df)
    names(df) <- "Group"
  }
  N_df <- length(df)
  if(is.list(df) == TRUE){
    if(length(names(df)) == 0){
      names(df) <- paste("Group", 1:N_df)
    }
  }
  df_p <- NULL
  for(j in 1:N_df){
    df[[j]] %>%
      setup_inplay2() %>%
      hr_h_gam_fit() %>%
      grid_predict() %>%
      mutate(Group = names(df)[j]) -> df_c
    df_p <- rbind(df_p, df_c)
  }
  
  if(N_df == 1){
    tile_plot(df_p, title)
  } else {
    tile_plot(df_p, title) +
      facet_wrap(~ Group, ncol = NCOL)
  }
  
}

hr_h_gam_fit <- function(d, HR = TRUE){
  if(HR == TRUE){
    gam(HR ~ s(plate_x, plate_z),
        family=binomial,
        data=d)} 
  else{
    gam(H ~ s(plate_x, plate_z),
        family=binomial,
        data=d)}
}

R_hr_plot <- home_run_plot2(sc %>% filter(batter_hand == "R"), title = "Righty Homerun")
L_hr_plot <- home_run_plot2(sc %>% filter(batter_hand == "L"), title = "Lefty Homerun")
ggarrange(R_hr_plot, L_hr_plot)

comia_hr_plot <- home_run_plot2(sc %>% filter(batter_name == "Branden Comia"), title = "Comia Home Run")
ggarrange(R_hr_plot, comia_hr_plot)

ggarrange(comia_sa_plot, comia_ls_plot, comia_la_plot, comia_hr_plot, ncol = 4)

#-------------------------------------------------------------------------------------
#Batting average MLB data
ehit_contour <- function(df, L = seq(0, 1, by = 0.02),
                         title = "Expected Batting Average",
                         NCOL = 2){
  
  if(is.data.frame(df) == TRUE) {
    df <- list(df)
    names(df) <- "Group"
  }
  N_df <- length(df)
  if(is.list(df) == TRUE){
    if(length(names(df)) == 0){
      names(df) <- paste("Group", 1:N_df)
    }
  }
  df_p <- NULL
  for(j in 1:N_df){
    df[[j]] %>%
      setup_inplay() %>%
      mutate(eba =
               as.numeric(estimated_ba_using_speedangle)) %>%
      eba_gam_fit()  %>%
      grid_predict() %>%
      mutate(Group = names(df)[j]) -> df_c
    df_p <- rbind(df_p, df_c)
  }
  
  if(N_df == 1){
    contour_graph(df_p, L, title)
  } else {
    contour_graph(df_p, L, title) +
      facet_wrap(~ Group, ncol = NCOL)
  }
}

eba_gam_fit <- function(d){
  gam(eba ~ s(plate_x, plate_z),
      data=d)
}

#---------------------------------------------------------------------------------------
#Batting average uiuc data
ehit_contour2 <- function(df, L = seq(0, 1, by = 0.02),
                          title = "Expected Batting Average",
                          NCOL = 2){
  
  if(is.data.frame(df) == TRUE) {
    df <- list(df)
    names(df) <- "Group"
  }
  N_df <- length(df)
  if(is.list(df) == TRUE){
    if(length(names(df)) == 0){
      names(df) <- paste("Group", 1:N_df)
    }
  }
  df_p <- NULL
  for(j in 1:N_df){
    df[[j]] %>%
      setup_inplay2() -> df_f
    estimated_ba_using_ls_la(df_f) -> df_m
    df_f %>%
      mutate(new_eba = df_m) %>%
      eba_gam_fit2()  %>%
      grid_predict() %>%
      mutate(Group = names(df)[j]) -> df_c
    df_p <- rbind(df_p, df_c)
  }
  
  if(N_df == 1){
    contour_graph(df_p, L, title)
  } else {
    contour_graph(df_p, L, title) +
      facet_wrap(~ Group, ncol = NCOL)
  }
}

estimated_ba_using_ls_la <- function(d) {
  eba2 <- gam(H ~ s(exit_velocity, launch_angle), data = d)
  predict(eba2, d, type = "response")
}

eba_gam_fit2 <- function(d){
  gam(new_eba ~ s(plate_x, plate_z),
      data=d)
}

ehit_contour2(sc %>% filter(batter_name == "Branden Comia"))



