library(RSQLite)
library(DBI)
db = dbConnect(SQLite(), dbname = "statcast_db.sqlite")
dbListTables(db)
HData <- dbGetQuery(conn = db, "select * from statcast_data")

library(mgcv)
library(tidyverse)
library(baseballr)
library(metR)
library(shiny)
library(CalledStrike)
library(broom)
library(dplyr)

f <- HData %>% filter(stand == "L")
CalledStrike::sa_plot(f)

d <- HData %>% filter(player_name == "Harper, Bryce")
sa_plot(HData %>% filter(player_name %in% c("Ozuna, Marcell", "Trout, Mike")), title = "d")

RSA = la_plot(HData %>% filter(player_name == "Ozuna, Marcell"), title = "Mike Trout Spray Angle")

sa_plot(d)


library(ggpubr)

ggarrange(LgRSA, RSA)

mike_trout_sa = spray_angle_plot(HData %>% filter(player_name == "Trout, Mike"), title = "Mike Trout Spray Angle")
mike_trout_sa

josé_ramírez_sa = spray_angle_plot(HData %>% filter(player_name == "Ramírez, José"), title = "José Ramírez")
josé_ramírez_sa

#sa_plot
sa_plot <- function(df, title = "Spray Angle", NCOL = 2){
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
      spray_angle_gam_fit() %>%
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
  


if(is.data.frame(f) == TRUE) {
  f <- list(f)
  names(f) <- "Group"
}

N_f <- length(f)
if(is.list(f) == TRUE){
  if(length(names(f)) == 0){
    names(f) <- paste("Group", 1:N_f)
  }
}

f_p <- NULL

#setup_inplay
setup_inplay <- function(sc){
  filter(sc, type == "X") %>%
    mutate(HR = ifelse(events == "home_run", 1, 0),
           H = ifelse(events %in% c("single",
                                    "double", "triple", "home_run"), 1, 0),
           Count = paste(balls, strikes, sep="-"),
           spray_angle = atan((hc_x - 125.42) /
                                (198.27 - hc_y)) * 180 / pi)
}

  
f_1 <- filter(f[[1]], type == "X") %>%
  mutate(HR = ifelse(events == "home_run", 1, 0),
         H = ifelse(events %in% c("single",
                                  "double", "triple", "home_run"), 1, 0),
         Count = paste(balls, strikes, sep="-"),
         spray_angle = atan((hc_x - 125.42) /
                              (198.27 - hc_y)) * 180 / pi)

#sa_gam_fit
sa_gam_fit <- function(d){
  gam(spray_angle ~ s(plate_x, plate_z),
      data=d)
}

f_2 <- gam(spray_angle ~ plate_x + plate_z, data=f_1)

#grid_predict
grid_predict <- function(fit){
  grid <- expand.grid(plate_x = seq(-1.5, 1.5, length=50),
                      plate_z = seq(1, 4, length=50))
  grid$lp <- predict(fit, grid, type = "response")
  grid
}

grid <- expand.grid(plate_x = seq(-1.5, 1.5, length=50),
                    plate_z = seq(1, 4, length=50))

grid$lp <- predict(f_2, grid, type = "response")


grid %>% mutate(Group = names(f)[1]) -> f_c
f_p <- rbind(f_p, f_c)
if(N_f == 1){
  tile_plot(f_p, title = "Spray Angle")
} else {
  tile_plot(f_p, title = "Stuff") +
    facet_wrap(~ Group, ncol = 2)
}

sa_plot(f, title = "f")


--------------------------------------------------------------------------------

mike_trout_sa = sa_plot(HData %>% filter(player_name == "Trout, Mike"), title = "Mike Trout Spray Angle")

cody_bellinger_sa = sa_plot(HData %>% filter(player_name == "Bellinger, Cody"), title = "Cody Bellinger Spray Angle")

bryce_harper_sa = sa_plot(HData %>% filter(player_name == "Harper, Bryce"), title = "Bryce Harper Spray Angle")

ggarrange(mike_trout_sa, cody_bellinger_sa)


#ls_plot
ls_plot <- function(df,
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
      setup_inplay() %>%
      ls_gam_fit() %>%
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

#ls_gam_fit
ls_gam_fit <- function(d){
  gam(launch_speed ~ s(plate_x, plate_z),
      data=d)
}

#ensure launch_speed_plot satisfies ls_plot

mike_trout_ls = ls_plot(HData %>% filter(player_name == "Trout, Mike"), title = "Mike Trout Exit Velocity")

cody_bellinger_ls = ls_plot(HData %>% filter(player_name == "Bellinger, Cody"), title = "Cody Bellinger Exit Velocity")

bryce_harper_ls = ls_plot(HData %>% filter(player_name == "Harper, Bryce"), title = "Bryce Harper Exit Velocity")

ggarrange(mike_trout_ls, cody_bellinger_ls)


------------------------------------------------------------------------------------

bh <- collect_player("Bryce Harper",
                     Season = 2021)
bh2 <- bh %>% select(type, description, events)
CalledStrike::home_run_plot(bh)


-------------------------------------------------------------------------------------
la_plot <- function(df,
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
      setup_inplay() %>%
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

la_gam_fit <- function(d){
  gam(launch_angle ~ s(plate_x, plate_z),
      data=d)
}


mike_trout_la = la_plot(HData %>% filter(player_name == "Trout, Mike"), title = "Mike Trout Launch Angle")

cody_bellinger_la = la_plot(HData %>% filter(player_name == "Bellinger, Cody"), title = "Cody Bellinger Launch Angle")

bryce_harper_la = la_plot(HData %>% filter(player_name == "Harper, Bryce"), title = "Bryce Harper Launch Angle")

ggarrange(mike_trout_la, cody_bellinger_la)



-----------------------------------------------------------------------------------
  home_run_plot <- function(df,
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
        setup_inplay() %>%
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



mike_trout_hr = home_run_plot(HData %>% filter(player_name == "Trout, Mike"), title = "Mike Trout Homerun")

cody_bellinger_hr = home_run_plot(HData %>% filter(player_name == "Bellinger, Cody"), title = "Cody Bellinger Homerun")

bryce_harper_hr = home_run_plot(HData %>% filter(player_name == "Harper, Bryce"), title = "Bryce Harper Homerun")
ggarrange(mike_trout_hr, cody_bellinger_hr, bryce_harper_hr, ncol = 3)


ggarrange(mike_trout_sa, cody_bellinger_sa, bryce_harper_sa,
          mike_trout_ls, cody_bellinger_ls, bryce_harper_ls,
          mike_trout_la, cody_bellinger_la, bryce_harper_la,
          mike_trout_hr, cody_bellinger_hr, bryce_harper_hr, nrow = 4, ncol = 3)


------------------------------------------------------------------------------------
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
na <- filter(sc_sample,
             player_name == "Nolan Arenado")
ehit_contour(na, L = seq(0, .5, by = 0.01))
ewoba_contour(na, L = seq(0, .5, by = 0.01))
shohei_ohtani_avg = ehit_contour(HData %>% filter(player_name == "Ohtani, Shohei"), L = seq(0, .5, by = 0.01), title = "Shohei Ohtani Average")

aaron_judge_avg = ehit_contour(HData %>% filter(player_name == "Judge, Aaron"), L = seq(0, .5, by = 0.01), title = "Aaron Judge Average")

bryce_harper_avg = ehit_contour(HData %>% filter(player_name == "Harper, Bryce"), L = seq(0, .5, by = 0.01), title = "Bryce Harper Average")

shohei_ohtani_woba = ewoba_contour(HData %>% filter(player_name == "Ohtani, Shohei"), L = seq(0, .5, by = 0.01), title = "Shohei Ohtani Average")

aaron_judge_woba = ewoba_contour(HData %>% filter(player_name == "Judge, Aaron"), L = seq(0, .5, by = 0.01), title = "Aaron Judge Average")

bryce_harper_woba = ewoba_contour(HData %>% filter(player_name == "Harper, Bryce"), L = seq(0, .5, by = 0.01), title = "Bryce Harper Average")

ggarrange(shohei_ohtani_avg, aaron_judge_avg, bryce_harper_avg, ncol = 3)
      
#------------------------------------------------------------------------------------

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
      setup_inplay() -> df_f
      estimated_ba_using_ls_la(df_f) -> df_m
    df_f %>%
      mutate(new_eba = ifelse(df_m < 0, 0, df_m)) %>%
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

library(dplyr)

estimated_ba_using_ls_la <- function(d) {
  eba2 <- gam(H ~ s(launch_speed, launch_angle), family = binomial, data = d)
  predict(eba2, d, type = "response")
}

eba_gam_fit2 <- function(d){
  gam(new_eba ~ s(plate_x, plate_z),
      data=d)
}


Ohtani_data %>%
  setup_inplay() -> df_f
estimated_ba_using_ls_la(df_f) -> df_m

df_f %>% 
  mutate(new_eba = ifelse(df_m < 0, 0, df_m)) %>%
  eba_gam_fit2() -> p1

grid <- expand.grid(plate_x = seq(-1.5, 1.5, length=50),
                    plate_z = seq(1, 4, length=50))
grid$lp1 <- predict(p1, grid, type = "response")
grid -> data1



fit <- df_f %>%
  gam(H ~ s(launch_speed, launch_angle), family = binomial, data = .)

ehit_contour2(HData %>% filter(player_name == "Ohtani, Shohei"))

df_f %>%
  mutate(eba =
           as.numeric(estimated_ba_using_speedangle)) %>%
  eba_gam_fit()  %>%
  grid_predict() -> data2

data1
data2 
da <- inner_join(data1, data2)

plot1 <- ehit_contour2(df_f)
plot2 <- ehit_contour(df_f)
ggarrange(plot1, plot2)

ehit_contour(HData %>% filter(player_name == "Isbel, Kyle"), L = seq(0, 1, by = 0.005))
