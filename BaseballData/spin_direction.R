#Spin rate and spin efficiency

library(tidyverse)
library(xml2)
library(rvest)
library(dplyr)
library(readr)
library(ggplot2)
library(ggpubr)

#Acceptable team names for the argument
#BAL, BOS< NYY, TB, TOR, ATL, MIA, NYM, PHI, WSH
#CLE, CWS, DET, KC, MIN, CHC, CIN, MIL, PIT, STL
#HOU, LAA, OAK, SEA, TEX, ARI, COL, LAD, SD, SF

#Acceptable throwing_hand arguments
#L, R

#Acceptable pitch_type arguments
#ALL, FF, SI, CH, CU, FC, SL


spin_direction_leaderboard <- function(num_pitches, pitch_type = "ALL", throwing_hand = "All", team) {
  if(missing(team)) {
    if(throwing_hand != "All") {
      url <- paste0("https://baseballsavant.mlb.com/leaderboard/spin-direction-pitches?year=2020&min=", num_pitches, "&sort=9&sortDir=asc$pitch_type=", pitch_type, "&throws=", throwing_hand, "&playerName=&team=&csv=true")
    } else {
      url <- paste0("https://baseballsavant.mlb.com/leaderboard/spin-direction-pitches?year=2020&min=", num_pitches, "&sort=9&sortDir=asc$pitch_type=", pitch_type, "&throws=&playerName=&team=&csv=true")
    }
  }
  else {
    if(throwing_hand != "All") {
      url <- paste0("https://baseballsavant.mlb.com/leaderboard/spin-direction-pitches?year=2020&min=", num_pitches, "&sort=9&sortDir=asc$pitch_type=", pitch_type, "&throws=", throwing_hand, "&playerName=&team=", team, "&csv=true")
    } else {
      url <- paste0("https://baseballsavant.mlb.com/leaderboard/spin-direction-pitches?year=2020&min=", num_pitches, "&sort=9&sortDir=asc$pitch_type=", pitch_type, "&throws=&playerName=&team=", team, "&csv=true")
    }
  }
  payload <- read_csv(url)
  return(payload)
}

spin_axis_data <- spin_direction_leaderboard(1)
spin_axis_data

spin_axis_data %>% filter(api_pitch_type == "FF")

#data_2021 <- read_csv("C:/Users/Riku/Downloads/data_2021.csv")

sc_2020 <- read_csv("C:/Users/Riku/Downloads/statcast_data_2020.csv")

sc_2020 <- sc_2020 %>% mutate(zone_loc = case_when(zone == 1 | zone == 2 | zone == 3 ~ "High",
                                                   zone == 4 | zone == 5 | zone == 6 ~ "Medium",
                                                   zone == 7 | zone == 8 | zone == 9 ~ "Low",
                                                   plate_z >= 3.5 ~ "High",
                                                   zone == 13 | zone == 14 ~ "Low",
                                                   plate_z <= 1.6 ~ "Low",
                                                   TRUE ~ NA_character_))

sc_2020 <- left_join(sc_2020, spin_axis_data, by=c("pitcher" = "player_id", "pitch_name" = "api_pitch_name"))

spin_efficiency <- sc_2020 %>%
  filter(pitch_type == "FF", zone_loc == "Low") %>%
  group_by(hawkeye_measured_clock_label, p_throws) %>%
  summarize(n_pitches = n(),
            spin_rate = mean(release_spin_rate, na.rm = T)) %>%
  ggplot() + geom_bar(aes(hawkeye_measured_clock_label, n_pitches, fill = spin_rate), position = "dodge", stat = "identity") +
  theme_classic() + facet_wrap(~p_throws) + ggtitle("Spin Direction Distribution on Low FF")



#plotting data for FF
#Clock in terms of pitcher's viewpoint
#Spin rate increases but spin efficiency decreases as the clock gets closer to 12:00
active_spinrate <- sc_2020 %>%
  filter(pitch_type == "FF", zone_loc == "Low") %>%
  group_by(hawkeye_measured_clock_label, p_throws) %>%
  summarize(n_pitches = n(),
            active_spin = mean(active_spin, na.rm = T)) %>%
  ggplot() + geom_bar(aes(hawkeye_measured_clock_label, n_pitches, fill = active_spin), position = "dodge", stat = "identity") +
  theme_classic() + facet_wrap(~p_throws) + ggtitle("Spin Direction Distribution on Low FF", subtitle = "Active Spin Percentage by Clock Time")

ggarrange(spin_efficiency, active_spinrate, ncol = 1, nrow = 2)

                                                    