library(RMySQL)
library(tidyverse)
library(illinibaseball)
library(xgboost)
library(ranger)
library(caret)
#library(ElemStatLearn)
library(car)
library(doParallel)
library(foreach)
library(iterators)


query = "SELECT * FROM tm_pitches
         WHERE Season = 2023 OR Season = 2022
         AND TaggedPitchType != 'Undefined';"
con = connect_db("uiuc")
pitches = dbGetQuery(con, query)
dbDisconnect(con)
#query = "SELECT * FROM reliableFS"
query = "select * from fs_pitches where season in (2018, 2019, 2020, 2021)"
con = connect_db("uiuc")
pitches2 = dbGetQuery(con, query)
dbDisconnect(con)


#Use Trackman data only first

data <- pitches %>%
  mutate(HALF.INNING = paste(GameID, Inning, BatterTeam))


half_innings <- data %>%
  mutate(OutsOnPlay = ifelse(KorBB == "Strikeout", 1, OutsOnPlay)) %>%
  group_by(HALF.INNING) %>%
  summarize(Outs.Inning = sum(OutsOnPlay),
            Runs.Inning = sum(RunsScored))

#RUNS.ROI = runs scored in rest of inning: total runs in inning - current run scored
data2 <- inner_join(data, half_innings, by = "HALF.INNING") %>%
  mutate(RUNS.ROI = Runs.Inning - RunsScored)




#Function to add RUNS.ROI

runs_roi <- function(ID) {
  data_ID <- data %>% 
    filter(GameID == ID) %>%
    arrange(Time)
  
  data_ID2 <- data_ID %>%
    mutate(RUNS = 0) %>%
    select(Inning, RunsScored, RUNS, Time)
  
  ls = as.list(data_ID2)
  
  run_so_far = 0
  
  for (i in 1:length(ls$Inning)) {
    run_so_far = run_so_far + ls$RunsScored[i]
    ls$RUNS[i] = run_so_far
  }
  
  df <- as.data.frame(ls)
  
  data_ID3 <- inner_join(data_ID, df, by = c("Time", "Inning", "RunsScored"))


  half_innings <- data_ID3 %>%
    mutate(OutsOnPlay = ifelse(KorBB == "Strikeout", 1, OutsOnPlay)) %>%
    group_by(HALF.INNING) %>%
    summarize(Outs.Inning = sum(OutsOnPlay),
              Runs.Inning = sum(RunsScored),
              Runs.Start = first(RUNS),
              MAX.RUNS = Runs.Inning + Runs.Start)

  data_ID4 <- data_ID3 %>%
    inner_join(half_innings, by = "HALF.INNING") %>%
    mutate(RUNS.ROI = MAX.RUNS - RUNS) %>%
    arrange(Time)

  return(data_ID4)
}

data4 <- runs_roi("20220508-IllinoisField-1")

game_ids <- unique(data$GameID)

data_all <- lapply(game_ids, runs_roi)

data_combined <- as.data.frame(data_all[1])

for (i in 2:length(data_all)) {
  data_combined <- rbind(data_combined, as.data.frame(data_all[i]))
}
  


data %>% filter(GameID == "20220508-IllinoisField-1") %>%
  arrange(Inning) -> May8_field1_data


inrs <- May8_field1_data %>%
  mutate(RUNS_SOFAR = 0) %>%
  select(Inning, RunsScored, RUNS_SOFAR, Time) %>%
  arrange(Time)


list = as.list(inrs)

runsofar = 0

for (i in 1:length(list$Inning)) {
  runsofar = runsofar + list$RunsScored[i]
  list$RUNS_SOFAR[i] = runsofar
}


list

df <- as.data.frame(list)

datamay8 <- inner_join(May8_field1_data, df, by = c("Time", "Inning", "RunsScored"))


half_innings <- datamay8 %>%
  mutate(OutsOnPlay = ifelse(KorBB == "Strikeout", 1, OutsOnPlay)) %>%
  group_by(HALF.INNING) %>%
  summarize(Outs.Inning = sum(OutsOnPlay),
            Runs.Inning = sum(RunsScored),
            Runs.Start = first(RUNS_SOFAR),
            MAX.RUNS = Runs.Inning + Runs.Start)

datamay80 <- datamay8 %>%
  inner_join(half_innings, by = "HALF.INNING") %>%
  mutate(RUNS.ROI = MAX.RUNS - RUNS_SOFAR) %>% 
  arrange(Time)





















inPlay = c("Contact Out", "Double", "Single", "Error", "Triple", "Home Run")
pitches2 = pitches2 %>% #editing FlightScope data to match Trackman
  mutate(PitchCall = str_replace_all(string = PitchCall,
                                     pattern = " ",
                                     repl = "")) %>%
  mutate(TaggedPitchType = ifelse(TaggedPitchType == "Fastball" | TaggedPitchType == "Four Seam Fastball",
                                  "Fastball",
                                  ifelse(TaggedPitchType == "Two Seam Fastball",
                                         "Sinker",
                                         TaggedPitchType))) %>%
  mutate(PitcherThrows = ifelse(PitcherThrows == "R", "Right", "Left")) %>%
  mutate(PitchCall = case_when(PitchCall == "Ball" | PitchCall == "Walk" ~ "BallCalled",
                               PitchCall == "Hit by Pitch" ~ "HitByPitch",
                               PitchCall %in% inPlay ~ "InPlay",
                               PitchCall == "Swinging Strike" | PitchCall == "Swinging Strikeout" ~ "StrikeSwinging",
                               PitchCall == "Called Strike" | PitchCall == "Called Strikeout" ~ "StrikeCalled",
                               PitchCall == "Foul Ball" ~ "FoulBall"))



# swings2 = pitches2 %>% #filtering FlightScope data for swings only
#   filter(PitchCall == c("StrikeSwinging", "InPlay")) %>%
#   select(listOfRelevantVariables)
# 
# 
# swings = pitches %>%
#   filter(PitchCall == c("StrikeSwinging", "InPlay")) %>%
#   select(listOfRelevantVariables)
# #combining Trackman and Flightscope Data
# swings = rbind(swings, swings2) 



#Download packages needed
library(Lahman)
library(tidyverse)
library(retrosheet)
library(baseballr)


#Use flight metrics and run value from 2021 MLB data and find its accuracy

data <- read.csv("C:/Users/12244/CSV_2021/sc.csv")

#Obtain data for 2016 season
fields <- read_csv("C:/Users/12244/STAT430/stat430materials/lectures/week5_runexpectancy/fields.csv")
dat2016 <- read_csv("C:/Users/12244/STAT430/stat430materials/lectures/week5_runexpectancy/all2016.csv",
                    col_names = pull(fields, Header),
                    na = character())

#Include runs, half inning, and runs scored
dat2016 <- dat2016 %>% 
  mutate(RUNS = AWAY_SCORE_CT + HOME_SCORE_CT,
         HALF.INNING = paste(GAME_ID, INN_CT, BAT_HOME_ID),
         RUNS.SCORED = (BAT_DEST_ID > 3) + (RUN1_DEST_ID > 3) + (RUN2_DEST_ID > 3) + (RUN3_DEST_ID > 3))

#Group by half inning
half_innings <- dat2016 %>%
  group_by(HALF.INNING) %>%
  dplyr::summarise(Outs.Inning = sum(EVENT_OUTS_CT),
                   Runs.Inning = sum(RUNS.SCORED),
                   Runs.Start = first(RUNS),
                   MAX.RUNS = Runs.Inning + Runs.Start)

#Join two data
dat2016 <- dat2016 %>%
  inner_join(half_innings, by = "HALF.INNING") %>%
  mutate(RUNS.ROI = MAX.RUNS - RUNS)

#Mutate additional information regarding state and new state
dat2016 <- dat2016 %>% mutate(BASES = paste(ifelse(BASE1_RUN_ID != "",1,0), 
                                            ifelse(BASE2_RUN_ID != "",1,0), 
                                            ifelse(BASE3_RUN_ID != "",1,0), sep = ""), 
                              STATE = paste(BASES, OUTS_CT))

dat2016 <- dat2016 %>% 
  mutate(NRUNNER1 = as.numeric(RUN1_DEST_ID == 1 | BAT_DEST_ID == 1), 
         NRUNNER2 = as.numeric(RUN1_DEST_ID == 2 | RUN2_DEST_ID == 2 | BAT_DEST_ID == 2),
         NRUNNER3 = as.numeric(RUN1_DEST_ID == 3 | RUN2_DEST_ID == 3 | RUN3_DEST_ID == 3 | BAT_DEST_ID == 3),
         NOUTS = OUTS_CT + EVENT_OUTS_CT,
         NEW.BASES = paste(NRUNNER1, NRUNNER2, NRUNNER3, sep = ""),
         NEW.STATE = paste(NEW.BASES, NOUTS)) %>%
  filter((STATE != NEW.STATE) | (RUNS.SCORED > 0)) %>%
  filter(Outs.Inning == 3)

#Create data for runs
RUNS <- dat2016 %>%
  group_by(STATE) %>%
  dplyr::summarize(Mean = mean(RUNS.ROI)) %>%
  mutate(Outs = substr(STATE, 5, 5)) %>%
  arrange(Outs)


dat2016 <- dat2016 %>%
  left_join(select(RUNS, - Outs), by = "STATE") %>%
  rename(Runs.State = Mean) %>%
  left_join(select(RUNS, -Outs), by = c("NEW.STATE" = "STATE")) %>%
  rename(Runs.New.State = Mean) %>%
  replace_na(list(Runs.New.State = 0)) %>%
  mutate(run_value = Runs.New.State - Runs.State + RUNS.SCORED)

data_selected <- dat2016 %>%
  select(run_value, )


forestTrain = train(run_value ~ .- Pitcher - PitcherThrows,
                    data = trainFastball,
                    method = "ranger",
                    tuneGrid = expand.grid(mtry = c(1, 3, 5),
                                           min.node.size = c(1, 3, 5, 10),
                                           splitrule = "gini"),
                    trControl = trainControl(method = "cv",
                                             number = 10),
                    num.trees = 100,
                    respect.unordered.factors = "partition")
forestTrain$results #use mtry = 3, min.node.size = 3
forestTrain$results[which.max(forestTrain$results$Accuracy), ]

