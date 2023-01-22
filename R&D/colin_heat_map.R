library(remotes)

install_github("BillPetti/baseballr")

install_github("bayesball/CalledStrike")
# Run the above before running app

library(RMySQL)
#library(illinibaseball)
library(randomForest)
library(caret)
library(rsample)
library(ggplot2)
library(gridExtra)
library(kableExtra)
library(tidyverse)
library(viridis)
library(ggpointdensity)
library(mgcv)
library(tidyverse)
library(baseballr)
library(CalledStrike)
library(remotes)
library(egg)
library(shiny)
connect_db = function(db) {
  con = dbConnect(MySQL(),
                  user="mbaadmin",
                  password="gUZSaQAAwgDRhqZPPCqa",
                  dbname=db,
                  host="prodbaseballmysqlohio.ciurqw3b29ox.us-east-2.rds.amazonaws.com")
  
  return(con)
}

#Setting global variables and functions
query =   "select *
          from fs_pitches
          where pitch_type is not null and
          launch_angle is not null and
          exit_velocity is not null and
          carry_distance > 0 and
          carry_distance < 450;"

load_pitches = function() {
  con = connect_db("uiuc")
  pitches = dbGetQuery(con,
                       query)
  dbDisconnect(con)
  
  return(pitches)
}


pitches = load_pitches()

pitchesDataFrame = as.data.frame(pitches)

colnames(pitchesDataFrame)[49] = c("launch_speed")
colnames(pitchesDataFrame)[24] = c("events")


pitchesDataFrame$events[pitchesDataFrame$events == "Home Run"] <- "home_run"
pitchesDataFrame$events[pitchesDataFrame$events == "Single"] <- "single"
pitchesDataFrame$events[pitchesDataFrame$events == "Double"] <- "double"
pitchesDataFrame$events[pitchesDataFrame$events == "Triple"] <- "triple"

ls_gam_fit <- function(d){
  gam(launch_speed ~ s(plate_x, plate_z),
      data=d)
}

grid_predict <- function(fit){
  grid <- expand.grid(plate_x = seq(-1.5, 1.5, length=50),
                      plate_z = seq(1, 4, length=50))
  grid$lp <- predict(fit, grid, type = "response")
  grid
}

setup_inplay <- function(sc){
  filter(sc) %>%
    mutate(HR = ifelse(events == "home_run", 1, 0),
           H = ifelse(events %in% c("single",
                                    "double", "triple", "home_run"), 1, 0),
           Count = paste(balls, strikes, sep="-"),
           spray_angle = atan((plate_x - 125.42) /
                                (198.27 - plate_z)) * 180 / pi)
}

ls_plot <- function(dFrame,
                    title = "Launch Velocity",
                    NCOL = 2){
  
  if(is.data.frame(dFrame) == TRUE) {
    dFrame <- list(dFrame)
    names(dFrame) <- "Group"
  }
  
  
  N_df <- length(dFrame)
  
  
  if(is.list(dFrame) == TRUE){
    if(length(names(dFrame)) == 0){
      names(dFrame) <- paste("Group", 1:N_df)
    }
  }
  
  
  df_p <- NULL
  
  
  for(j in 1:N_df){
    dFrame[[j]] %>%
      setup_inplay() %>%
      ls_gam_fit() %>%
      grid_predict() %>%
      mutate(Group = names(dFrame)[j]) -> df_c
    df_p <- rbind(df_p, df_c)
  }
  
  
  if(N_df == 1){
    tile_plot(df_p, title)
  } else {
    tile_plot(df_p, title) +
      facet_wrap(~ Group, ncol = NCOL)
  }
  
}



illinoisBatterData = pitchesDataFrame %>%
  filter(batter_team == "Illinois")


illinoisBatterData = illinoisBatterData %>%
  mutate(offspeed = case_when(
    illinoisBatterData$pitch_type == "Four Seam Fastball" ~ "All Fastball",
    illinoisBatterData$pitch_type == "Two Seam Fastball" ~ "All Fastball",
    illinoisBatterData$pitch_type == "Cutter" ~ "All Fastball",
    illinoisBatterData$pitch_type == "Fastball" ~ "All Fastball",
    illinoisBatterData$pitch_type == "Changeup" ~ "All Off-speed",
    illinoisBatterData$pitch_type == "Slider" ~ "All Off-speed",
    illinoisBatterData$pitch_type == "Curveball" ~ "All Off-speed",
    illinoisBatterData$pitch_type == "Splitter" ~ "All Off-speed",
  ))

uniquePitchVec = c("Four Seam Fastball", "Two Seam Fastball", "Cutter", "Changeup", "Slider", "Curveball", "Splitter")

batterNameVec = unique(illinoisBatterData$batter_name)
batterNameVec = append(batterNameVec, " ", after = 0)

offspeedVec = unique(illinoisBatterData$offspeed)
offspeedVec = append(offspeedVec, "All", after = 0)
offspeedVec = append(offspeedVec, uniquePitchVec)


handinessVec = unique(illinoisBatterData$pitcher_hand)
handinessVec = append(handinessVec, "All", after = 0)



# Define UI
ui <- fluidPage(
  
  # Application title
  titlePanel("Exit Velocity Heatmaps"),
  
  # Sidebar
  sidebarLayout(
    sidebarPanel(
      
      fluidRow(
        column(9, offset = 1,
               selectInput(inputId = 'player',
                           label = 'Choose Batter',
                           choices = batterNameVec),
        ),
      ),
      
      fluidRow(
        column(9, offset = 1,
               selectInput(inputId = 'pitch',
                           label = 'Pitch Type',
                           choices = offspeedVec),
        ),
      ),
      
      fluidRow(
        column(9, offset = 1,
               selectInput(inputId = 'handiness',
                           label = 'RHP or LHP',
                           choices = handinessVec),
        ),
      )
    ),
    
    # Show a plot
    mainPanel(
      plotOutput("heatmap")
    )
  )
)








# Define server logic
server <- function(input, output) {
  
  output$heatmap = renderPlot({
    data = illinoisBatterData %>%
      filter(batter_name == toString(input$player))
    
    if(input$pitch == "All Fastball") {data = filter(data, offspeed == "All Fastball")}
    if(input$pitch == "All Off-speed") {data = filter(data, offspeed == "All Off-speed")}
    
    if(input$pitch == "Four Seam Fastball") {data = filter(data, pitch_type == "Four Seam Fastball")}
    if(input$pitch == "Two Seam Fastball") {data = filter(data, pitch_type == "Two Seam Fastball")}
    if(input$pitch == "Cutter") {data = filter(data, pitch_type == "Cutter")}
    if(input$pitch == "Changeup") {data = filter(data, pitch_type == "Changeup")}
    if(input$pitch == "Slider") {data = filter(data, pitch_type == "Slider")}
    if(input$pitch == "Curveball") {data = filter(data, pitch_type == "Curveball")}
    if(input$pitch == "Splitter") {data = filter(data, pitch_type == "Splitter")}
    
    if(input$handiness == "R") {data = filter(data, pitcher_hand == "R")}
    if(input$handiness == "L") {data = filter(data, pitcher_hand == "L")}
    
    
    
    ls_plot(data, title = paste(toString(input$player),
                                "Exit Velocity Heatmap",
                                sep = " "))
  })
}





# Run the application
shinyApp(ui = ui, server = server)
