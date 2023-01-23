#Shiny App for Pitcher's Hit Percentage and Pitch Frequency by Location

#Import libraries
library(shiny)
library(DBI)
library(tidyverse)
library(ggplot2)
library(shinydashboard)
library(DT)
library(ggiraph)
library(dplyr)
library(baseballr)
library(mgcv)
library(broom)
library(plyr)



#Import Pitch by Pitch Data from Statcast:
#Since the data was too big to push into Github, please use the statcast_scraper.R file to scrape data for seasons: 2018, 2019, 2021, and 2022.
#statcast pitch by pitch data for 2018 season:
# data_2018 <- read.csv("C:/Users/12244/STAT430/statcast/2018.csv") 
# #statcast pitch by pitch data for 2019 season:
# data_2019 <- read.csv("C:/Users/12244/STAT430/statcast/2019.csv")
# #statcast pitch by pitch data for 2021 season:
# data_2021 <- read.csv("C:/Users/12244/STAT430/statcast/2021.csv")
# #statcast pitch by pitch data for 2022 season:
# data_2022 <- read.csv("C:/Users/12244/STAT430/statcast/2022.csv") 

#Import Pitcher's general data from baseball savant: 
#These data are included in the statcast_data folder in the working directory
pdata_2018 <- read.csv("statcast_data/data2018.csv")
pdata_2019 <- read.csv("statcast_data/data2019.csv")
pdata_2021 <- read.csv("statcast_data/data2021.csv")
pdata_2022 <- read.csv("statcast_data/data2022.csv")

#Merge and adjust data
p_data <- rbind(pdata_2018, pdata_2019, pdata_2021, pdata_2022)

p_data$first_name <- str_remove_all(p_data$first_name, " ")

names(p_data)[names(p_data) == 'year'] <- 'game_year'

p_data$Name = paste(p_data$first_name, p_data$last_name, sep = " ")

#load in pitcher WAR
bwar_pit = read_csv("https://www.baseball-reference.com/data/war_daily_pitch.txt", na = "NULL") %>% 
  filter(year_ID > 2017, year_ID != 2020) %>% 
  select(name_common, year_ID, stint_ID, WAR)

#collapse WAR by stint
collapse.stint <- function(d){
  data.frame(WAR=sum(d$WAR))
}
bwar_pit.new <- ddply(bwar_pit, .(name_common, year_ID), 
                      collapse.stint)
names(bwar_pit.new)[names(bwar_pit.new) == 'name_common'] <- 'Name'
names(bwar_pit.new)[names(bwar_pit.new) == 'year_ID'] <- 'game_year'

#merged WAR for all pitchers
p_data2 = merge(p_data, bwar_pit.new, by=c("Name", "game_year"))

#Filter to pitchers who have thrown more than 50 innings
p_data2 = p_data2 %>% 
  filter(p_formatted_ip >= 50)

names(p_data2)[names(p_data2) == 'player_id'] <- 'pitcher'

sc <- rbind(data_2018, data_2019, data_2021, data_2022)

#Add off speed to data (define off speed pitch any pitch other than fastballs) 
data_offspeed <- sc %>%
  filter(!(is.na(pitch_type))) %>%
  mutate(offspeed = ifelse(pitch_type %in% c("SI", "SL", "CH", "CU", "KC", "KN", "EP", "SC", "CS"), 1, 0)) %>%
  group_by(pitcher, game_year) %>%
  dplyr::summarise(total_pitches = n(),
                   offspeed_total = sum(offspeed),
                   frac_offspeed = offspeed_total / total_pitches)

#Join all three data: statcast, junk, and baseball savant
sc1 <- inner_join(sc, data_offspeed, by = c("pitcher", "game_year"))

sc2 <- inner_join(sc1, p_data2, by = c("pitcher", "game_year")) %>%
  filter(pitch_type != "")


#Filter data to in play data and strikeouts (Use for Hit Percentage heat plot)
#plate_x and plate_z in catcher's perspective. Positive plate_x is towards lefty's batter box.
sc4 <- sc2 %>%
  filter(!is.na(plate_x),
         !is.na(plate_z),
         type == "X" | events %in% c("strikeout", "strikeout_double_play")) %>%
  arrange(game_year, Name, pitch_type)

#Filter data to pitches with plate_x and plate_z (Use for Pitch Frequency heat plot)
sc5 <- sc2 %>%
  filter(!is.na(plate_x),
         !is.na(plate_z))

#Define dimension of home plate: home plate is 17 inches wide, and ball is 9 inches in circumference. Horizontal edges of the strike zone is + or - 0.947 feet (165).
top_strike_zone <- 3.6
bottom_strike_zone <- 1.5
inside_strike_zone <- -0.95
outside_strike_zone <- 0.95

#Build Strike Zone Border
strike_zone <- data.frame(
  x=c(inside_strike_zone, inside_strike_zone, outside_strike_zone, outside_strike_zone, inside_strike_zone),
  y=c(bottom_strike_zone, top_strike_zone, top_strike_zone, bottom_strike_zone, bottom_strike_zone)
)

#Function for hit percentage heat plot
hit_likely <- function(data) {
  data <- data %>%
    mutate(Hit = ifelse(events %in% c("single", "double", "triple", "home_run"), 1, 0))
  # implement the GAM fit binary model (logistic link)
  fit <- gam(Hit ~ s(plate_x, plate_z), family = binomial, data = data)
  
  # find predicted probabilities over a 50 x 50 grid
  x <- seq(-1.5, 1.5, length.out=50)
  y <- seq(0.5, 5, length.out=50)
  data.predict <- data.frame(plate_x = c(outer(x, y * 0 + 1)),
                             plate_z = c(outer(x * 0 + 1, y)))
  
  predicted_data <- fit %>%
    augment(type.predict = "response", newdata = data.predict)
  
  colnames(predicted_data)[colnames(predicted_data) == ".fitted"] <- "hit_percentage"
  
  # construct heat percentage tile plot with strike zone boundary line
  ggplot(predicted_data, aes(plate_x, plate_z)) + 
    geom_tile(aes(fill = hit_percentage)) + 
    scale_fill_distiller(palette = "Spectral") + 
    geom_path(data = strike_zone, aes(x, y)) +
    coord_fixed() + xlab("plate_x (ft)") + ylab("plate_z (ft)")
}

#Function for pitch location frequency
pitch_freq <- function(data) {
  #Divide zone into box of 0.15 (ft^2)
  data <- data %>%
    mutate(plate_x_fifteenth = 0.15 * round(plate_x / 0.15, 0),
           plate_z_fifteenth = 0.15 * round(plate_z / 0.15, 0))
  
  #Count the frequency of pitch in each box
  pitch_grouped <- data %>%
    dplyr::group_by(plate_x_fifteenth, plate_z_fifteenth) %>%
    dplyr::summarize(frequency = n())
  
  #Plot pitch location frequency with strike zone boundary line
  ggplot(pitch_grouped, aes(plate_x_fifteenth, plate_z_fifteenth)) +
    geom_tile(aes(fill = frequency)) +
    geom_path(data = strike_zone, aes(x, y)) +
    scale_fill_viridis_c(option = "B", direction = -1) +
    coord_fixed() +
    xlim(c(-1.5, 1.5)) +
    ylim(c(0.5, 5)) + xlab("plate_x (ft)") + ylab("plate_z (ft)")
}


# Define UI for application that displays inputs, plots, and table
ui <- dashboardPage(skin = "green",
                    # Application title
                    dashboardHeader(title = "Pitcher Data"),
                    
                    # Sidebar
                    dashboardSidebar(),
                    
                    dashboardBody(
                      box(title = "Inputs", width = 3,
                          selectizeInput("year", "Select Year", choices = unique(sc4$game_year)),
                          selectizeInput("name", "Select Pitcher", choices = unique(sc4$Name)),
                          selectizeInput("type", "Select Pitch Type", choices = unique(sc4$pitch_type))),
                      box(title = "Table", width = 9, height = 300,
                          tableOutput("table")),
                      box(title = "Plot1", width = 6,
                          girafeOutput("plot1")),
                      box(title = "Plot2", width = 6,
                          girafeOutput("plot2"))
                    )
)


# Define server logic required to react to inputs and plot data
server <- function(input, output, session) {
  
  #Filtering data depending on inputs for Plot 1 
  sc_reactive <- reactive({
    sc4 %>%
      filter(game_year == input$year,
             Name == input$name,
             pitch_type == input$type)
  })
  
  #Filtering data depending on inputs for Plot 2
  sc_reactive2 <- reactive({
    sc5 %>%
      filter(game_year == input$year,
             Name == input$name,
             pitch_type == input$type)
  })
  
  #Reactor for input: year
  years <- reactive({
    year <- subset(sc4, game_year == input$year)
    year <- select(year, "Name")
  })
  
  observe({
    updateSelectizeInput(session, "name", choices = years())
  })
  
  #Reactor for input: name
  names <- reactive({
    sc4 %>% dplyr::filter(Name == input$name) %>%
      select(pitch_type)
  })
  
  observe({
    updateSelectizeInput(session, "type", choices = names())
  })
  
  #Table
  output$table <- renderTable({
    #Use data from baseball savant
    sc_reactive() %>% select(WAR, p_win, p_loss, pitch_count, p_era, in_zone_percent, frac_offspeed, whiff_percent, p_k_percent, p_bb_percent, fastball_avg_speed, offspeed_avg_speed) %>% head(1)
  })
  
  #Plot 1: Hit Percentage by Location
  output$plot1 <- renderGirafe({
    pitcher_map <- hit_likely(sc_reactive())
    g <- pitcher_map + theme_void() + coord_fixed() +
      ggtitle(paste(input$name, "Hit Percentage by Location for", input$type, "in ", input$year))
    girafe(ggobj = g)
  })
  
  #Plot 2: Pitch Location Frequency
  output$plot2 <- renderGirafe({
    frequency_map <- pitch_freq(sc_reactive2())
    g <- frequency_map + theme_void() + coord_fixed() + 
      labs(title = paste(input$name, "Pitch Location Frequency for ", input$type, "in ", input$year))
    girafe(ggobj = g)
  })
}


# Run the application
shinyApp(ui = ui, server = server)

