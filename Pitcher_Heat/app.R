#Shiny App for pitcher chart and heat maps

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


#Import Pitch by Pitch Data from Statcast
data_2018 <- read.csv("C:/Users/12244/STAT430/statcast_data/2018.csv")
data_2019 <- read.csv("C:/Users/12244/STAT430/statcast_data/2019.csv")
data_2021 <- read.csv("C:/Users/12244/STAT430/statcast_data/2021.csv")
data_2022 <- read.csv("C:/Users/12244/STAT430/statcast_data/2022.csv")

#Import Pitcher's general data from baseball savant
pdata_2018 <- read.csv("C:/Users/12244/STAT430/statcast_data/data2018.csv")
pdata_2019 <- read.csv("C:/Users/12244/STAT430/statcast_data/data2019.csv")
pdata_2021 <- read.csv("C:/Users/12244/STAT430/statcast_data/data2021.csv")
pdata_2022 <- read.csv("C:/Users/12244/STAT430/statcast_data/data2022.csv")

#Merge and adjust data
p_data <- rbind(pdata_2018, pdata_2019, pdata_2021, pdata_2022)

p_data$first_name <- str_remove_all(p_data$first_name, " ")

names(p_data)[names(p_data) == 'year'] <- 'game_year'

p_data$Name = paste(p_data$first_name, p_data$last_name, sep = " ")


player_data_2022 <- read.csv("C:/Users/12244/STAT430/statcast_data/player2022.csv")
player_data_2021 <- read.csv("C:/Users/12244/STAT430/statcast_data/player2021.csv")
player_data_2022 <- player_data_2022 %>%
  select(-X)
player_data_2020s <- rbind(player_data_2021, player_data_2022)
player_data_2020s$first_name <- str_remove_all(player_data_2020s$first_name, " ")
player_data_2020s <- player_data_2020s %>%
  mutate(Name = paste0(first_name, " ", last_name),
         pitcher = player_id)
names(player_data_2020s)[names(player_data_2020s) == 'year'] <- 'game_year'


################FILTER TO PITCHERS WITH MORE THAN 50 INNINGS


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

#Add frac_junk to data (the fraction of team pitches that are sinkers (SI), splitters (FS), sliders (SL), or change ups (CH) for a given season)
data_junk <- sc %>%
  filter(!(is.na(pitch_type))) %>%
  mutate(junk = ifelse(pitch_type %in% c("SI", "FS", "SL", "CH"), 1, 0)) %>%
  group_by(pitcher, game_year) %>%
  dplyr::summarise(pitch_count = n(),
            junk_total = sum(junk),
            frac_junk = junk_total / pitch_count)

#Join all three data: statcast, junk, and baseball savant
sc1 <- inner_join(sc, data_junk, by = c("pitcher", "game_year"))

sc2 <- inner_join(sc1, player_data_2020s, by = c("pitcher", "game_year"))

unique(sc2$game_year)
#Filter data to only pitches with plate_x and plate_z
sc4 <- sc2 %>%
  filter(!is.na(plate_x),
         !is.na(plate_z)) %>%
  arrange(Name)

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
  # implement the GAM fit (logistic link)
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
    coord_fixed()
}

#Function for pitch location frequency
pitch_freq <- function(data) {
  #Divide zone into box of 0.05 (ft^2)
  data <- data %>%
    mutate(plate_x_tenth = 0.15 * round(plate_x / 0.15, 0),
           plate_z_tenth = 0.15 * round(plate_z / 0.15, 0))
  #Count the frequency of pitch in each box
  pitch_grouped <- data %>%
    dplyr::group_by(plate_x_tenth, plate_z_tenth) %>%
    dplyr::summarize(frequency = n())
  #Plot pitch location frequency with strike zone boundary line
  ggplot(pitch_grouped, aes(plate_x_tenth, plate_z_tenth)) +
    geom_tile(aes(fill = frequency)) +
    geom_path(data = strike_zone, aes(x, y)) +
    scale_fill_viridis_c(option = "B", direction = -1) +
    coord_fixed() +
    xlim(c(-1.5, 1.5)) +
    ylim(c(0.5, 5))
}


# Define UI for application that displays an input, plot, and table
ui <- dashboardPage(skin = "green",
                    # Application title
                    dashboardHeader(title = "Pitcher Data"),
                    
                    # Sidebar
                    dashboardSidebar(),
                    
                    dashboardBody(
                      box(title = "Inputs", width = 6,
                          selectizeInput("year", "Select Year", choices = unique(sc4$game_year)),
                          selectizeInput("name", "Select Pitcher", choices = unique(sc4$Name)),
                          selectizeInput("type", "Select Pitch Type", choices = unique(sc4$pitch_type))),
                      box(title = "Table", width = 6, height = 300,
                          tableOutput("table")),
                      box(title = "Plot1", width = 6,
                          girafeOutput("plot1")),
                      box(title = "Plot2", width = 6, 
                          girafeOutput("plot2"))
                    )
)


# Define server logic required to react to inputs and plot data
server <- function(input, output, session) {
  
  sc_reactive <- reactive({
    sc4 %>%
      filter(game_year == input$year,
             Name == input$name,
             pitch_type == input$type)
  })
  
  years <- reactive({
    year <- subset(sc4, game_year == input$year)
    year <- select(year, "Name")
  })

  observe({
    updateSelectizeInput(session, "name", choices = years())
  })
  
  names <- reactive({
    sc4 %>% dplyr::filter(Name == input$name) %>%
      select(pitch_type)
  })

  observe({
    updateSelectizeInput(session, "type", choices = names())
  })

  output$table <- renderTable({
    #Use data from baseball savant
    sc_reactive() %>% select(frac_junk, pitch_count, game_date, Name, type, balls, release_extension) %>% head(1)
  })

  output$plot1 <- renderGirafe({
    pitcher_map <- hit_likely(sc_reactive() %>% filter(type == "X" | events %in% c("strikeout", "strikeout_double_play")))
    g <- pitcher_map + theme_void() + coord_fixed() +
      ggtitle(paste(input$name, "Hit Percentage by Location for", input$type, "in ", input$year))
    girafe(ggobj = g)
  })
  
  output$plot2 <- renderGirafe({
    frequency_map <- pitch_freq(sc_reactive())
    g <- frequency_map + theme_void() + coord_fixed() +
      ggtitle(paste(input$name, "Pitch Location Frequency for ", input$type, "in ", input$year))
    girafe(ggobj = g)
  })
}


# Run the application
shinyApp(ui = ui, server = server)

