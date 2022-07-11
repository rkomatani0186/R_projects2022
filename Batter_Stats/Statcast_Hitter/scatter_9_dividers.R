
#import libraries
library(shiny)
library(DBI)
library(RSQLite)
library(tidyverse)
library(ggplot2)
library(shinydashboard)
library(DT)
library(ggiraph)
library(RMySQL)
library(dplyr)
library(mgcv)

#Use functions connect_db and field_lines from Illini Baseball functions

#Obtain data for UIUC hitters
con <- connect_db("uiuc")
sc <- dbGetQuery(con, paste("select * from fs_pitches where season = 2021"))
dbDisconnect(con)



# Define UI for application that displays an input, plot, and table
ui <- dashboardPage(skin = "green",
                    
                    # Application title
                    dashboardHeader(title = "Statcast Data in 2021"),
                    
                    # Sidebar with a slider input for exit velocity
                    dashboardSidebar(),
                    
                    dashboardBody(
                      box(title = "Inputs", width = 6,
                          selectizeInput("team", "Select Team", choices = unique(sc$batter_team)),
                          selectizeInput("name", "Select Batter", choices = unique(sc$batter_name)),
                          sliderInput("ev", "Exit Velocity", min = 0, max = 120, value = 60)),
                      box(title = "Spray Plot", width = 6,
                          girafeOutput("plot")),
                      box(title = "Heat Plot", width = 6,
                          girafeOutput("plot2")),
                      box(title = "9 Dividers", width = 6,
                          girafeOutput("plot3")),
                      box(title = "Table", width = 12,
                          div(style = 'overflow-y:scroll;height:500px:',
                              DT::dataTableOutput("table")))
                      
                    )
)

# Define server logic required to react to inputs and plot data
server <- function(input, output, session) {
  
  sc_reactive <- reactive({
    sc %>% dplyr::filter(exit_velocity > input$ev,
                         batter_name == input$name) %>%
      select(date, pitcher_name, pitcher_hand, pitcher_team, batter_hand, balls, strikes,
             pitch_type, pitch_call, hit_type, play_result, exit_velocity, launch_angle,
             carry_distance_x, carry_distance_y) %>%
      mutate(carry_distance_x = round(carry_distance_x, 2),
             carry_distance_y = round(carry_distance_y, 2))
  })
  
  teams <- reactive({
    team <- subset(sc, batter_team %in% input$team)
    team <- select(team, "batter_name")
  })
  
  observe({
    updateSelectizeInput(session, "name", choices = teams(),
                         selected = NULL)
    
  })
  
  
  output$table <- DT::renderDataTable({
    sc_reactive()
  })
  
  output$plot <- renderGirafe({
    spray_chart <- ggplot(sc_reactive(), aes(x = carry_distance_x, y = carry_distance_y, color = play_result)) +
      geom_point()
    g <- field_lines(spray_chart) + theme_void() + coord_fixed() +
      ggtitle(paste(input$name, "Spray Chart for Exit Velocity Greater Than", input$ev, "MPH"))
    girafe(ggobj = g)
  })
  
  output$plot2 <- renderGirafe({
    g <- heat_plot2(input$name, sc)
    girafe(ggobj = g)
  }) 
  
  output$plot3 <- renderGirafe({
    g <- plot_b_avg_data(input$name)
    girafe(ggobj = g)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
