#Shiny app for tile plots for batters

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

#get data from uiuc database
con <- connect_db("uiuc")
sc <- dbGetQuery(con, paste("select * from fs_pitches where season = 2021"))
dbDisconnect(con)

#filter data into in-play and players with enough in-play data
inplay_sc <- sc %>% filter(play_result %in% c("Contact Out", "Single", "Double", "Triple", "Home Run", "Error"))
sl <- inplay_sc %>%  
  group_by(batter_name) %>% 
  summarize(count = n()) %>%
  filter(count > 15)
sl_batters <- sl$batter_name
sl_sc <- inplay_sc %>% filter(batter_name %in% sl_batters)
 
# Define UI for application that draws a table and plots
ui <- dashboardPage(skin = "green",
                    
  
  # Application title
  dashboardHeader(title = "Spray Angle"),
  
  # Sidebar with a slider input for exit velocity
  dashboardSidebar(),
  dashboardBody(
    box(title = "Inputs", width = 6,
        selectizeInput("name", "Select Batter", choices = unique(sl_sc$batter_name))
        ),
    # Show a table and plots of the generated distribution
    box(title = "Spray Angle", width = 6,
        plotOutput("plot")),
    box(title = "Exit Velo", width = 6,
        plotOutput("plot2")),
    box(title = "Launch Angle", width = 6,
        plotOutput("plot3")),
    box(title = "Table", width = 12,
        div(style = 'overflow-y:scroll;height:500px:',
        DT::dataTableOutput("table")))
  )
)

# Define server logic required to draw a table and plots
server <- function(input, output) {
  
  sl_sc_reactive <- reactive({
    sl_sc %>% filter(batter_name == input$name)
  })
  output$table <- DT::renderDataTable({
    sl_sc_reactive()
  })
  output$plot <- renderPlot({
    tryCatch({
      sa_plot2(sl_sc_reactive(), title = paste(input$name, "Spray Angle"))
      
    },
    error = function(cond) {
      message(paste("Not enough data to create spray chart"))
      message(paste("Here is the original message:"))
      message(cond)
      
      return(NULL)
    })
  })
  output$plot2 <- renderPlot({
    tryCatch({
      ls_plot2(sl_sc_reactive(), title = paste(input$name, "Exit Velocity"))
      
    },
    error = function(cond) {
      message(paste("Not enough data to create spray chart"))
      message(paste("Here is the original message:"))
      message(cond)
      
      return(NULL)
    })
  })
  output$plot3 <- renderPlot({
    tryCatch({
      la_plot2(sl_sc_reactive(), title = paste(input$name, "Launch Angle"))
      
    },
    error = function(cond) {
      message(paste("Not enough data to create spray chart"))
      message(paste("Here is the original message:"))
      message(cond)
      
      return(NULL)
    })
  })
}
# Run the application 
shinyApp(ui = ui, server = server)
