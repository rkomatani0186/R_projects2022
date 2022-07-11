
#import libraries
library(shiny)
library(DBI)
library(RSQLite)
library(tidyverse)
library(ggplot2)
library(shinydashboard)
library(DT)
library(ggiraph)


#Obtain data for batter
con <- connect_db("uiuc")
sc <- dbGetQuery(con, paste("select * from fs_pitches where season = 2021"))
dbDisconnect(con)



# Define UI for application that displays a table and a plot
ui <- fluidPage(

  # Application title
  titlePanel("Statcast Data in 2021"),

  # Sidebar with a slider input for exit velocity
  sidebarLayout(
    sidebarPanel(
      sliderInput("ev",
                  "Exit Velocity",
                  min = 0,
                  max = 120,
                  value = 60),
      selectInput("team", "Select Team", choices = sc$batter_team),
      selectInput("name", "Select Batter", choices = unique(sc$batter_name))
    ),

    # Show a table and plot of the generated distribution
    mainPanel(
      tableOutput("table"),
      plotOutput("plot")
    )
  )
)

# Define server logic required to react to inputs and plot data
server <- function(input, output, session) {

  sc_reactive <- reactive({
    sc %>% dplyr::filter(exit_velocity >= input$ev,
                         batter_name == input$name)
  })

  teams <- reactive({
    team <- subset(sc, batter_team %in% input$team)
    team <- select(team, "batter_name")
  })

  observe({
    updateSelectInput(session, "name", choices = teams(),
                      selected = NULL)
  })


  output$table <- renderTable({
    sc_reactive()
  })

  output$plot <- renderPlot({
    spray_chart <- ggplot(sc_reactive(), aes(x = carry_distance_x, y = carry_distance_y, color = play_result)) +
      geom_point() +
      theme_void() +
      coord_fixed() +
      ggtitle(paste(input$name, "Spray Chart on Balls Greater Than or Equal to ", input$ev, "MPH"))
    field_lines(spray_chart)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
