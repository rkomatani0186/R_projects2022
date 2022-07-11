#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(DBI)
library(RSQLite)
library(tidyverse)
library(shiny)

db = dbConnect(SQLite(), "statcast_db.sqlite")
dbListTables(db)
sc = dbGetQuery(conn = db, statement = "SELECT * FROM statcast_hitting")
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Statcast Hitting"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("ev",
                        "Exit velocity:",
                        min = 1,
                        max = 120,
                        value = 60)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           tableOutput("table")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$table <- renderTable({
      sc %>%
        dplyr::filter(launch_speed >= input$ev)


    })
}

# Run the application
shinyApp(ui = ui, server = server)
