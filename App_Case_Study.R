library(shiny)

readRDS(file = "Case_Study_Datensatz.rds")

ui <- fluidPage(
  
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)