if(!require(shiny)){
  install.packages("shiny")
  require(shiny)
}

if(!require(shinydashboard)){
  install.packages("shinydashboard")
  require(shinydashboard)
}

if(!require(readr)){
  install.packages("readr")
  require(readr)
}

if(!require(tidyverse)){
  install.packages("tidyverse")
  require(tidyverse)
}

if(!require(leaflet.extras)){
  install.packages("leaflet.extras")
  require(leaflet.extras)
}

#Datensatz importieren
data_origin <- readRDS(file = "Case_Study_Datensatz.rds")

#ui
ui <- dashboardPage(skin = "red",
                    dashboardHeader(
                      title = "Shiny-App"
                    ),
                    dashboardSidebar(
                      width = "300px",
                      fluidRow(
                            box(
                              dateRangeInput('dateRange',
                                             label = paste('Zeitraum auswählen'),
                                             start = as.Date("2009-01-02"), end = as.Date("2016-12-30"),
                                             min = as.Date("2009-01-02"), max = as.Date("2016-12-30"),
                                             separator = " - ", format = "dd/mm/yy",
                                             startview = 'year', language = 'de', weekstart = 1
                              )
                            ),
                            box(
                              checkboxGroupInput('fahrzeug',
                                                 label = "Fahrzeugtypen:",
                                                 choices = c("OEM 11" = "11",
                                                             "OEM 12" = "12",
                                                             "OEM 21" = "21",
                                                             "OEM 22" = "22"),
                                                 selected = c("11" ,
                                                              "12",
                                                              "21",
                                                              "22")
                                
                              )
                              
                            ),
                            box(
                              checkboxGroupInput('sitz',
                                                 label = "Sitztypen:",
                                                 choices = c("Typ K2LE1" = "K2LE1",
                                                             "Typ K2LE2" = "K2LE2"),
                                                 selected = c("K2LE1" ,
                                                              "K2LE2")
                                                 
                              )
                              
                            )
                          ),
                      sidebarMenu(
                        id = "sidebar_menu",
                        menuItem(
                          text = "Karten",
                          tabName = "karten"
                        ),
                        menuItem(
                          text = "Einstellungen",
                          tabName = "einstellungen"
                        )
                      ),
                      fluidRow(
                          dateRangeInput(inputId = "dateRange",
                                         label = paste("Zeitraum auswählen"),
                                         start = as.Date("2009-01-02"), end = as.Date("2016-12-30"),
                                         min = as.Date("2009-01-02"), max = as.Date("2016-12-30"),
                                         separator = " - ", format = "dd/mm/yy",
                                         startview = "year", language = "de", weekstart = 1,
                                         width = "100%"
                        )
                      )
                    ),
                    dashboardBody(
                      tabItems(
                        tabItem(
                          tabName = "karten",
                          fluidRow(
                            tabBox(
                              id = "tabBox_karten",
                              width = "100%",
                              height = "600px",
                              tabPanel(
                                title = "Karte",
                                leafletOutput(
                                  outputId = "output_standardkarte",
                                  width = "100%",
                                  height = "600px"
                                )
                              ),
                              tabPanel(
                                title = "Heatmap",
                                leafletOutput(
                                  outputId = "output_heatmap",
                                  width = "100%",
                                  height = "600px"
                                )
                              )
                            )
                          )
                        ),
                        tabItem(
                          tabName = "einstellungen"
                        )
                      )
                    )
)

server <- function(input, output, session) {
  
  
  output$output_heatmap <- renderLeaflet({
    leaflet(data = data_final()) %>%
      addTiles() %>%
      setView(lng = 10.4775, lat = 51.16, zoom = 5) %>%
      addHeatmap(lng = ~Längengrad, 
                 lat = ~Breitengrad, 
                 minOpacity = 0.5,
                 radius = 15,
                 blur = 20)
  })
  
  output$output_standardkarte <- renderLeaflet({
    leaflet(data = data_final_count()) %>%
      addTiles() %>%
      setView(lng = 10.4775, lat = 51.16, zoom = 5) %>%
      addMarkers(lng = ~Laenge, 
                 lat = ~Breite, popup = ~Gemeinden,
                 clusterOptions = markerClusterOptions())
  })
  
  data_final <- reactive({
    data_origin %>%
      filter(Zulassung >= input$dateRange[1] & Zulassung <= input$dateRange[2])%>%
      filter(Sitz_Art == input$sitz)%>%
      filter(Fahrzeug_Typ == input$fahrzeug)

  })
  
  data_final_count <- reactive({
    data_origin  %>%
      filter(Zulassung >= input$dateRange[1] & Zulassung <= input$dateRange[2])%>%
      filter(Sitz_Art == input$sitz)%>%
      filter(Fahrzeug_Typ == input$fahrzeug)%>%
      count(Gemeinden)%>%
      left_join(geodaten, by ="Gemeinden" )
  })
  
  geodaten <- {
    unique(data_origin[,c("Gemeinden","Postleitzahl","Längengrad","Breitengrad")])
    
  }
}

shinyApp(ui, server)