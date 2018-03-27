if(!require(shiny)){
  install.packages("shiny")
  require(shiny)
}

if(!require(shinydashboard)){
  install.packages("shinydashboard")
  require(shinydashboard)
}

if(!require(shinycssloaders)){
  install.packages("shinycssloaders")
  require(shinycssloaders)
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
                        ),
                        checkboxGroupInput("fahrzeug",
                                           label = "Fahrzeugtypen:",
                                           choices = c("OEM 11" = "11",
                                                       "OEM 12" = "12",
                                                       "OEM 21" = "21",
                                                       "OEM 22" = "22"),
                                           selected = c("11" ,
                                                        "12",
                                                        "21",
                                                        "22")
                      ),
                      checkboxGroupInput("sitz",
                                         label = "Sitztypen:",
                                         choices = c("Typ K2LE1" = "K2LE1",
                                                     "Typ K2LE2" = "K2LE2"),
                                         selected = c("K2LE1" ,
                                                      "K2LE2")
                                         
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
                                withSpinner(
                                  type = 5,
                                  color = "red",
                                  ui_element = leafletOutput(
                                    outputId = "output_standardkarte",
                                    width = "100%",
                                    height = "600px"
                                  )
                                )
                              ),
                              tabPanel(
                                title = "Heatmap",
                                withSpinner(
                                  type = 5,
                                  color = "red",
                                  ui_element = leafletOutput(
                                    outputId = "output_heatmap",
                                    width = "100%",
                                    height = "600px"
                                  )
                                )
                              )
                            )
                          )
                        ),
                        tabItem(
                          tabName = "einstellungen",
                          box(
                            checkboxInput(
                              inputId = "cluster",
                              label = "Marker zusammenfassen (empfohlen):",
                              value = TRUE
                            )
                          )
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
    data <- data_final_count()
    if(input$cluster == TRUE){
      leaflet(data = data) %>%
        addTiles() %>%
        setView(lng = 10.4775, lat = 51.16, zoom = 5) %>%
        addAwesomeMarkers(lng = ~Längengrad, 
                          lat = ~Breitengrad, 
                          popup = paste("Gemeinde:", data$Gemeinden, "<br>", "Anzahl:", data$n, sep = ""),
                          icon = icons(),
                          clusterOptions = markerClusterOptions())
    } else {
      leaflet(data = data) %>%
        addTiles() %>%
        setView(lng = 10.4775, lat = 51.16, zoom = 5) %>%
        addAwesomeMarkers(lng = ~Längengrad, 
                          lat = ~Breitengrad, 
                          popup = paste("Gemeinde:", data$Gemeinden, "<br>", "Anzahl:", data$n, sep = ""),
                          icon = icons())
    }
  })
  
  
  icons <- reactive({
    awesomeIcons(
      icon = 'ios-close',
      iconColor = 'black',
      library = 'ion',
      markerColor = map(data_final_count()$n, getColor)
    )
  })
  
  getColor <- function(n){
    color <- reactive({
        if(n >= 100){
          color = "black"
        } else if(n >= 25){
          color = "red"
        } else if(n >= 5){
          color = "orange"
        } else if(n >= 2){
          color = "yellow"
        } else if(n >= 1){
          color = "green"
        }
        return(color)
    })
    color()
  }
  
  data_final <- reactive({
    data_origin %>%
      filter(Zulassung >= input$dateRange[1] & Zulassung <= input$dateRange[2]) %>%
      filter(Sitz_Art %in% input$sitz) %>%
      filter(Fahrzeug_Typ %in% input$fahrzeug)
  })
  
  data_final_count <- reactive({
    data_origin  %>%
      filter(Zulassung >= input$dateRange[1] & Zulassung <= input$dateRange[2]) %>%
      filter(Sitz_Art %in% input$sitz) %>%
      filter(Fahrzeug_Typ %in% input$fahrzeug) %>%
      count(Gemeinden) %>%
      left_join(geodaten, by = "Gemeinden")
  })
  
  geodaten <- {
    unique(data_origin[,c("Gemeinden","Postleitzahl","Längengrad","Breitengrad")])
    
  }
}

shinyApp(ui, server)