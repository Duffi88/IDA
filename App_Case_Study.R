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
                                         
                      ),
                      checkboxGroupInput(
                        inputId = "fehler",
                        label = "Fehler liegt auf:",
                        choices = c("Einzelteilebene" = "einzelteil",
                                    "Komponentenebene" = "komponente"),
                        selected = c("Einzelteilebene" = "einzelteil")
                      ),
                      actionButton(
                        inputId = "update",
                        label = "Update Karten"
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
    input$update
    isolate({
      leaflet(data = data_final()) %>%
        addTiles() %>%
        setView(lng = 10.4775, lat = 51.16, zoom = 5) %>%
        addHeatmap(lng = ~Laengengrad, 
                   lat = ~Breitengrad, 
                   minOpacity = 0.5,
                   radius = 15,
                   blur = 20)
    })
  })
  
  output$output_standardkarte <- renderLeaflet({
    input$update
    isolate({
      data <- data_final_count()
      if(input$cluster == TRUE){
        leaflet(data = data) %>%
          addTiles() %>%
          setView(lng = 10.4775, lat = 51.16, zoom = 5) %>%
          addAwesomeMarkers(lng = ~Laengengrad, 
                            lat = ~Breitengrad, 
                            popup = paste("Gemeinde:", data$Gemeinden, "<br>", "Anzahl:", data$n, sep = ""),
                            icon = icons(),
                            clusterOptions = markerClusterOptions())
      } else {
        leaflet(data = data) %>%
          addTiles() %>%
          setView(lng = 10.4775, lat = 51.16, zoom = 5) %>%
          addAwesomeMarkers(lng = ~Laengengrad, 
                            lat = ~Breitengrad, 
                            popup = paste("Gemeinde:", data$Gemeinden, "<br>", "Anzahl:", data$n, sep = ""),
                            icon = icons())
      }
    })
  })
  
  
  icons <- reactive({
    input$update
    awesomeIcons(
      icon = 'ios-close',
      iconColor = 'black',
      library = 'ion'
    )
  })
  
  get_einzelteil_zahl <- function(){
    zahl <-reactive({
      if("einzelteil" %in% input$fehler){
        zahl <- 1
      } else {
        zahl <- 0
      }
    })
    zahl()
  }
  
  get_komponente_zahl <- function(){
    zahl <-reactive({
      if("komponente" %in% input$fehler){
        zahl <- 1
      } else {
        zahl <- 0
      }
    })
    zahl()
  }
  
  data_filter <- reactive({
    data_return <- data_origin %>%
      filter(Zulassung >= input$dateRange[1] & Zulassung <= input$dateRange[2]) %>%
      filter(Sitz_Art %in% input$sitz) %>%
      filter(Fahrzeug_Typ %in% input$fahrzeug)
    einzelteil_zahl <- get_einzelteil_zahl()
    komponente_zahl <- get_komponente_zahl()
    data_return <- filter(data_return, Einzelteil_Fehlerhaft == einzelteil_zahl, Komponente_Fehlerhaft == komponente_zahl)
    return(data_return)
  })
  
  data_final <- reactive({
    data_filter()
  })
  
  data_final_count <- reactive({
    data <- data_filter()
    data_return <- count(data, Gemeinden) %>%
      left_join(geodaten, by = "Gemeinden")
  })
  
  geodaten <- {
    unique(data_origin[,c("Gemeinden","Postleitzahl","Laengengrad","Breitengrad")])
  }
}

shinyApp(ui, server)