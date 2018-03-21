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

data_origin <- readRDS(file = "Case_Study_Datensatz.rds")

# Da noch kein Datensatz vorhanden ist, wird er hier erstellt, das sollte am Ende im .Rmd passieren

# Import Geodaten
geodaten <- read_csv2("Geodaten_Gemeinden.csv")
geodaten <- geodaten %>%
  select(Gemeinde = 4, Laenge = 5, Breite = 6)

# Import Fahrzeugzulassungen
zulassungen <- read_csv2("Zulassungen_alle_Fahrzeuge.csv", n_max = 10000)
zulassungen <- zulassungen %>%
  select(IDNummer, Gemeinde = Gemeinden, Zulassung)

# Join
data <- inner_join(zulassungen, geodaten, by = "Gemeinde")

ui <- dashboardPage(skin = "red",
                    dashboardHeader(
                      title = "Shiny-App"
                    ),
                    dashboardSidebar(
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
                      )
                    ),
                    dashboardBody(
                      tabItems(
                        tabItem(
                          tabName = "karten",
                          fluidRow(
                            tabBox(
                              id = "tabBox_karten",
                              width = 12,
                              tabPanel(
                                title = "Karte",
                                leafletOutput(
                                  outputId = "output_standardkarte"
                                )
                              ),
                              tabPanel(
                                title = "Heatmap",
                                leafletOutput(
                                  outputId = "output_heatmap"
                                )
                              )
                            )
                          )
                        ),
                        tabItem(
                          tabName = "einstellungen",
                          fluidRow(
                            box(
                              dateRangeInput('dateRange',
                                             label = paste('Zeitraum auswählen'),
                                             start = as.Date("2009-01-02"), end = as.Date("2016-12-30"),
                                             min = as.Date("2009-01-02"), max = as.Date("2016-12-30"),
                                             separator = " - ", format = "dd/mm/yy",
                                             startview = 'year', language = 'de', weekstart = 1
                              )
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
      addWebGLHeatmap(lng = ~Längengrad, lat = ~Breitengrad, opacity = 0.5)
  })
  
  output$output_standardkarte <- renderLeaflet({
    leaflet(data = data_final_count()) %>%
      addTiles() %>%
      setView(lng = 10.4775, lat = 51.16, zoom = 5) %>%
      addMarkers(lng = ~Laenge, lat = ~Breite, popup = ~Gemeinden)
  })
  
  data_final <- reactive({
    data_origin %>%
      filter(Zulassung >= input$dateRange[1] & Zulassung <= input$dateRange[2])

  })
  
  data_final_count <- reactive({
    data_origin  %>%
      filter(Zulassung >= input$dateRange[1] & Zulassung <= input$dateRange[2])%>%
      count(Gemeinden)%>%
      left_join(geodaten, by = c("Gemeinden" = "Gemeinde"))
    
  })
}

shinyApp(ui, server)