# Laden der benötigten Packages
if(!require(shiny)){
  install.packages("shiny")
  require(shiny)
}

if(!require(readxl)){
  install.packages("readr")
  require(readxl)
}

if(!require(tidyverse)){
  install.packages("tidyverse")
  require(tidyverse)
}


# Import der Ausgangsdatensätze 
Komponente_K7 <- read_csv2("Komponente_K7.csv")
Logistikverzug_K7 <- read_csv2("Logistikverzug_K7.csv")

# Erzeugen des Datensatzes
Logistikverzug_K7 <- Logistikverzug_K7 %>%
  select(IDNummer, Wareneingang)

Logistikverzug <- inner_join(Komponente_K7, Logistikverzug_K7, by = "IDNummer")

# Formatieren der Spalte "Wareneingang" ins Datumsformat
Logistikverzug$Wareneingang <- as.Date(Logistikverzug$Wareneingang, format = "%d.%m.%Y")

# Hinzufügen der Spalte "Produktionsmonat (Produktionsdaten sind nur noch durch Monat und Jahr spezifiziert)
Logistikverzug$Produktionsmonat <- as.Date(cut(Logistikverzug$Produktionsdatum, breaks = "month"))

# Hinzufügen der Spalte "Monat" (Je nach Produktionsmonat wird ein Faktor 1-12 zugeteilt.)
Logistikverzug$Monat <- factor(lubridate::month(Logistikverzug$Produktionsmonat))

# Erzeugen eines Dataframes, der die zur Auswahl stehenden Farben enthält
name <- c("rot", "dunkelrot", "gelb", "orange", "pink", "rosa", "violett", 
          "dunkelblau", "hellblau", "dunkelgrün", "hellgrün", "türkis")
colors <- data.frame(name)
colors$color <- c("red", "red4", "yellow", "darkorange", "deeppink", "lightpink", "mediumorchid4",
                  "darkblue", "lightskyblue1", "darkgreen", "lightgreen", "turquoise1")

# Oberfläche der App
ui <- fluidPage(
  headerPanel(
    title = "Logistik Komponente K7"
  ),
  
  sidebarLayout(
    
    sidebarPanel(
      width = 3,
      titlePanel(
        title = "Anfangsdatum"
      ),
      fluidRow(
        
        column(
          width = 3,
          uiOutput("day.start")
        ),
        column(
          width = 3,
          selectInput(
            inputId = "month.start",
            label = "Monat",
            choices = c(1:12)
          )
        ),
        column(
          width = 5,
          selectInput(
            inputId = "year.start",
            label = "Jahr",
            choices = c(2008:2016)
          )
        )
      ),
      
      titlePanel(
        title = "Enddatum"
      ),
      fluidRow(
        
        column(
          width = 3,
          uiOutput("day.end")
        ),
        column(
          width = 3,
          selectInput(
            inputId = "month.end",
            label = "Monat",
            choices = c(1:12)
          )
        ),
        column(
          width = 5,
          selectInput(
            inputId = "year.end",
            label = "Jahr",
            choices = c(2008:2016)
          )
        )
      )
    ),
    
    mainPanel(
      
      # Der css-Befehl unterdrückt eine Fehlermeldung, die kurzfristig erzeugt wird,
      # solange der User die Daten im zweiten Tab noch nicht eingegeben hat.
      
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"
      ),
      width = 9,
      tabsetPanel(
        id = "tabs",
        tabPanel(
          title = "Produktionsmenge",
          value = 1,
          column(
            width = 3,
            fluidRow(
              column(
                width = 5,
                checkboxInput(
                  inputId = "jan",
                  label = "Januar",
                  value = FALSE
                )
              ),
              column(
                width = 7,
                selectInput(
                  inputId = "col.jan",
                  label = NULL, 
                  choices = c("Farbe...", name) 
                )
              )
            ),
            fluidRow(
              column(
                width = 5,
                checkboxInput(
                  inputId = "feb",
                  label = "Februar",
                  value = FALSE
                )
              ),
              column(
                width = 7,
                selectInput(
                  inputId = "col.feb",
                  label = NULL, 
                  choices = c("Farbe...", name) 
                )
              )
            ),
            fluidRow(
              column(
                width = 5,
                checkboxInput(
                  inputId = "mar",
                  label = "März",
                  value = FALSE
                )
              ),
              column(
                width = 7,
                selectInput(
                  inputId = "col.mar",
                  label = NULL, 
                  choices = c("Farbe...", name) 
                )
              )
            ),
            fluidRow(
              column(
                width = 5,
                checkboxInput(
                  inputId = "apr",
                  label = "April",
                  value = FALSE
                )
              ),
              column(
                width = 7,
                selectInput(
                  inputId = "col.apr",
                  label = NULL, 
                  choices = c("Farbe...", name) 
                )
              )
            ),
            fluidRow(
              column(
                width = 5,
                checkboxInput(
                  inputId = "may",
                  label = "Mai",
                  value = FALSE
                )
              ),
              column(
                width = 7,
                selectInput(
                  inputId = "col.may",
                  label = NULL, 
                  choices = c("Farbe...", name) 
                )
              )
            ),
            fluidRow(
              column(
                width = 5,
                checkboxInput(
                  inputId = "jun",
                  label = "Juni",
                  value = FALSE
                )
              ),
              column(
                width = 7,
                selectInput(
                  inputId = "col.jun",
                  label = NULL, 
                  choices = c("Farbe...", name) 
                )
              )
            ),
            fluidRow(
              column(
                width = 5,
                checkboxInput(
                  inputId = "jul",
                  label = "Juli",
                  value = FALSE
                )
              ),
              column(
                width = 7,
                selectInput(
                  inputId = "col.jul",
                  label = NULL, 
                  choices = c("Farbe...", name) 
                )
              )
            ),
            fluidRow(
              column(
                width = 5,
                checkboxInput(
                  inputId = "aug",
                  label = "August",
                  value = FALSE
                )
              ),
              column(
                width = 7,
                selectInput(
                  inputId = "col.aug",
                  label = NULL, 
                  choices = c("Farbe...", name) 
                )
              )
            ),
            fluidRow(
              column(
                width = 5,
                checkboxInput(
                  inputId = "sep",
                  label = "September",
                  value = FALSE
                )
              ),
              column(
                width = 7,
                selectInput(
                  inputId = "col.sep",
                  label = NULL, 
                  choices = c("Farbe...", name) 
                )
              )
            ),
            fluidRow(
              column(
                width = 5,
                checkboxInput(
                  inputId = "oct",
                  label = "Oktober",
                  value = FALSE
                )
              ),
              column(
                width = 7,
                selectInput(
                  inputId = "col.oct",
                  label = NULL, 
                  choices = c("Farbe...", name) 
                )
              )
            ),
            fluidRow(
              column(
                width = 5,
                checkboxInput(
                  inputId = "nov",
                  label = "November",
                  value = FALSE
                )
              ),
              column(
                width = 7,
                selectInput(
                  inputId = "col.nov",
                  label = NULL, 
                  choices = c("Farbe...", name) 
                )
              )
            ),
            fluidRow(
              column(
                width = 5,
                checkboxInput(
                  inputId = "dec",
                  label = "Dezember",
                  value = FALSE
                )
              ),
              column(
                width = 7,
                selectInput(
                  inputId = "col.dec",
                  label = NULL, 
                  choices = c("Farbe...", name) 
                )
              )
            ),
            fluidRow(
              column(
                width = 5,
                actionButton(
                  inputId = "alle",
                  label = "Alle"
                )
              ),
              column(
                width = 7,
                actionButton(
                  inputId = "reset",
                  label = "Zurücksetzen"
                )
              )
            )
          ),
          column(
            width = 9,
            plotOutput(
              outputId = "plot_prod"
            )
          )
        ),
        
        tabPanel(
          title = "Warenausgang",
          value = 2,
          plotOutput(
            outputId = "plot_log"
          )
        )
      )
    )
  )
)

# Funktion der App
server <- function(input, output, session) {
  
  output$day.start <- renderUI({
    
    if (input$tabs == 2) {
      selectInput(
        inputId = "day.start",
        label = "Tag",
        choices = c(1:31)
      )
    }
  })
  
  output$day.end <- renderUI({
    
    if (input$tabs == 2) {
      selectInput(
        inputId = "day.end",
        label = "Tag",
        choices = c(1:31)
      )
    }
  })
  
  
  setcol <- function(month, col) {
    if (month == TRUE & col != "Farbe...") {
      color <- colors$color[colors$name == col]
    } else {
      color <- "gray50"
    }
    return(color)
  }
  
  observeEvent(input$alle, {
    map(c("jan", "feb", "mar", "apr", "may", "jun",
          "jul", "aug", "sep", "oct", "nov", "dec"), 
        .f = updateCheckboxInput, session = session, 
        label = NULL, value = TRUE)
    map2(.x = c("col.jan", "col.feb", "col.mar", "col.apr", "col.may", "col.jun",
                "col.jul", "col.aug", "col.sep", "col.oct", "col.nov", "col.dec"),
         .y = sample(name, 12),
         .f = updateSelectInput, session = session, label = NULL, choices = NULL)
  })
  
  output$plot_prod <- renderPlot({
    data <- Logistikverzug[Logistikverzug$Produktionsdatum >=
                             as.Date(paste(as.character(input$year.start), as.character(input$month.start), "01", sep = "-"))
                           & Logistikverzug$Produktionsdatum <=
                             as.Date(paste(as.character(input$year.end), as.character(input$month.end), "31", sep = "-")),]
    month_input <- c(input$jan, input$feb, input$mar, input$apr, input$may, input$jun,
                     input$jul, input$aug, input$sep, input$oct, input$nov, input$dec)
    col_input <- c(input$col.jan, input$col.feb, input$col.mar, input$col.apr, input$col.may, input$col.jun,
                   input$col.jul, input$col.aug, input$col.sep, input$col.oct, input$col.nov, input$col.dec)
    month_col <- map2(.x = month_input, .y = col_input, .f = setcol) 
    ggplot(data = data, aes(x = Produktionsmonat, fill = Monat)) +
      geom_bar(width = 25) +
      scale_fill_manual(values = c("1" = month_col[[1]], "2" = month_col[[2]], "3" = month_col[[3]], "4" = month_col[[4]],
                                   "5" = month_col[[5]], "6" = month_col[[6]], "7" = month_col[[7]], "8" = month_col[[8]],
                                   "9" = month_col[[9]], "10" = month_col[[10]], "11" = month_col[[11]], "12" = month_col[[12]]),
                        guide = FALSE) +
      scale_x_date(date_breaks = "1 month",
                   labels = scales::date_format("%m/%y")) +
      ylab("Anzahl produzierter Komponenten") +
      theme(axis.text.x = element_text(angle = 90))
  })
  
  observeEvent(input$reset, {
    map(c("jan", "feb", "mar", "apr", "may", "jun",
          "jul", "aug", "sep", "oct", "nov", "dec"), 
        .f = updateCheckboxInput, session = session, 
        label = NULL, value = FALSE)
    map(c("col.jan", "col.feb", "col.mar", "col.apr", "col.may", "col.jun",
          "col.jul", "col.aug", "col.sep", "col.oct", "col.nov", "col.dec"),
        .f = updateSelectInput, session = session, label = NULL, 
        choices = NULL, selected = "Farbe...")
  })
  output$plot_log <- renderPlot({
    date.start <- as.Date(paste(as.character(input$year.start), 
                                as.character(input$month.start), 
                                as.character(input$day.start), 
                                sep = "-"))
    date.end <- as.Date(paste(as.character(input$year.end), 
                              as.character(input$month.end), 
                              as.character(input$day.end), 
                              sep = "-"))
    dates <- seq(date.start, date.end, by = "day")
    
    dates_df <- data.frame(dates)
    
    for (i in c(1:length(dates))) {
      dates_df$height[i] = nrow(Logistikverzug[(Logistikverzug$Produktionsdatum < dates[i] & Logistikverzug$Wareneingang > dates[i]),])
    }
    
    ggplot(data = dates_df, aes(x = dates, y = height)) +
      geom_col(width = 0.9) +
      scale_x_date(date_breaks = ifelse(nrow(dates_df) > 100, "1 month", "1 day"),
                   labels = scales::date_format("%Y-%m-%d")) +
      geom_hline(aes(yintercept = mean(height), 
                     linetype = as.character(round(mean(dates_df$height)))), color = "red") +
      scale_linetype_manual(name = "Mittelwert:", values = c(1, 1)) +
      xlab("Datum") +
      ylab("Anzahl Komponenten im Auslieferungszustand") +
      theme(axis.text.x = element_text(angle = 90), legend.position = "bottom")
    
  })
}

shinyApp(ui, server)
