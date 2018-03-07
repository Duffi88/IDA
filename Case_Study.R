<<<<<<< HEAD
library(tidyverse)
library(lubridate)
library(dplyr)


######################  EINZELTEILE  ###########################################################################

pathT11 <- "Einzelteil_T11.csv"
pathT14 <- "Einzelteil_T14.csv"
pathT15 <- "Einzelteil_T15.csv"
pathT16 <- "Einzelteil_T16.csv"
pathT19 <- "Einzelteil_T19.csv"
pathT20 <- "Einzelteil_T20.csv"

#Importieren, T16,T19 mit Komma getrennt, Rest mit Semikolon

  T11 <- read.csv2(pathT11, stringsAsFactors = FALSE)
  T14 <- read.csv2(pathT14, stringsAsFactors = FALSE)
  T15 <- read.csv2(pathT15, stringsAsFactors = FALSE)
  T16 <- read.csv(pathT16, stringsAsFactors = FALSE)
  T19 <- read.csv(pathT19, stringsAsFactors = FALSE)
  T20 <- read.csv2(pathT20, stringsAsFactors = FALSE)

#Funktion für Überblick über Dataframes
  firstGlance <- function(x){
    print(summary(x))
    cat("\n")
    print(str(x))
    cat("\n")
    print(head(x))
    cat("\n")
    print(tail(x))
  }
 
#Funktion für NA Überprüfung
  NATest <- function(x){
    #print(summarize(group_by(x, Herstellernummer)))
    #print(summarize(group_by(x, Werksnummer)))
    for (i in 1:6){
      if (nrow((x[is.na(x[i]),])) > 0){
        print(x[is.na(x[i]),])
        cat("\n")
      }
    }
  }
  
  firstGlance(T11)
#Bei Betrachtung der Datensätze fällt auf
#1.T16 Produktionsdattum ist falsch
#Origin aus der 7. Spalte entnehmen
  T16.new <- T16 %>%
    select(1:6)
  T16.new$Produktionsdatum <- as.Date(T16.new$Produktionsdatum, format = "%d",origin = as.Date("1970-01-01", format="%Y-%m-%d"))
#Entfernen von der Origin Spalte und Umbenennung
  T16.new <- select(T16.new,1:6)
  names(T16.new) <- names(T14)

#2.T19 Datensätze vermischt/nebeneinander
  T19.1 <- select(T19[!is.na(T19$IDNummer),],1:6) 
  T19.2 <- select(T19[!is.na(T19$IDNummer1),],7:12)
  names(T19.2) <- names(T19.1)
  
  #Beim Betrachten von tail() fällt auf, dass es einen Fehler in der IDNummer gibt(sowohl im Werk,
  #als auch in der Herstellernummer und Teilnummer)
  firstGlance(T19.2)
  NATest(T19.2)
  summarize(group_by(T19.2, Werksnummer))
  
  #Für die 2.Tabelle muss fast die gesammte ID neu generiert werden
  T19.2sep <- T19.2 %>%
    separate(IDNummer, c("BT","Herst","Werk","X"), sep = c("-","-","-")) 
  T19.2sep$BT <- 19
  T19.2uni <- T19.2sep%>%
    unite(col = IDNummer, BT, Herstellernummer, Werksnummer, X, sep = c("-","-","-"),remove = FALSE)%>%
    select( X1,IDNummer,Produktionsdatum,Herstellernummer,Werksnummer,Fehlerhaft)
  
  #Für die 1. Tabelle muss nur die Bauteilnummer in der ID berichtigt werden
  T19.1sep <- T19.1%>%
    separate(IDNummer, c("BT","Rest"),sep = c("-"), extra = "merge")
  T19.1sep$BT <- 19
  T19.1uni <- T19.1sep%>%
    unite(col = IDNummer, BT, Rest, sep = c("-"))%>%
    select(X1,IDNummer,Produktionsdatum,Herstellernummer,Werksnummer,Fehlerhaft)
  
  T19.new <- arrange(bind_rows(T19.1uni,T19.2uni),X1)
  names(T19.new) <- names(T14)

  


#3.T20 Datum Format ist falsch
  T20.new <- T20
  T20.new$Produktionsdatum <-  as.Date(format(as.Date(T20$Produktionsdatum, format = "%d.%m.%Y"), "%Y-%m-%d"))
  

  
  #Für alle Teile testen -> Nur bei T20 (insgesammt 4 Stück)
  NATest(T20.new)
  
  #Da man davon ausgehen muss, dass das NA ein Fehler ist wird es als fehlerhaft gespeichert
  T20.new$Fehlerhaft[T20$IDNummer == "20-209-2091-96178"] <- 1
  T20.new$X[T20$IDNummer == "20-209-2091-96178"] <- 163764
  T20.new$Herstellernummer[T20$X == 699] <- 211
  T20.new$Werksnummer[T20$X == 129019] <- 163764
  
  
#4 T15 hat falsches Datumsformat
  T15.new <- T15
  names(T15.new) <- names(T14)
  T15.new$Produktionsdatum <-  as.Date(format(as.Date(T15.new$Produktionsdatum, format = "%d.%m.%Y"), "%Y-%m-%d"))
  
#5 Alle Produktionsdaten auf Datumstyp ändern
  T11.new <- T11
  T14.new <- T14
  
  T11.new$Produktionsdatum <- as.Date(T11$Produktionsdatum, format = "%Y-%m-%d")
  T14.new$Produktionsdatum <- as.Date(T14$Produktionsdatum, format = "%Y-%m-%d")
  T19.new$Produktionsdatum <- as.Date(T19.new$Produktionsdatum, format = "%Y-%m-%d")
  
  

#Untersuchen welche Datensätze auffallen
  T11F <- T11.new %>%
    filter(Fehlerhaft == 1)
  T14F <- T14.new %>%
   filter(Fehlerhaft == 1)
  T15F <- T15.new %>%
   filter(Fehlerhaft == 1) 
  T16F <- T16.new %>%
    filter(Fehlerhaft == 1) 
  T19F <- T19.new %>%
    filter(Fehlerhaft == 1) 
  T20F <- T20.new %>%
    filter(Fehlerhaft == 1) 
  


#Es fällt auf, dass bei T15 & T20 sehr große Fehlerzahlen sind
  nrow(T15F)
  nrow(T20F)
  
#Binden der Tabellen zu E(inzel)T(eil)F(ehler)
ETF <- bind_rows("T11" = T11F, "T14" = T14F, "T15" = T15F, "T16" = T16F, "T19" = T19F, "T20" = T20F, .id = "Einzelteil")


###########################################  KOMPONENTEN  ########################################################


path.K2LE1B <- "Bestandteile_Komponente_K2LE1.csv" 
path.K2LE2B <- "Bestandteile_Komponente_K2LE2.csv" 
path.K2LE1 <- "Komponente_K2LE1.csv" 
path.K2LE2 <- "Komponente_K2LE1.csv" 


K2LE1B <- read.csv2(path.K2LE1B)
K2LE2B <- read.csv2(path.K2LE2B)
K2LE1 <- read.csv2(path.K2LE1)
K2LE2 <- read.csv2(path.K2LE2)

K2LE1 %>%
  filter(Fehlerhaft == 1) %>%
  nrow()

K2LE2 %>%
  filter(Fehlerhaft == 1) %>%
  nrow()

#Keine NA's
K2LE2[is.na(K2LE2),]



###################################### Fahrzeuge ####################################################################

path.OEM11B <- "Bestandteile_Fahrzeuge_OEM1_Typ11.csv" 
path.OEM12B <- "Bestandteile_Fahrzeuge_OEM1_Typ12.csv" 
path.OEM21B <- "Bestandteile_Fahrzeuge_OEM2_Typ21.csv" 
path.OEM22B <- "Bestandteile_Fahrzeuge_OEM2_Typ22.csv" 
path.OEM11 <- "Fahrzeuge_OEM1_Typ11.csv" 
path.OEM12 <- "Fahrzeuge_OEM1_Typ12.csv" 
path.OEM21 <- "Fahrzeuge_OEM2_Typ21.csv" 
path.OEM22 <- "Fahrzeuge_OEM2_Typ22.csv" 

OEM11B <- read.csv2(path.OEM11B, stringsAsFactors = FALSE)
OEM12B <- read.csv2(path.OEM12B, stringsAsFactors = FALSE)
OEM21B <- read.csv2(path.OEM21B, stringsAsFactors = FALSE)
OEM22B <- read.csv2(path.OEM22B, stringsAsFactors = FALSE)
OEM11 <- read.csv2(path.OEM11, stringsAsFactors = FALSE)
OEM12 <- read.csv2(path.OEM12, stringsAsFactors = FALSE)
OEM21 <- read.csv2(path.OEM21, stringsAsFactors = FALSE)
OEM22 <- read.csv2(path.OEM22, stringsAsFactors = FALSE)



head(OEM22)

OEM11 %>%
  filter(Fehlerhaft == 1) %>%
  nrow()

OEM12 %>%
  filter(Fehlerhaft == 1) %>%
  nrow()

OEM21 %>%
  filter(Fehlerhaft == 1) %>%
  nrow()

OEM22 %>%
  filter(Fehlerhaft == 1) %>%
  nrow()


OEM22B[is.na(OEM22B),]

colnames(T16)[7] <- "origin"

data <- data.frame(x = 1:10)

saveRDS(data, file = "Case_Study_Datensatz.rds")
||||||| merged common ancestors
=======
library(tidyverse)
library(lubridate)
library(dplyr)


######################  EINZELTEILE  ###########################################################################

pathT11 <- "Einzelteil_T11.csv"
pathT14 <- "Einzelteil_T14.csv"
pathT15 <- "Einzelteil_T15.csv"
pathT16 <- "Einzelteil_T16.csv"
pathT19 <- "Einzelteil_T19.csv"
pathT20 <- "Einzelteil_T20.csv"

#Importieren, T16,T19 mit Komma getrennt, Rest mit Semikolon

  T11 <- read.csv2(pathT11, stringsAsFactors = FALSE)
  T14 <- read.csv2(pathT14, stringsAsFactors = FALSE)
  T15 <- read.csv2(pathT15, stringsAsFactors = FALSE)
  T16 <- read.csv(pathT16, stringsAsFactors = FALSE)
  T19 <- read.csv(pathT19, stringsAsFactors = FALSE)
  T20 <- read.csv2(pathT20, stringsAsFactors = FALSE)

#Funktion für Überblick über Dataframes
  firstGlance <- function(x){
    print(summary(x))
    cat("\n")
    print(str(x))
    cat("\n")
    print(head(x))
    cat("\n")
    print(tail(x))
  }
 
#Funktion für NA Überprüfung
  NATest <- function(x){
    #print(summarize(group_by(x, Herstellernummer)))
    #print(summarize(group_by(x, Werksnummer)))
    for (i in 1:6){
      if (nrow((x[is.na(x[i]),])) > 0){
        print(x[is.na(x[i]),])
        cat("\n")
      }
    }
  }
  
  firstGlance(T11)
#Bei Betrachtung der Datensätze fällt auf
#1.T16 Produktionsdattum ist falsch
#Origin aus der 7. Spalte entnehmen
  T16.new <- T16 %>%
    select(1:6)
  T16.new$Produktionsdatum <- as.Date(T16.new$Produktionsdatum, format = "%d",origin = as.Date("1970-01-01", format="%Y-%m-%d"))
#Entfernen von der Origin Spalte und Umbenennung
  T16.new <- select(T16.new,1:6)
  names(T16.new) <- names(T14)

#2.T19 Datensätze vermischt/nebeneinander
  T19.1 <- select(T19[!is.na(T19$IDNummer),],1:6) 
  T19.2 <- select(T19[!is.na(T19$IDNummer1),],7:12)
  names(T19.2) <- names(T19.1)
  
  #Beim Betrachten von tail() fällt auf, dass es einen Fehler in der IDNummer gibt(sowohl im Werk,
  #als auch in der Herstellernummer und Teilnummer)
  firstGlance(T19.2)
  NATest(T19.2)
  summarize(group_by(T19.2, Werksnummer))
  
  #Für die 2.Tabelle muss fast die gesammte ID neu generiert werden
  T19.2sep <- T19.2 %>%
    separate(IDNummer, c("BT","Herst","Werk","X"), sep = c("-","-","-")) 
  T19.2sep$BT <- 19
  T19.2uni <- T19.2sep%>%
    unite(col = IDNummer, BT, Herstellernummer, Werksnummer, X, sep = c("-","-","-"),remove = FALSE)%>%
    select( X1,IDNummer,Produktionsdatum,Herstellernummer,Werksnummer,Fehlerhaft)
  
  #Für die 1. Tabelle muss nur die Bauteilnummer in der ID berichtigt werden
  T19.1sep <- T19.1%>%
    separate(IDNummer, c("BT","Rest"),sep = c("-"), extra = "merge")
  T19.1sep$BT <- 19
  T19.1uni <- T19.1sep%>%
    unite(col = IDNummer, BT, Rest, sep = c("-"))%>%
    select(X1,IDNummer,Produktionsdatum,Herstellernummer,Werksnummer,Fehlerhaft)
  
  T19.new <- arrange(bind_rows(T19.1uni,T19.2uni),X1)
  names(T19.new) <- names(T14)

  


#3.T20 Datum Format ist falsch
  T20.new <- T20
  T20.new$Produktionsdatum <-  as.Date(format(as.Date(T20$Produktionsdatum, format = "%d.%m.%Y"), "%Y-%m-%d"))
  

  
  #Für alle Teile testen -> Nur bei T20 (insgesammt 4 Stück)
  NATest(T20.new)
  
  #Da man davon ausgehen muss, dass das NA ein Fehler ist wird es als fehlerhaft gespeichert
  T20.new$Fehlerhaft[T20$IDNummer == "20-209-2091-96178"] <- 1
  T20.new$X[T20$IDNummer == "20-209-2091-96178"] <- 163764
  T20.new$Herstellernummer[T20$X == 699] <- 211
  T20.new$Werksnummer[T20$X == 129019] <- 163764
  
  
#4 T15 hat falsches Datumsformat
  T15.new <- T15
  names(T15.new) <- names(T14)
  T15.new$Produktionsdatum <-  as.Date(format(as.Date(T15.new$Produktionsdatum, format = "%d.%m.%Y"), "%Y-%m-%d"))
  
#5 Alle Produktionsdaten auf Datumstyp ändern
  T11.new <- T11
  T14.new <- T14
  
  T11.new$Produktionsdatum <- as.Date(T11$Produktionsdatum, format = "%Y-%m-%d")
  T14.new$Produktionsdatum <- as.Date(T14$Produktionsdatum, format = "%Y-%m-%d")
  T19.new$Produktionsdatum <- as.Date(T19.new$Produktionsdatum, format = "%Y-%m-%d")
  
  

#Untersuchen welche Datensätze auffallen
  T11F <- T11.new %>%
    filter(Fehlerhaft == 1)
  T14F <- T14.new %>%
   filter(Fehlerhaft == 1)
  T15F <- T15.new %>%
   filter(Fehlerhaft == 1) 
  T16F <- T16.new %>%
    filter(Fehlerhaft == 1) 
  T19F <- T19.new %>%
    filter(Fehlerhaft == 1) 
  T20F <- T20.new %>%
    filter(Fehlerhaft == 1) 
  


#Es fällt auf, dass bei T15 & T20 sehr große Fehlerzahlen sind
  nrow(T15F)
  nrow(T20F)
  
#Binden der Tabellen zu E(inzel)T(eil)F(ehler)
ETF <- bind_rows("T11" = T11F, "T14" = T14F, "T15" = T15F, "T16" = T16F, "T19" = T19F, "T20" = T20F, .id = "Einzelteil")


###########################################  KOMPONENTEN  ########################################################


path.K2LE1B <- "Bestandteile_Komponente_K2LE1.csv" 
path.K2LE2B <- "Bestandteile_Komponente_K2LE2.csv" 
path.K2LE1 <- "Komponente_K2LE1.csv" 
path.K2LE2 <- "Komponente_K2LE1.csv" 


K2LE1B <- read.csv2(path.K2LE1B)
K2LE2B <- read.csv2(path.K2LE2B)
K2LE1 <- read.csv2(path.K2LE1)
K2LE2 <- read.csv2(path.K2LE2)

K2LE1 %>%
  filter(Fehlerhaft == 1) %>%
  nrow()

K2LE2 %>%
  filter(Fehlerhaft == 1) %>%
  nrow()

#Keine NA's
K2LE2[is.na(K2LE2),]



###################################### Fahrzeuge ####################################################################

path.OEM11B <- "Bestandteile_Fahrzeuge_OEM1_Typ11.csv" 
path.OEM12B <- "Bestandteile_Fahrzeuge_OEM1_Typ12.csv" 
path.OEM21B <- "Bestandteile_Fahrzeuge_OEM2_Typ21.csv" 
path.OEM22B <- "Bestandteile_Fahrzeuge_OEM2_Typ22.csv" 
path.OEM11 <- "Fahrzeuge_OEM1_Typ11.csv" 
path.OEM12 <- "Fahrzeuge_OEM1_Typ12.csv" 
path.OEM21 <- "Fahrzeuge_OEM2_Typ21.csv" 
path.OEM22 <- "Fahrzeuge_OEM2_Typ22.csv" 

OEM11B <- read.csv2(path.OEM11B, stringsAsFactors = FALSE)
OEM12B <- read.csv2(path.OEM12B, stringsAsFactors = FALSE)
OEM21B <- read.csv2(path.OEM21B, stringsAsFactors = FALSE)
OEM22B <- read.csv2(path.OEM22B, stringsAsFactors = FALSE)
OEM11 <- read.csv2(path.OEM11, stringsAsFactors = FALSE)
OEM12 <- read.csv2(path.OEM12, stringsAsFactors = FALSE)
OEM21 <- read.csv2(path.OEM21, stringsAsFactors = FALSE)
OEM22 <- read.csv2(path.OEM22, stringsAsFactors = FALSE)



head(OEM22)

OEM11 %>%
  filter(Fehlerhaft == 1) %>%
  nrow()

OEM12 %>%
  filter(Fehlerhaft == 1) %>%
  nrow()

OEM21 %>%
  filter(Fehlerhaft == 1) %>%
  nrow()

OEM22 %>%
  filter(Fehlerhaft == 1) %>%
  nrow()


OEM22B[is.na(OEM22B),]

colnames(T16)[7] <- "origin"
>>>>>>> 4b73a17365be7b7152bcec3d47dfd1a9de117e52
