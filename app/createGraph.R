library(shiny)
library(shinydashboard)
library(igraph) 
library(ggplot2)     
library(ggmap)       
library(maps)
library(geosphere)
library(leaflet)
library(DT)
library(visNetwork)
library(dplyr)
library(globe4r)

################    CARICAMENTO DEGLI AEROPORTI
airportsData <- read.csv("dataset\\airports.txt", header = FALSE, sep = ",", quote = "\"")
colnames(airportsData) <- c("openflightCode" , "name", "city", "country", "IATA", "ICAO", "latitude", "longitude", "altitude", "timezone", "DST", "tzdb", "type", "source")

################    CARICAMENTO DELLE ROUTE
routeData <- read.csv("dataset\\routes.txt", header = FALSE, sep = ",", quote = "\"")
colnames(routeData) <- c("airline", "airlineID", "sourceAirport", "sourceAirportID", "destinationAirport", "destinationAirportID", "codeshare", "stops", "equipment")

################    CARICAMENTO DELLE COMPAGNIE AEREE
airlinesData <- read.csv("dataset\\airlines.txt", header = FALSE, sep = ",", quote = "\"")
colnames(airlinesData) <- c("openflightCode" , "name", "alias", "IATA", "ICAO", "callsign", "country", "active")

################    CARICAMENTO DELLE NAZIONI
countriesData <- read.csv("dataset\\countries.txt", header = FALSE, sep = ",", quote = "\"" )
colnames(countriesData) <- c("name", "isoCode", "dafifCode")

################    CARICAMENTO DEI VELIVOLI
planesData <- read.csv("dataset\\planes.txt", header = FALSE, sep = ",", quote = "\"")
colnames(planesData) <- c("name", "IATA", "ICAO")



airportsData <- airportsData[, c(5,1,2,3,4,6:13)]  #riordino le colonne del dataframe degli aeroporti in modo che la prima colonna sia lo IATA code

routeData <- routeData[, c(3,5,1,2,4,6:9)]  #riordino le colonne del dataframe delle route in modo che le prime due colonne siano i codici IATA dell'aeroporto sorgente e destinazione





###############  CONSIDERIAMO SOLO VOLI DIRETTI (stops = 0) 

routeData <- subset(routeData , stops==0)

# Verificare che le route riguardano solo aeroporti di cui si conosce il codice IATA

for(i in 1:nrow(routeData)){
  if(nchar(routeData[i,"sourceAirport"])<3 ){
    cat(routeData[i,"sourceAirport"])
  }
}

for(i in 1:nrow(routeData)){
  if(nchar(routeData[i,"destinationAirport"])<3 ){
    cat(routeData[i,"destinationAirport"])
  }
}

# La verifica conferma che le route riguardano solo aeroporti di cui si conosce lo
# IATA, quindi si rimuovono dal dataset tutte le row per cui lo IATA è sconosciuto

airportsData <- airportsData[airportsData$IATA != "\\N", ]



# nel database sono presenti delle route in cui la destinazione e la sorgente hanno
# codici IATA che non sono presenti tra i possibili aeroporti. Vengono rimosse tali
# route


routeData <- routeData[routeData$sourceAirport %in% airportsData$IATA & routeData$destinationAirport %in% airportsData$IATA, ]
routeData <- routeData[routeData$sourceAirport != routeData$destinationAirport,]


### CREAZIONE EFFETTIVA DEL GRAFO, A PARTIRE DAI DUE DATA-FRAME

graph <- graph_from_data_frame(d=routeData , vertices=airportsData , directed=T)

# CALCOLO DEI PESI DEGLI ARCHI (DISTANZA GEOGRAFICA TRA I DUE AEROPORTI)

for(i in 1:length(E(graph))){
  fromAirport = get.edgelist(graph)[i,][1]  #id del nodo dell'aeroporto "sorgente"
  toAirport = get.edgelist(graph)[i,][2]  #id del nodo dell'aeroporto "destinazione"
 
   # coordinate aeroporto sorgente
 
  xFrom <- vertex_attr(graph, "longitude", index = fromAirport)
  yFrom <- vertex_attr(graph, "latitude", index = fromAirport)
 
  # coordinate aeroporto destinazione

  xTo <- vertex_attr(graph, "longitude", index = toAirport)
  yTo <- vertex_attr(graph, "latitude", index = toAirport)

  dist <- distm (c(xFrom, yFrom), c(xTo, yTo), fun = distHaversine) #distanza in metri

  E(graph)[i]$weight = dist/1000 #imposto il peso dell'arco uguale alla distanza (in Km) tra i due aeroporti
}




########### SCRITTURA DEL GRAFO IN UN FILE IN FORMATO "GRAPHML"

write_graph(graph, file = "airportsGraph.graphml", format = "graphml")
