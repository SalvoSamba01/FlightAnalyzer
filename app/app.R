
# LIBRERIA GLOBI INTERATTIVI ANCORA PIU BELLA : https://globe4r.john-coene.com/index.html

# Wiki : https://shiny.rstudio.com/articles/

# https://datastorm-open.github.io/visNetwork/

# https://stackoverflow.com/questions/33620133/change-the-color-of-action-button-in-shiny

# GRAPH MATCHING IN IGRAPH: https://igraph.org/r/doc/subgraph_isomorphisms.html  (CODICE RIGA 1013)

# SELEZIONE IN DT-TABLE : https://rstudio.github.io/DT/shiny.html

#################### IMPORT DATA #################### 

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

# # CALCOLO DEI PESI DEGLI ARCHI (DISTANZA GEOGRAFICA TRA I DUE AEROPORTI)
# 
# for(i in 1:length(E(graph))){
#   fromAirport = get.edgelist(graph)[i,][1]  #id del nodo dell'aeroporto "sorgente"
#   toAirport = get.edgelist(graph)[i,][2]  #id del nodo dell'aeroporto "destinazione"
# 
#   # coordinate aeroporto sorgente
# 
#   xFrom <- vertex_attr(graph, "longitude", index = fromAirport)
#   yFrom <- vertex_attr(graph, "latitude", index = fromAirport)
# 
#   # coordinate aeroporto destinazione
# 
#   xTo <- vertex_attr(graph, "longitude", index = toAirport)
#   yTo <- vertex_attr(graph, "latitude", index = toAirport)
# 
#   dist <- distm (c(xFrom, yFrom), c(xTo, yTo), fun = distHaversine) #distanza in metri
# 
#   E(graph)[i]$weight = dist/1000 #imposto il peso dell'arco uguale alla distanza (in Km) tra i due aeroporti
# }




# ########### SCRITTURA DEL GRAFO IN UN FILE IN FORMATO "GRAPHML"
# 
# write_graph(graph, file = "airportsGraph.graphml", format = "graphml")


###########  CARICAMENTO DEL GRAFO CON I PESI GIA' ASSEGNATI

graph <- read_graph(file="airportsGraph.graphml",format = "graphml")
graph <- delete_edges(graph, which(E(graph)$weight == 0))

#######################      


# CALCOLO SOGLIE CONSIGLIATE SECONDO I QUARTILI

stats <- summary(E(graph)$weight)

# 1a soglia = 1o quartile
firstThresh <- as.double(stats["1st Qu."])

# 2a soglia = 3o quartile
secondThresh <- as.double(stats["3rd Qu."])


#################################################################


# creazione del file .txt che rappresenta la rete degli aeroporti, da dare in input
# all'algoritmo "MultiMotif" per la ricerca di motivi all'interno della rete


# E(graph)[E(graph)$weight < firstThresh]$tipoTratta <- "C"
# E(graph)[E(graph)$weight >= firstThresh & E(graph)$weight < secondThresh]$tipoTratta <- "M"
# E(graph)[E(graph)$weight >= secondThresh]$tipoTratta <- "L"
#  
# 
#  ########## CREO UN GRAFO D'APPOGGIO IN CUI OGNI ARCO CONTIENE LE ETICHETTE DEGLI ARCHI MULTIPLI, CONCATENATE FRA LORO
#  
#  graph2 <- simplify(
#    graph,
#    remove.multiple = TRUE,
#    edge.attr.comb = toString   #https://www.rdocumentation.org/packages/igraph/versions/0.7.1/topics/Combining%20attributes
#  )
#  
#  ########## MODIFICO GLI ID, CHE SONO NEL FORMATO "nX" , dove X è il numero di ID, nel formato "X"
#  
#  
#  setwd("C:\\Users\\smbsv\\Desktop\\Tesi")
#  file.create("airportsNetwork.txt")
#  write( "directed" , "airportsNetwork.txt",append = TRUE)
#  write( length(V(graph2)) , "airportsNetwork.txt",append = TRUE)
#  
#  for(i in 1:length(V(graph2))){
#    write( V(graph2)[i]$name , "airportsNetwork.txt",append = TRUE)
#  }
#  
#  
#  E(graph2)$tipoTratta <- gsub(" ", "", E(graph2)$tipoTratta, fixed = TRUE)  #toString() concatena due stringhe x e y nella forma "x, y". E' necessario quindi rimuovere lo spazio
#  
#  for(i in 1:length(E(graph2))){
#    write(paste(c(toString(as.integer(ends(graph2, i, names = FALSE)[1,1])-1) , toString(as.integer(ends(graph2, i, names = FALSE)[1,2])-1) , E(graph2)[i]$tipoTratta) , collapse = "\t") , "airportsNetwork.txt",append = TRUE) #names=FALSE nella funzione "ends" -> restituisce l'indice e non il nome del vertice
#  }



############### APP #######################


ui <- dashboardPage(
  dashboardHeader(title = "FlightAnalyzer"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Ricerca", tabName = "searchAirport", icon = icon("search")),
      menuItem("Globo 3D", tabName = "3dGlobe", icon = icon("globe")),
      menuItem("Misure di centralità", tabName = "centrality", icon = icon("arrows-to-circle")),
      menuItem("Ricerca di motivi", tabName = "motifSearch", icon = icon("diagram-project"))
    )
  ),
  dashboardBody(
    tabItems(
      
      tabItem(tabName = "dashboard",
              fluidRow(
                box(
                  div(
                    h2("Tipologie di tratte"),
                    style = "text-align: center;"
                  ),
                  column(
                    width = 5,
                    sliderInput("firstThresh", "Soglia voli a corto raggio", value = 0 , min = 0 , max = as.integer(max(E(graph)$weight)-2)),
                    sliderInput("secondThresh", "Soglia voli a medio raggio", value = 0 , min = 0 , max = as.integer(max(E(graph)$weight)-1)),
                    checkboxInput("suggestedThresh" , "Soglie consigliate" , value = FALSE)
                  ),
                  column(
                    width = 7,
                    plotOutput("threshHistogram")
                  )
                ),
                
                box(
                  div(
                    h2("Misure di base della rete"),
                    style = "text-align: center;"
                  ),
                  h4("Numero di nodi: ",length(V(graph))),
                  h4("Numero di archi: ",length(E(graph))),
                  h4("Numero di triangoli: ",length(triangles(graph))/3),
                  h4("Coefficente di clustering globale: " , round(transitivity(graph,type = "global",isolates = "NaN"),4)),
                  h4("Assortatività: ",round(assortativity_degree(graph, directed = TRUE),4)),
                  h4("Distanza media: ",round(mean_distance(graph,directed = TRUE),4) , " (Km)"),
                  h4("Diametro: ", round(diameter(graph),4) , " (Km)")
                )
              ),
              fluidRow(
                box(
                  width = 4,
                  plotOutput("mostActiveAirlines")
                ),
                box(
                  width = 4,
                  plotOutput("mostDenseCountry")
                ),box(
                  width = 4,
                  plotOutput("denseCities")
                )
              ),
              fluidRow(
                box(
                  width = 12,
                  div(
                    h2("Distribuzione dei gradi"),
                    style = "text-align: center;"
                  ),
                  checkboxInput("showZero","Considera nodi isolati",value=FALSE),
                  numericInput("minDegree","Grado minimo da considerare" , min=0,max=max(degree(graph , v = V(graph) , mode = "all"))-1,value=1),
                  numericInput("maxDegree","Grado massimo da considerare" , min=1,max=max(degree(graph , v = V(graph) , mode = "all")),value=max(degree(graph , v = V(graph) , mode = "all"))),
                  plotOutput("degreeDistribution")
                )
              )
      ) , 
      # Second tab content
      tabItem(tabName = "searchAirport",
              fluidRow(
                box(
                  width = 4,
                  h3("Inserisci chiave di ricerca:"),
                  radioButtons("key","Cerca tramite: ",choices=c("IATA","ICAO","Nome","Nazione","Compagnia aerea") , selected="IATA"),
                  textInput("value","Inserisci il valore da cercare: ")
                ),
                box(
                  width = 8,
                  leafletOutput("worldMap")
                )
              ),
              fluidRow(
                column(
                  width=12,
                  box(
                    width=12,
                    h3("Aeroporti trovati" , style = "text-align: center;"),
                    tags$br(),
                    DTOutput('tabledata'),
                  )
                )
              ),
              fluidRow(
                box(
                  width = 12,
                  h3("Compagnie aeree che servono gli aeroporti trovati"),
                  br(),
                  DTOutput('tableAirlines')
                )
              )
      ),
      
      tabItem( tabName = "3dGlobe",
               fluidRow(
                 column( width = 5,
                         box(
                           radioButtons(
                             "filters", 
                             "Filtri" , 
                             choiceNames = list("Tutti gli aeroporti","Per continente"), 
                             choiceValues = list("all","continent")
                           ),
                           
                           conditionalPanel(
                             condition = "input.filters == 'continent'",
                             checkboxGroupInput(
                               "continentCheckbox", 
                               "Seleziona il Continente:",
                               choices = c("Europa"="EL", "Asia"="UOZVRW", "Africa"="GDHF", "America"="BCKMTS","Oceania"="YAPN") #come da definizione di "continente" data dalle nazioni unite                       )
                             )
                           ),
                           checkboxInput("globeArcs" , "Mostra rotte"),
                           conditionalPanel(
                             condition = "input.globeArcs",
                             sliderInput("maxLength" ,"Lunghezza massima rotte visualizzate",value = floor(min(E(graph)$weight))+500 , min = floor(min(E(graph)$weight)) , max = ceiling(max(E(graph)$weight)))
                           )
                         ),
                         column( width = 1,
                                 globeOutput("globe", width = "650px" , height = "650px")
                         )
                 )
               )
               
      ),
      
      tabItem( tabName = "centrality",
               fluidRow(
                 box(
                   width = 6,
                   div(
                     h2("DEGREE CENTRALITY"),
                     style = "text-align: center;"
                   ),
                   verbatimTextOutput("degree")
                 ),
                 box(
                   width = 6,
                   div(
                     h2("CLOSENESS CENTRALITY"),
                     style = "text-align: center;"
                   ),
                   verbatimTextOutput("closeness")
                 )
               ),
               fluidRow(
                 box(
                   width = 6,
                   div(
                     h2("BETWENNESS CENTRALITY"),
                     style = "text-align: center;"
                   ),
                   verbatimTextOutput("betwennes")
                 ),
                 box(
                   width = 6,
                   div(
                     h2("PAGE-RANK CENTRALITY"),
                     style = "text-align: center;"
                   ),
                   verbatimTextOutput("pagerank")
                 )
               ),
               br(),br(),
               fluidRow(
                 column( width = 5,
                         box(
                           radioButtons(
                             "measureFilters", 
                             "Misura" , 
                             choiceNames = list("Degree C.","Closeness C.","Betweennes C.","PageRank C."), 
                             choiceValues = list("degree","closeness","betweennes","pagerank")
                           )
                         ),
                         column( width = 1,
                            globeOutput("measureGlobe", width = "650px" , height = "650px")
                         )
                 )
               )
               
      ),
      tabItem( tabName = "motifSearch",
               
               fluidRow(
                 div(
                   h2("RICERCA DI MOTIVI"),
                   style = "text-align: center; "
                 )
               ),
               
               fluidRow(
                 column(style = "border: 4px double #222d32; padding: 5px; margin:20px;",
                   width = 2,
                   fluidRow(
                     br(),
                     radioButtons("labelType","Etichetta dei nodi: ",choices=c("Nome aeroporto","Nazione") , selected="Nome aeroporto"),
                     br(),
                     textInput(inputId = "newQueryName",label = "Inserisci il nome della nuova query" , width = "70%"),
                     br(),
                     actionButton("addQuery","Aggiungi query", style="color: #fff; background-color: #337ab7; border-color: #2e6da4; align: center"),
                     br(),
                     br(),
                     actionButton("resetQuery","Reset query attuale", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                     br(),br(),
                     align = "center"
                  )
                 ),
                 column(
                   width = 9,
                   visNetworkOutput("motifGraph")
                 )
               ),
               
               fluidRow(
                 actionButton("deleteQuery","Elimina TUTTE le query", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                 actionButton("searchMotif", "Cerca motivi", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                 align = "center"
                ),
               
               
               fluidRow(
                 br(),
                 div(
                   h2("RISULTATI"),
                   style = "text-align: center; "
                 ),
                 br(),br(),
                 DTOutput ("motifSearchResults")
               ),
               
               br(),
               fluidRow(
                 column(
                   width=6,
                   globeOutput("motifGlobe" , width="700px" , height="700px")
                 ),
                 
                 column(
                   width=6, 
                   uiOutput("legenda")
                 )
                 
               )
               
      )
    )
  )
)     



init.nodes.df = data.frame(id = character(),
                           label = character(),
                           stringsAsFactors = F)
init.edges.df = data.frame(id = character(),
                           from = character(), 
                           to = character(),
                           label = character(),
                           stringsAsFactors = F)


server <- function(input, output , session) {
  
  
  #################### CREAZIONE GRAFO INTERATTIVO ########################
  
  
  graph_data = reactiveValues(
    nodes = init.nodes.df,
    edges = init.edges.df
  )
  
  
  output$motifGraph <- renderVisNetwork({
    visNetwork(graph_data$nodes, graph_data$edges) %>%
      visOptions(manipulation = list(
        enabled = TRUE,
        editEdgeCols = c("label"),
        addEdgeCols = c("label"),
        editNodeCols = c("label"),
        addNodeCols = c("label")
      )
      )%>%
      visEdges(arrows = 'to')%>%
      visInteraction(navigationButtons = TRUE)
  })
  
  
  # If the user edits the graph, this shows up in
  # `input$[name_of_the_graph_output]_graphChange`.  This is a list whose
  # members depend on whether the user added a node or an edge.  The "cmd"
  # element tells us what the user did.
  observeEvent(input$motifGraph_graphChange, {
    # If the user added a node, add it to the data frame of nodes.
    if(input$motifGraph_graphChange$cmd == "addNode") {
      newID <- NULL
      if(nrow(graph_data$nodes) == 0)
        newID <- 0
      else
        newID <- max(as.integer(graph_data$nodes$id))+1
      
      #id = input$motifGraph_graphChange$id
      
      temp = bind_rows(
        graph_data$nodes,
        data.frame(id = toString(newID),
                   label = input$motifGraph_graphChange$label,
                   stringsAsFactors = F)
      )
      graph_data$nodes = temp
    }
    
    # If the user added an edge, add it to the data frame of edges.
    else if(input$motifGraph_graphChange$cmd == "addEdge") {
      temp = bind_rows(
        graph_data$edges,
        data.frame(id = input$motifGraph_graphChange$id,
                   from = input$motifGraph_graphChange$from,
                   to = input$motifGraph_graphChange$to,
                   stringsAsFactors = F)
      )
      graph_data$edges = temp
    }
    # If the user edited a node, update that record.
    else if(input$motifGraph_graphChange$cmd == "editNode") {
      temp = graph_data$nodes
      temp$label[temp$id == input$motifGraph_graphChange$id] = input$motifGraph_graphChange$label
      graph_data$nodes = temp
    }
    # If the user edited an edge, update that record.
    else if(input$motifGraph_graphChange$cmd == "editEdge") {
      temp = graph_data$edges
      temp$label[temp$id == input$motifGraph_graphChange$id] = input$motifGraph_graphChange$label
      # temp$from[temp$id == input$motifGraph_graphChange$id] = input$motifGraph_graphChange$from
      # temp$to[temp$id == input$motifGraph_graphChange$id] = input$motifGraph_graphChange$to
      graph_data$edges = temp
    }
    # If the user deleted something, remove those records.
    else if(input$motifGraph_graphChange$cmd == "deleteElements") {
      for(node.id in input$motifGraph_graphChange$nodes) {
        temp = graph_data$nodes
        temp = temp[temp$id != node.id,]
        graph_data$nodes = temp
      }
      for(edge.id in input$motifGraph_graphChange$edges) {
        temp = graph_data$edges
        temp = temp[temp$id != edge.id,]
        graph_data$edges = temp
      }
      
      # si sistemano di nuovo gli indici dei nodi in modo che vadano da 0 a N-1 (serve per la successiva ricerca di motivi)
      
      for(i in 0:nrow(graph_data$nodes)-1)
        graph_data$nodes$id[i+1] = i
    }
  })
  
  updateSelectizeInput(session, 'aeroportoPartenza', choices = unique(sort(airportsData$name)), server = TRUE)
  updateSelectizeInput(session, 'aeroportoArrivo', choices = unique(sort(airportsData$name)), server = TRUE)
  
  
  nRes <- 0
  
  key <- reactive({
    input$key
  })
  
  observeEvent(input$key , {
    updateTextInput(session , "value" , value="")
  })
  
  
  output$tabledata <- renderDT({
    occ <- NULL
    if(input$value!=""){
      if(key() == "Nome"){
        occ <- airportsData[grep(input$value, airportsData$name , ignore.case = TRUE),][,c("IATA","ICAO","name","city","country")]
      }
      
      else if(key() == "Nazione"){
        occ <- airportsData[grep(input$value, airportsData$country , ignore.case = TRUE),][,c("IATA","ICAO","name","city","country")]
      }
      
      else if(key()=="IATA" | key()=="ICAO")
        occ <- airportsData[airportsData[,key()] == toupper(input$value),][,c("IATA","ICAO","name","city","country")]
      
      else if(key()=="Compagnia aerea"){
        
        edges <- E(graph)[E(graph)$airline == airlinesData[airlinesData$name == input$value ,]$IATA]
        air <- unlist(unique(as.list(ends(graph , edges))))
        occ <- airportsData[airportsData$name %in% air,][,c("IATA","ICAO","name","city","country")]
      }
      
      
      if(nrow(occ) > 0 ){
        nRes <- nrow(occ)
      }
      
    }  
    occ
  })
  
  output$mostActiveAirlines <- renderPlot({
    
    tableAirlines <- table(get.edge.attribute(graph , "airline"))
    topAirlines <- head(sort(tableAirlines , decreasing=T) , 3)
    names(topAirlines) = c(na.omit(airlinesData[airlinesData$IATA == names(topAirlines)[1],])$name , na.omit(airlinesData[airlinesData$IATA == names(topAirlines)[2],])$name , na.omit(airlinesData[airlinesData$IATA == names(topAirlines)[3],])$name)
    barplot(topAirlines, main="Compagnie con più rotte", xlab="Compagnia", ylab="# rotte", col="lightblue")
  })
  
  output$mostDenseCountry <- renderPlot({
    
    tableCountry <- table(airportsData[airportsData$country != "",]$country)
    topCountry <- head(sort(tableCountry , decreasing=T) , 3)
    barplot(topCountry, main="Stati con più aeroporti", xlab="Stato", ylab="# aeroporti", col="lightblue")
  })
  
  output$denseCities <- renderPlot({
    tableCities <- table(airportsData[airportsData$city != "",]$city)
    topCities <- head(sort(tableCities , decreasing=T) , 3)
    barplot(topCities, main="Città con più aeroporti", xlab="Città", ylab="# aeroporti", col="lightblue")
  })
  
  
  
  output$tableAirlines <- renderDT({
    if(input$value!=""){
      
      if(key() == "Nome"){
        occ <- airportsData[grep(input$value, airportsData$name , ignore.case = TRUE),][,c("IATA","ICAO","name","city","country")]
      }
      
      else if(key() == "Nazione"){
        occ <- airportsData[grep(input$value, airportsData$country , ignore.case = TRUE),][,c("IATA","ICAO","name","city","country")]
      }
      
      else if(key()=="IATA" | key()=="ICAO")
        occ <- airportsData[airportsData[,key()] == toupper(input$value),][,c("IATA","ICAO","name","city","country")]
      
      else if(key()=="Compagnia aerea"){
        
        edges <- E(graph)[E(graph)$airline == airlinesData[airlinesData$name == input$value ,]$IATA]
        air <- unlist(unique(as.list(ends(graph , edges))))
        occ <- airportsData[airportsData$name %in% air,][,c("IATA","ICAO","name","city","country")]
      }
      
      if(nrow(occ) > 0 ){
        routes <- routeData[routeData$sourceAirport %in% occ$IATA | routeData$destinationAirport %in% occ$IATA,]
        airlines <- routes$airline
        airlinesData[airlinesData$IATA %in% routes$airline,][,c("name","country","IATA","ICAO")]
      }
    }
  })
  
  
  output$worldMap <- renderLeaflet({
    if(input$value!=""){
      if(key() == "Nome"){
        occ <- airportsData[grep(input$value, airportsData$name , ignore.case = TRUE),]
      }
      
      else if(key() == "Nazione"){
        occ <- airportsData[grep(input$value, airportsData$country , ignore.case = TRUE),]
      }
      
      else if(key()=="IATA" | key()=="ICAO"){
        occ <- airportsData[airportsData[,key()] == toupper(input$value),]
      }
      
      else if(key()=="Compagnia aerea"){
        edges <- E(graph)[E(graph)$airline == airlinesData[airlinesData$name == input$value ,]$IATA]
        air <- unlist(unique(as.list(ends(graph , edges))))
        occ <- airportsData[airportsData$name %in% air,]
      }
      
      if(nrow(occ)>0){
        leaflet(data = occ) %>% 
          addProviderTiles(providers$CartoDB.Positron) %>%
          addMarkers(~longitude, ~latitude, popup = paste(occ$name,"<br>" , occ$country ,"<br>IATA: ", occ$IATA ,"<br>ICAO: ", occ$ICAO) , clusterOptions = markerClusterOptions())
      }
    }
    
  })
  
  
  maxLength <- reactive({
    input$maxLength
  })
  
  
  output$globe <- renderGlobe({
    temp <- airportsData[,c(1,3,4,5,6,7,8)]
    airportsDataSubset <- temp
    
    globeArcs <- data.frame(yFrom = double() , xFrom = double() , yTo = double() , xTo = double())
    
    if (input$filters == "continent"){
      if(length(input$continentCheckbox)!=0){
        validChars <- strsplit(paste(input$continentCheckbox,collapse=""), "")[[1]]
        airportsDataSubset <- temp[substr(temp$ICAO, 1, 1) %in% validChars, ]
        
      }
    }
    
    if(input$globeArcs == TRUE){
      df <- as_data_frame(get.edgelist(graph, names = FALSE) , get.edge.attribute(graph,name="weight"))
      weights <- get.edge.attribute(graph,name="weight")
      arcs <- cbind(df,weights)
      arcs <- arcs[arcs$weights <= maxLength() & get.vertex.attribute(graph , "name" , arcs$V1)  %in% airportsDataSubset$name & get.vertex.attribute(graph , "name" , arcs$V2)  %in% airportsDataSubset$name, ]
      xFrom <- V(graph)[arcs$V1]$longitude
      yFrom <- V(graph)[arcs$V1]$latitude
      xTo <- V(graph)[arcs$V2]$longitude
      yTo <- V(graph)[arcs$V2]$latitude
      
      globeArcs <- cbind(yFrom,xFrom,yTo,xTo)
      
    }
    
    airportsDataSubset <- as.data.frame(sapply(airportsDataSubset,as.character))

    # creazione etichette
    
    airportsDataSubset$labels <- apply(airportsDataSubset, 1, function(row) {
      values <- row[c(2,3,4)]
      names <- names(airportsDataSubset)[c(2,3,4)]
      combined <- paste(sprintf("%s: %s", names, values), collapse = " ; ")
      combined
    })
    
    
    create_globe()%>%
      globe_arcs(
        coords(
          start_lat = yFrom,
          start_lon = xFrom,
          end_lat = yTo,
          end_lon = xTo,
          stroke = 0.5
        ),
        data = as.data.frame(globeArcs)
      )%>%
      globe_bars(
        coords(latitude,longitude, label = labels),
        data = airportsDataSubset
      )
    
  })
  
  
  output$measureGlobe <- renderGlobe({
    
    airportsList <- airportsData[,c(3,7,8)]
    indici <- match(airportsList$name, V(graph)$name)
    
    if (input$measureFilters == "degree"){
      val <- degree(graph)[indici]
      airportsList <- cbind(airportsList, values = val)
    }
    
    else if (input$measureFilters == "closeness"){
      val <- closeness(graph,v = V(graph),weights = E(graph)$weight,normalized = TRUE)[indici]
      airportsList <- cbind(airportsList, values = val)
    }
    
    else if (input$measureFilters == "betweennes"){
      val <- betweenness(graph,v = V(graph),directed = TRUE,weights = E(graph)$weight,normalize = TRUE)[indici]
      airportsList <- cbind(airportsList, values = val)
    }
    
    else if (input$measureFilters == "pagerank"){
      val <- page_rank(graph,algo = "prpack",v = V(graph),weights = E(graph)$weight)$vector[indici]
      airportsList <- cbind(airportsList, values = val)
    }
    
    airportsList <- as.data.frame(sapply(airportsList,as.character))
    
    airportsList$labels <- paste(airportsList$name, "\n", airportsList$values)
    
    airportsList$values <- as.double(airportsList$values)
    
    
    create_globe()%>%
      globe_bars(
        coords(latitude,longitude, label = labels , altitude = values),
        data = airportsList
      )%>%
      scale_bars_altitude()
    
  })
  
  aeroportoPartenza <- reactive({
    input$aeroportoPartenza
  })
  
  aeroportoArrivo <- reactive({
    input$aeroportoArrivo
  })
  
  
  observe({
    if(!is.na(input$firstThresh) & !is.na(input$secondThresh))
      if (input$secondThresh <= input$firstThresh) {
        updateNumericInput(session, "secondThresh", value = as.integer(input$firstThresh + 1))
      }
  })
  
  observe({
    if (input$firstThresh > max(E(graph)$weight)-2) {
      updateNumericInput(session, "firstThresh", value = as.integer(max(E(graph)$weight)-2))
    }
  })
  
  observe({
    if (input$secondThresh > max(E(graph)$weight)-1) {
      updateNumericInput(session, "secondThresh", value = as.integer(max(E(graph)$weight)-1))
    }
  })
  
  
  observe({
    if (input$suggestedThresh) {
      stats <- summary(E(graph)$weight)
      updateNumericInput(session, "firstThresh", value = as.integer(stats["1st Qu."]))
      updateNumericInput(session, "secondThresh", value = as.integer(stats["3rd Qu."]))
    }
  })
  
  output$threshHistogram <- renderPlot({
    n1 <- sum(E(graph)$weight < input$firstThresh)
    n2 <- sum(E(graph)$weight >= input$firstThresh & E(graph)$weight <= input$secondThresh)
    n3 <- sum(E(graph)$weight > input$secondThresh)
    
    barplot(c(n1, n2, n3), names.arg = c("Corto raggio", "Medio raggio", "Lungo raggio") , col = c("red","blue","green"))
  })
  
  firstThresh <- reactive({
    input$firstThresh
  })
  
  
  secondThresh <- reactive({
    input$secondThresh
  })
  
  observe({
    E(graph)[E(graph)$weight < input$firstThresh]$tipoTratta <- "C"
    
    E(graph)[E(graph)$weight >= input$firstThresh & E(graph)$weight < input$secondThresh]$tipoTratta <- "M"
    
    E(graph)[E(graph)$weight >= input$secondThresh]$tipoTratta <- "L"
  })
  
  output$betwennes <- renderText({
    
    betweennesMeasure <- betweenness(
      graph,
      v = V(graph),
      directed = TRUE,
      weights = E(graph)$weight,
      normalize = TRUE
    )
    
    max_index <- which.max(betweennesMeasure)
    mostCentralAirport <- names(betweennesMeasure)[max_index]
    
    paste("Aeroporto che massimizza la Betwenness Centrality:\n ", mostCentralAirport, "\nValore: " , betweennesMeasure[mostCentralAirport])
  })
  
  maxDegree <- reactive({
    input$maxDegree
  })
  
  
  minDegree <- reactive({
    input$minDegree
  })
  
  
  degreeMeasure <- degree(graph , v = V(graph) , mode="all")
  
  
  observeEvent(input$minDegree , {
    output$degreeDistribution <- renderPlot({
      newDegreeMeasure <- degreeMeasure
      if(!input$showZero){
        newDegreeMeasure <- degreeMeasure[degreeMeasure != 0] #rimuovo nodi isolati
      }
      degreeFrequencies <- table(newDegreeMeasure)
      degreeFrequencies <- subset(degreeFrequencies, as.integer(names(degreeFrequencies)) >= minDegree() & as.integer(names(degreeFrequencies)) <= maxDegree())
      
      
      barplot(degreeFrequencies, main="Distribuzione dei gradi" , xlab="Grado", ylab="# nodi", col="blue" , las=2)
      
    })
  })
  
  
  observeEvent(input$maxDegree , {
    output$degreeDistribution <- renderPlot({
      newDegreeMeasure <- degreeMeasure
      if(!input$showZero){
        newDegreeMeasure <- degreeMeasure[degreeMeasure != 0] #rimuovo nodi isolati
      }
      degreeFrequencies <- table(newDegreeMeasure)
      
      degreeFrequencies <- subset(degreeFrequencies, as.integer(names(degreeFrequencies)) >= minDegree() & as.integer(names(degreeFrequencies)) <= maxDegree())
      
      barplot(degreeFrequencies, main="Distribuzione dei gradi" , xlab="Grado", ylab="# nodi", col="blue" , las=2)
      
    })
  })
  
  output$degreeDistribution <- renderPlot({
    newDegreeMeasure <- degreeMeasure
    if(!input$showZero){
      newDegreeMeasure <- degreeMeasure[degreeMeasure != 0] #rimuovo nodi isolati
    }
    degreeFrequencies <- table(newDegreeMeasure)
    degreeFrequencies <- subset(degreeFrequencies, names(degreeFrequencies) >= minDegree() & names(degreeFrequencies) <= maxDegree())
    
    barplot(degreeFrequencies, main="Distribuzione dei gradi" , xlab="Grado", ylab="# nodi", col="blue" , las=2)
    
  })
  
  output$degree <- renderText({
    newDegreeMeasure <- degreeMeasure
    if(!input$showZero){
      newDegreeMeasure <- degreeMeasure[degreeMeasure != 0] #rimuovo nodi isolati
    }
    max_index <- which.max(newDegreeMeasure)
    highestDegree <- names(newDegreeMeasure)[max_index]
    paste("Aeroporto che massimizza la Degree Centrality:\n ", highestDegree , "(Grado:" , newDegreeMeasure[highestDegree] ,")\nGrado medio: ", mean(newDegreeMeasure))
    
  })
  
  output$closeness <- renderText({
    closenessMeasure <- closeness(
      graph,
      v = V(graph),
      weights = E(graph)$weight,
      normalized = TRUE
    )
    
    max_index <- which.max(closenessMeasure)
    mostCLoseAirport <- names(closenessMeasure)[max_index]
    
    paste("Aeroporto che massimizza la Closeness Centrality:\n ", mostCLoseAirport , "\nValore: " , closenessMeasure[mostCLoseAirport])
    
  })
  
  output$pagerank <- renderText({
    pagerankMeasure <- page_rank(
      graph,
      algo = "prpack",
      v = V(graph),
      weights = E(graph)$weight
    )
    
    max_index <- which.max(pagerankMeasure$vector)
    highestPageRank <- names(pagerankMeasure$vector)[max_index]
    
    paste("Aeroporto che massimizza la Page-Rank Centrality:\n ", highestPageRank , "\nValore: " , pagerankMeasure$vector[highestPageRank])
    
  })
  
  
  ##################    APPENA SI PREME IL PULSANTE DI ID "searchMotif", CREARE LA QUERY E POI CHIAMARE MULTIMOTIF
  
  
  #ottengo dinamicamente il valore del nome della query
  
  newQueryName <- reactive({
    input$newQueryName
  })
  
  
  observeEvent(input$deleteQuery,{
    
    # cancello il file di query se se esistente
    
    if(file.exists("query.txt"))
      file.remove("query.txt")
    
    showNotification(paste("Query cancellate"), duration = 4  , type="message")
    
  })
  
  
  
  observeEvent(input$resetQuery,{
    
    # cancello il motivo creato attualmente
    
    graph_data$nodes <- graph_data$nodes[0, ]
    graph_data$edges <- graph_data$edges[0, ]
    
  })
  
  
  
  #aggiungo la query al file
  
  observeEvent(input$addQuery,{
    
    # se il file delle query non esiste, lo si crea
    
    if(!file.exists("query.txt"))
      file.create("query.txt")
    
    if(nrow(graph_data$nodes)== 0 | nrow(graph_data$edges) == 0)
      showNotification(paste("Devi inserire almeno un nodo e un arco"), duration = 4  , type="error")
    
    else if(newQueryName() == "" |  any(grepl(paste0("#",newQueryName()), readLines("query.txt"))))
      showNotification(paste("Inserire un nome valido (no nome vuoto o nomi duplicati)"), duration = 4  , type="error")
    
    else{
      
      write(paste0("#",newQueryName()),"query.txt",append = TRUE)
      write(nrow(graph_data$nodes) , "query.txt",append = TRUE)
      
      # scrittura del numero e delle etichette dei nodi
      
      for(i in 1:nrow(graph_data$nodes)){
        if(graph_data$nodes$label[i]=="")
          write( "?" , "query.txt",append = TRUE)
        else
          write( graph_data$nodes$label[i] , "query.txt",append = TRUE)
      }
      
      # se non è stata specificata un'etichetta per l'arco, essa è considerata come "?"
      
      for(i in 1:nrow(graph_data$edges))
        if(is.na(graph_data$edges$label[i]) | graph_data$edges$label[i] == "")
          graph_data$edges$label[i] <- "?"
      
      # unifico le righe relative a multiarchi (stessa sorgente e destinazione)
      
      multiedgeDF <- aggregate(graph_data$edges$label, by = list(graph_data$edges$from, graph_data$edges$to), FUN = function(x) {paste(x, collapse = ",")})
      colnames(multiedgeDF) <- c("from", "to", "label")
      
      #scrittura degli archi nel file query
      
      for(i in 1:nrow(multiedgeDF))
        write( paste( c(multiedgeDF$from[i] , multiedgeDF$to[i] , multiedgeDF$label[i] ),  collapse = "\t"), "query.txt",append = TRUE)
      
      # cancello il motivo creato
      
      graph_data$nodes <- graph_data$nodes[0, ]
      graph_data$edges <- graph_data$edges[0, ]
      
      showNotification(paste("Query aggiunta correttamente"), duration = 4  , type="message")
      
    }
    
  })
  
  
  labelType <- reactive({
    input$labelType
  })
  
  

  
  # controllo se si clicca una cella della tabella dei motivi trovati, per stampare i motivi selezionati sul globo
  
  observeEvent(input$motifSearchResults_rows_selected, {
    
    if(labelType() == "Nome aeroporto"){
    
    selectedRows <- sort(input$motifSearchResults_rows_selected)
      nodes <- read.delim('results.txt',sep='\t' , header = TRUE , colClasses = c("character", "character","character","character","character","character"))$Motif_Nodes[selectedRows]
      edges <- read.delim('results.txt',sep='\t' , header = TRUE , colClasses = c("character", "character","character","character","character","character"))$Motif_edges[selectedRows]
      
      nodes <- strsplit(nodes,",",fixed=TRUE)
      edges <- strsplit(edges, "[)(]")
      edges <- lapply(edges, function(z){ z[!is.na(z) & z != ""]})
      
      for(i in 1:length(edges)){
        for(j in 1:length(edges[[i]])){
          edges[[i]][j] <- strsplit (substring(edges[[i]][j] , 1 , nchar(edges[[i]][j])-2) , "," )
        }
      }
      
      nodesDF <- data.frame()
      edgesDF <- data.frame()
      legenda <- data.frame()
      nodeColors <- c()
      values <- c()
      arcAltitude <- c()
      
      
      for(i in 1:length(nodes)){
        
        name <- unlist(nodes[[i]])
        latitude <- list()
        longitude <- list()

        for(z in 1:length(name)){
          nome <- name[z]
          latitude <- c(latitude , V(graph)[V(graph)$name == nome]$latitude)
          longitude <- c(longitude , V(graph)[V(graph)$name == nome]$longitude)
        }
        
        for(z in 1:length(unlist(name))){
          nodesDF <- rbind(nodesDF , c(unlist(name)[z] , unlist(latitude)[z] , unlist (longitude)[z] ))
        }
        
        colnames(nodesDF) <- c("name","lat","long")  
        
        for(j in 1:length(edges[[i]])){
          fromNode <- as.integer(edges[[i]][[j]][1])+(length(nodes[[i]]) * (i-1))
          toNode <- as.integer(edges[[i]][[j]][2])+(length(nodes[[i]]) * (i-1))
          data <- c(fromNode,toNode)
          edgesDF <- rbind(edgesDF , data)
        }
        
        colnames(edgesDF) <- c("from","to")
        
        color <- paste0("#", paste(sample(c(0:9, letters[1:6]), 6, replace = TRUE), collapse = ""))
        
          for(w in 1:length(nodes[[i]])){
            nodeColors <- c(nodeColors,color)
            values <- c(values,as.integer((length(nodes[[i]]) - i + 2)*20) )
          }
        
        globeArcs <- data.frame()
        
         for(k in 1:nrow(edgesDF)){
           xFrom <- as.double(nodesDF[as.integer(edgesDF[k,"from"])+1,"long"])
           yFrom <- as.double(nodesDF[as.integer(edgesDF[k,"from"])+1,"lat"])
           xTo <- as.double(nodesDF[as.integer(edgesDF[k,"to"])+1,"long"])
           yTo <- as.double(nodesDF[as.integer(edgesDF[k,"to"])+1,"lat"])
           data <- c(yFrom,xFrom,yTo,xTo)
           globeArcs <- rbind(globeArcs , data)
         }
        
        colnames(globeArcs) <- c("yFrom","xFrom","yTo","xTo")
        
        for(l in 1:length(edges[[i]])){
          legenda <- rbind(legenda , c(as.integer(sort(selectedRows[i])),color))
          arcAltitude <- c(arcAltitude,(as.integer((length(edges[[i]]) - i + 2)*10))/100 )
        }
        
        colnames(legenda) <- c("motif","color")
        
      }
      
       output$motifGlobe <- renderGlobe({
         create_globe()%>%
           globe_arcs(
             coords(
               start_lat = yFrom,
               start_lon = xFrom,
               end_lat = yTo,
               end_lon = xTo,
               color = color,
               label = motif,
               altitude = arcAltitude,
               stroke = 0.7,
               dash_length = .2,
               dash_gap = .08,
               dash_animate_time = 6000
             ),
             data = cbind(globeArcs,legenda,arcAltitude)
           )%>%
         globe_bars(
             coords(lat,long, label = name,color=nodeColors),
             data = cbind(nodesDF,nodeColors)
           )
         })
       
       generateSquare <- function(color, text) {
         paste0(
           '<div style="display: flex; flex-direction: row; align-items: center; margin-right: 10px;">',
           '<div style="width: 40px; height: 40px; margin-left: 20%; background-color:', color, '; border-style: solid; border-width: 2px; border-color: black;  "></div>',
           '<div style="margin-left: 10px; font-size: 15px; font-weight: bold;">', paste0("Motivo n°",text,collapse=" "), '</div><br><br><br>',
           '</div>'
         )
       }
       
       output$legenda <- renderUI({
         HTML('<h3>Legenda</h3>')
         legendaRows <- apply(unique(legenda), 1, function(row) {
           generateSquare(row[2], row[1])
         })
         HTML(paste(legendaRows, collapse = ""))
       })
       
    }
    
    else if(labelType() == "Nazione"){
      data(world.cities)
      selectedRows <- sort(input$motifSearchResults_rows_selected)
      nodes <- read.delim('results.txt',sep='\t' , header = TRUE , colClasses = c("character", "character","character","character","character","character"))$Motif_Nodes[selectedRows]
      edges <- read.delim('results.txt',sep='\t' , header = TRUE , colClasses = c("character", "character","character","character","character","character"))$Motif_edges[selectedRows]
      
      nodes <- strsplit(nodes,",",fixed=TRUE)
      edges <- strsplit(edges, "[)(]")
      edges <- lapply(edges, function(z){ z[!is.na(z) & z != ""]})
      
      for(i in 1:length(edges)){
        for(j in 1:length(edges[[i]])){
          edges[[i]][j] <- strsplit (substring(edges[[i]][j] , 1 , nchar(edges[[i]][j])-2) , "," )
        }
      }
      
      nodesDF <- data.frame()
      edgesDF <- data.frame()
      legenda <- data.frame()
      nodeColors <- c()
      values <- c()
      arcAltitude <- c()
      
      for(i in 1:length(nodes)){
        
        name <- unlist(nodes[[i]])
        
        latitude <- list()
        longitude <- list()
        
        for(z in 1:length(name)){
           nome <- name[z]
          capital <- world.cities[world.cities$capital == 1 & world.cities$country.etc == nome,c("lat","long")]
          latitude <- c(latitude , capital[1,]$lat)
          longitude <- c(longitude , capital[1,]$long)
        }
        
        for(z in 1:length(unlist(name))){
          nodesDF <- rbind(nodesDF , c(unlist(name)[z] , unlist(latitude)[z] , unlist (longitude)[z] ))
        }
        
        colnames(nodesDF) <- c("name","lat","long")  
        
        for(j in 1:length(edges[[i]])){
          fromNode <- as.integer(edges[[i]][[j]][1])+(length(nodes[[i]]) * (i-1))
          toNode <- as.integer(edges[[i]][[j]][2])+(length(nodes[[i]]) * (i-1))
          data <- c(fromNode,toNode)
          edgesDF <- rbind(edgesDF , data)
        }
        
        colnames(edgesDF) <- c("from","to")
        
        color <- paste0("#", paste(sample(c(0:9, letters[1:6]), 6, replace = TRUE), collapse = ""))
        
        for(w in 1:length(nodes[[i]])){
          nodeColors <- c(nodeColors,color)
          values <- c(values,as.integer((length(nodes[[i]]) - i + 2)*20) )
        }
        
        globeArcs <- data.frame()
        
        for(k in 1:nrow(edgesDF)){
          xFrom <- as.double(nodesDF[as.integer(edgesDF[k,"from"])+1,"long"])
          yFrom <- as.double(nodesDF[as.integer(edgesDF[k,"from"])+1,"lat"])
          xTo <- as.double(nodesDF[as.integer(edgesDF[k,"to"])+1,"long"])
          yTo <- as.double(nodesDF[as.integer(edgesDF[k,"to"])+1,"lat"])
          data <- c(yFrom,xFrom,yTo,xTo)
          globeArcs <- rbind(globeArcs , data)
        }
        
        colnames(globeArcs) <- c("yFrom","xFrom","yTo","xTo")
        
        for(l in 1:length(edges[[i]])){
          legenda <- rbind(legenda , c(as.integer(sort(selectedRows[i])),color))
          arcAltitude <- c(arcAltitude,(as.integer((length(edges[[i]]) - i + 2)*10))/100 )
        }
        
        colnames(legenda) <- c("motif","color")
      }

      isoCodes <- c()
      
      for(i in 1:nrow(nodesDF)){
        isoCodes <- c(isoCodes, countriesData[countriesData$name == nodesDF$name[i],]$isoCode)
      }
      
      cbind(isoCodes , nodesDF)
      
      output$motifGlobe <- renderGlobe({
        create_globe()%>%
          globe_arcs(
            coords(
              start_lat = yFrom,
              start_lon = xFrom,
              end_lat = yTo,
              end_lon = xTo,
              color = color,
              label = motif,
              altitude = arcAltitude,
              stroke = 0.7,
              dash_length = .2,
              dash_gap = .08,
              dash_animate_time = 6000
            ),
            data = cbind(globeArcs,legenda,arcAltitude)
          )%>%
          globe_choropleth(
            coords(country = name,long, label = name,cap_color=nodeColors , altitude=0.01),
            data = cbind(nodesDF,nodeColors),
            match = "auto"
          )
      })
      
      
      generateSquare <- function(color, text) {
        paste0(
          '<div style="display: flex; flex-direction: row; align-items: center; margin-right: 10px;">',
          '<div style="width: 40px; height: 40px; margin-left: 20%; background-color:', color, '; border-style: solid; border-width: 2px; border-color: black;  "></div>',
          '<div style="margin-left: 10px; font-size: 15px; font-weight: bold;">', paste0("Motivo n°",text,collapse=" "), '</div><br><br><br>',
          '</div>'
        )
      }
      
      output$legenda <- renderUI({
        HTML('<h3>Legenda</h3>')
        legendaRows <- apply(unique(legenda), 1, function(row) {
          generateSquare(row[2], row[1])
        })
        HTML(paste(legendaRows, collapse = ""))
      })
      
    }
    
    
  })
  
  
  # cerco i motivi nella query
  
  observeEvent(input$searchMotif, {
    
    netFile <- NULL
    
    if(labelType() == "Nome aeroporto")
      netFile <- "airportsNetwork.txt" 
    
    else if(labelType() == "Nazione")
      netFile <- "airportsNetworkCountry.txt" 
    
    if(!file.exists(netFile))
      showNotification(paste("File della rete non trovato"), duration = 4  , type="error")
    
    else if(!file.exists("query.txt"))
      showNotification(paste("File di query non trovato"), duration = 4  , type="error")
    
    else{
      
      # chiamo "MultiMotif" sui file network (in base alla label scelta) e query creati
      
      system(paste("java -jar MultiMotif\\MultiMotif.jar -n",netFile,"-q query.txt"))
      
      output$motifSearchResults <- renderDT({
        read.delim('results.txt',sep='\t' , header = TRUE , colClasses = c("character", "character","character","character","character","character"))
      })
    }
    
  })
  
}

shinyApp(ui = ui, server = server)