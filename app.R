library(readr)
library(devtools)
library(factoextra)
library(dplyr)
library(tidyr)
library(shiny)
library(ggplot2)
library(lubridate)
library(sf)
library(leaflet)
library(shinydashboard)

# Load and preprocess data (assuming CHVW and MAKt are already loaded)
# CHVW_orginal <- CHVW
# setwd("~/UAB/Projecte/ProjecteVD/app")
CHVW <- read_csv("VisualitzacioDades/Accenture/CHVW.csv")
MAKT <- read_csv("VisualitzacioDades/Accenture/MAKT.csv")


MAKT <- MAKT %>% rename(NumMaterial = MATNR, NomMaterial = MAKTK)

CHVW <- CHVW %>%
  rename(NumeroClient = KUNNR,
         Planta = WERKS,
         NumMaterial = MATNR,
         Lot = CHARG,
         Ordre = AUFNR,
         Item = AUFPS,
         DocumentCompra = EBELN,
         ItemDocumentCompra = EBELP,
         OrdenVenta = KDAUF,
         ItemOrdenVenta = KDPOS,
         DocumentDeMaterial = MBLNR,
         AnyDocumentMaterial = MJAHR,
         ItemDocumentMaterial = ZEILE,
         DataTransaccio = BUDAT,
         TipusMoviment = BWART,
         TipusOrdre = KZBEW,
         QuantitatProducte = MENGE,
         QuantitatUnitPerProducte = MEINS,
         NumeroLotProveidor = LICHA,
         NumDocumentVentes = VBELN,
         NumItem = POSNR)
CHVW$Pais <- substr(CHVW$Planta, 1, nchar(CHVW$Planta) - 2)

# CHVW <- CHVW[sample(nrow(CHVW), 100), ]
# CHVW <- CHVW_orginal
CHVW_any <- CHVW
CHVW_any$DataTransaccioAny <- format(CHVW$DataTransaccio, "%Y")

# Dataset modifications for panels 2 and 3
dataset_mod <- CHVW %>%
  mutate(Any = year(DataTransaccio), Mes = month(DataTransaccio, label = TRUE, abbr = TRUE)) %>%
  group_by(Planta, Any, Mes, TipusOrdre) %>%
  summarise(Transaccions = n())

dataset_mod1 <- CHVW %>%
  mutate(TipusMoviment = factor(TipusMoviment)) %>%
  mutate(Any = year(DataTransaccio), Mes = month(DataTransaccio, label = TRUE, abbr = TRUE)) %>%
  group_by(Planta, Any, Mes, TipusMoviment) %>%
  summarise(Moviments = n())

# Dataset modifications for panel 4
merged_data <- CHVW_any %>%
  inner_join(MAKT, by = "NumMaterial", relationship = "many-to-many")

agrup <- merged_data %>%
  group_by(Pais, DataTransaccioAny, NomMaterial) %>%
  summarise(count = n(), .groups = 'drop') %>%
  mutate(count = as.numeric(count))

mapa <- data.frame(
  Pais = c("CH", "ES", "FR", "GE", "IR", "IT"),
  long = c(8.2275, -3.7492, 2.2137, 10.4515, -7.6921, 12.5674),
  lat = c(46.8182, 40.4637, 46.6034, 51.1657, 53.1424, 41.8719)
)

final_data4 <- agrup %>%
  inner_join(mapa, by = "Pais")

final_data4$long <- as.numeric(final_data4$long)
final_data4$lat <- as.numeric(final_data4$lat)
dsf4 <- st_as_sf(final_data4, coords = c("long", "lat"))

final_data5 <- CHVW_any %>%
  inner_join(mapa, by = "Pais")

final_data5$long <- as.numeric(final_data5$long)
final_data5$lat <- as.numeric(final_data5$lat)
dsf5 <- st_as_sf(final_data5, coords = c("long", "lat"))

# UI definition
ui <- dashboardPage(
  dashboardHeader(title = "Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Transaccions per Planta", tabName = "panel1", icon = icon("line-chart")),
      menuItem("Transaccions per Mesos i Planta", tabName = "panel2", icon = icon("calendar")),
      menuItem("Nombre de Moviments per Mesos per Planta", tabName = "panel3", icon = icon("exchange")),
      menuItem("Mapa per Tipus de Material", tabName = "panel4", icon = icon("map")),
      menuItem("Mapa Localitzador", tabName = "panel5", icon = icon("map-marker"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "home",
              fluidPage(
                fluidRow(
                  column(width = 6,
                         box(title = "TRANSACCIONS PER PLANTA", width = NULL, status = "primary", solidHeader = TRUE,
                             plotOutput("previewPlot1", height = "250px"))
                  ),
                  column(width = 6,
                         box(title = "TRANSACCIONS PER MESOS I PLANTA", width = NULL, status = "primary", solidHeader = TRUE,
                             plotOutput("previewPlot2", height = "250px"))
                  )
                ),
                fluidRow(
                  column(width = 6,
                         box(title = "NOMBRE DE MOVIMENTS PER MESOS PER PLANTA", width = NULL, status = "primary", solidHeader = TRUE,
                             plotOutput("previewPlot3", height = "250px"))
                  ),
                  column(width = 6,
                         box(title = "MAPA PER TIPUS DE MATERIAL", width = NULL, status = "primary", solidHeader = TRUE,
                             leafletOutput("previewMap4", height = "250px"))
                  )
                ),
                fluidRow(
                  column(width = 12,
                         box(title = "MAPA LOCALITZADOR", width = NULL, status = "primary", solidHeader = TRUE,
                             leafletOutput("previewMap5", height = "250px"))
                  )
                )
              )),
      tabItem(tabName = "panel1",
              fluidPage(
                titlePanel("TRANSACCIONS PER PLANTA"),
                sidebarLayout(
                  sidebarPanel(
                    selectInput("planta1", "Selecciona una Planta:", choices = unique(CHVW$Planta))
                  ),
                  mainPanel(
                    plotOutput("transPlot1", width = "100%", height = "500px")
                  )
                )
              )),
      tabItem(tabName = "panel2",
              fluidPage(
                titlePanel("TRANSACCIONS PER PLANTA I PER ANYS SELECCIONATS"),
                sidebarLayout(
                  sidebarPanel(
                    selectInput("planta2", "Selecciona una Planta:", choices = unique(dataset_mod$Planta)),
                    selectInput("anys2", "Selecciona Anys:", choices = unique(dataset_mod$Any), selected = "2023", multiple = TRUE)
                  ),
                  mainPanel(
                    plotOutput("transPlot2", width = "100%", height = "500px")
                  )
                )
              )),
      tabItem(tabName = "panel3",
              fluidPage(
                titlePanel("MOVIMENTS PER PLANTA"),
                sidebarLayout(
                  sidebarPanel(
                    selectInput("planta3", "Selecciona una Planta:", choices = unique(dataset_mod1$Planta)),
                    selectInput("anys3", "Selecciona Anys:", choices = unique(dataset_mod1$Any), selected = "2023", multiple = TRUE)
                  ),
                  mainPanel(
                    plotOutput("transPlot3", width = "100%", height = "500px")
                  )
                )
              )),
      tabItem(tabName = "panel4",
              fluidPage(
                selectizeInput("any4", "Selecciona un any", choices = unique(final_data4$DataTransaccioAny), selected = "2023", multiple = FALSE),
                selectizeInput("material4", "Selecciona material", choices = unique(final_data4$NomMaterial), selected = "Aluminum", multiple = FALSE),
                leafletOutput("map4", width = "100%", height = "500px")
              )),
      
      tabItem(tabName = "panel5",
              fluidPage( selectizeInput( inputId = "lot5",
                                         label = "Selecciona un o més lots",
                                         choices = unique(CHVW$Lot),
                                         selected = CHVW$Lot[1],
                                         multiple = TRUE
              ),

        leafletOutput(outputId = "map5")
        ))
    )
  )
)

# Server logic
server <- function(input, output) {
  output$previewPlot1 <- renderPlot({
    dades_filtrades <- CHVW %>% filter(Planta == unique(CHVW$Planta)[1])
    ggplot(dades_filtrades, aes(x = factor(TipusOrdre), fill = factor(TipusOrdre))) +
      geom_bar(position = "dodge", color = "black", size = 0.3) +
      labs(x = "TIPUS DE TRANSACCIÓ", y = "Nombre de Transaccions", fill = "TIPUS DE TRANSACCIÓ") +
      ggtitle("Transaccions per Tipus de Transacció (B,F,L)") +
      theme_minimal(base_size = 10) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
            plot.title = element_text(hjust = 0.5, face = "bold"),
            legend.position = "bottom") +
      scale_fill_manual(values = c('lightgreen', 'lightblue', 'pink'))
  })
  
  output$previewPlot2 <- renderPlot({
    dades_filtrades <- dataset_mod %>%
      filter(Planta == unique(dataset_mod$Planta)[1], Any %in% unique(dataset_mod$Any))
    colors <- c('lightgreen', 'lightblue', 'pink')
    ggplot(dades_filtrades, aes(x = Mes, y = Transaccions, color = TipusOrdre, group = interaction(TipusOrdre, Any))) +
      geom_point(size = 2) +
      geom_line(size = 0.5) +
      facet_wrap(~ Any, scales = "free_y") +
      scale_color_manual(values = colors) + 
      labs(x = "Mes", y = "Nombre de Transaccions", color = "Tipus de Transacció") +
      ggtitle("Transaccions per Mes") +
      theme_minimal(base_size = 10) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  })
  
  output$previewPlot3 <- renderPlot({
    dades_filtrades <- dataset_mod1 %>% filter(Planta == unique(dataset_mod1$Planta)[1], Any %in% unique(dataset_mod1$Any))
    colors <- c("101"="#F8766D", "102"="#7CAE00", "261"="#00BFC3","262"= "#C77CFF","301"= "#F0E442", "302"="#377EB8",
                "501"="#FFDE00", "502"="#4696BF", "601"="#D55E00","602"= "#CC79A7", "701"="#56B4E9", "801"="#E69F00",
                "802"="#009E73")
    ggplot(dades_filtrades, aes(x = Mes, y = Moviments, color = TipusMoviment, group = interaction(TipusMoviment, Any))) +
      geom_point(size = 2) +
      geom_line(size = 0.5) +
      facet_wrap(~ Any, scales = "free_y") +
      scale_color_manual(values = colors) + 
      labs(x = "Mes", y = "Nombre de Moviments", color = "Tipus de Moviments") +
      ggtitle("Moviments per Mes") +
      theme_minimal(base_size = 10) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  })
  
  output$previewMap4 <- renderLeaflet({
    filtered_data <- dsf4 %>% 
      filter(DataTransaccioAny %in% input$DataTransaccioAny, NomMaterial %in% input$NomMaterial)
    
    lng <- as.numeric(st_coordinates(filtered_data)[, 1])
    lat <- as.numeric(st_coordinates(filtered_data)[, 2])
    color_palette <- colorFactor(topo.colors(length(unique(dsf4$NomMaterial))), dsf4$NomMaterial)
    leaflet(filtered_data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = lng, 
        lat = lat,
        color = ~color_palette(NomMaterial),
        radius = ~count / 500,
        popup = ~paste("País:", Pais, "<br>",
                       "Any:", DataTransaccioAny, "<br>",
                       "Tipus:", NomMaterial, "<br>",
                       "Count:", count)
      )
  })
  
  output$previewMap5 <- renderLeaflet({
    filtered_data <- dsf5 %>% filter(Lot %in% input$lot5)
    lng <- as.numeric(st_coordinates(filtered_data)[, 1]) 
    lat <- as.numeric(st_coordinates(filtered_data)[, 2]) 
    leaflet(filtered_data) %>%
      addTiles() %>%
      addMarkers(lng = lng, lat = lat,
                 popup = ~paste("Planta:", Planta, "<br>",
                                "Any:", DataTransaccio, "<br>",
                                "Lot:", Lot, "<br>"
                 )
      )
  })
  
  # Render detailed plots and maps for each panel
  output$transPlot1 <- renderPlot({
    dades_filtrades <- CHVW %>% filter(Planta == input$planta1)
    ggplot(dades_filtrades, aes(x = factor(TipusOrdre), fill = factor(TipusOrdre))) +
      geom_bar(position = "dodge", color = "black", size = 0.3) +
      labs(x = "TIPUS DE TRANSACCIÓ", y = "Nombre de Transaccions", fill = "TIPUS DE TRANSACCIÓ") +
      ggtitle(paste("Transaccions per Tipus de Transacció (B,F,L) a la Planta", input$planta1)) +
      theme_minimal(base_size = 15) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
            plot.title = element_text(hjust = 0.5, face = "bold"),
            legend.position = "bottom") +
      scale_fill_manual(values = c('lightgreen', 'lightblue', 'pink'))
  })
  
  output$transPlot2 <- renderPlot({
    dades_filtrades <- dataset_mod %>% filter(Planta == input$planta2 & Any %in% input$anys2)
    if (nrow(dades_filtrades) == 0) {
      print("No hi ha dades disponibles per a la selecció especificada.")
    } else {
      colors <- c('lightgreen', 'lightblue', 'pink')
      ggplot(dades_filtrades, aes(x = Mes, y = Transaccions, color = TipusOrdre, group = interaction(TipusOrdre, Any))) +
        geom_point(size = 3) +
        geom_line(size = 1) +
        facet_wrap(~ Any, scales = "free_y") +
        scale_color_manual(values = colors) + 
        labs(x = "Mes", y = "Nombre de Transaccions", color = "Tipus de Transacció") +
        ggtitle(paste("Transaccions per Mes a la Planta", input$planta2)) +
        theme_minimal(base_size = 15) +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    }
  })
  
  output$transPlot3 <- renderPlot({
    dades_filtrades <- dataset_mod1 %>% filter(Planta == input$planta3 & Any %in% input$anys3)
    colors <- c("101"="#F8766D", "102"="#7CAE00", "261"="#00BFC3","262"= "#C77CFF","301"= "#F0E442", "302"="#377EB8",
                "501"="#FFDE00", "502"="#4696BF", "601"="#D55E00","602"= "#CC79A7", "701"="#56B4E9", "801"="#E69F00",
                "802"="#009E73")
    ggplot(dades_filtrades, aes(x = Mes, y = Moviments, color = TipusMoviment, group = interaction(TipusMoviment, Any))) +
      geom_point(size = 3) +
      geom_line(size = 1) +
      facet_wrap(~ Any, scales = "free_y") +
      scale_color_manual(values = colors) + 
      labs(x = "Mes", y = "Nombre de Moviments", color = "Tipus de Moviments") +
      ggtitle(paste("Moviments per Mes a la Planta", input$planta3)) +
      theme_minimal(base_size = 15) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  })
  
  output$map4 <- renderLeaflet({
    filtered_data <- dsf4 %>% 
      filter(DataTransaccioAny %in% input$any4, NomMaterial %in% input$material4)
    lng <- as.numeric(st_coordinates(filtered_data)[, 1])
    lat <- as.numeric(st_coordinates(filtered_data)[, 2])
    color_palette <- colorFactor(topo.colors(length(unique(dsf4$NomMaterial))), dsf4$NomMaterial)
    leaflet(filtered_data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = lng, 
        lat = lat,
        color = ~color_palette(NomMaterial),
        radius = ~count / 500,
        popup = ~paste("País:", Pais, "<br>",
                       "Any:", DataTransaccioAny, "<br>",
                       "Tipus:", NomMaterial, "<br>",
                       "Count:", count)
      )
  })
  
  output$map5 <- renderLeaflet({
    filtered_data <- dsf5 %>% filter(Lot %in% input$lot5)
    lng <- as.numeric(st_coordinates(filtered_data)[, 1]) 
    lat <- as.numeric(st_coordinates(filtered_data)[, 2]) 
    leaflet(filtered_data) %>%
      addTiles() %>%
      addMarkers(lng = lng, lat = lat,
                 popup = ~paste("Planta:", Planta, "<br>",
                                "Any:", DataTransaccio, "<br>",
                                "Lot:", Lot, "<br>"
                 )
      )
  })
}

shinyApp(ui, server)
