library(shiny)
library(leaflet)

setwd("~/github_dayci/muestras_ageb/mapa_por_nse")
source("../01 lectura de datos.R")

# Definir una tabla de conversión
tabla_conversion <- c("A/B" = 1, "C+" = 2, "C" = 3, "C-" = 4, "D+" = 5, "D" = 6, "E" = 7, "N/D" = NA)

# Crear la paleta de colores
colores <- colorNumeric(palette = "RdYlBu", domain = c(1, 7), reverse = TRUE, na.color = "gray")


# Crear la aplicación Shiny
ui <- fluidPage(
  
  # Selector de variable
  selectInput("var_selector", "Seleccione la variable a graficar:",
              choices = c("A/B", "C+", "C", "C-", "D+", "D", "E"),
              selected = "C"),
  
  # Mapa
  leafletOutput("mapa")
)

server <- function(input, output) {
  
  # Crear una variable reactiva para almacenar la selección del usuario
  var_elegida <- reactive({
    tabla_conversion[input$var_selector]
  })
  
  # Crear el mapa y establecer la vista inicial
  output$mapa <- renderLeaflet({
    leaflet() %>% setView(-111, 29.1, 12) %>% addTiles()
  })
  
  # Actualizar el mapa cada vez que cambie la variable seleccionada
  observe({
    pal <- colorNumeric(palette = "Reds", domain = c(1, 7), reverse = TRUE, na.color = "gray")
    pal$setDomain(range(tabla_conversion))
    
    output$mapa <- renderLeaflet({
      leaflet() %>% 
        setView(-111, 29.1, 12) %>% 
        addTiles() %>%
        addPolygons(data = ageb.shp,
                    fillColor = ~pal(var_elegida()),
                    weight = 1, opacity = 1, color = "white",
                    dashArray = "3", fillOpacity = 0.7,
                    label = ~paste("AGEB:", AGEB, "\n", "NIVEL:", NIVEL),
                    highlight = highlightOptions(
                      weight = 3, color = "#666", dashArray = "", fillOpacity = 0.7,
                      bringToFront = TRUE))
    })
  })
}

shinyApp(ui = ui, server = server)
