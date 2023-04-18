
library(leaflet)
library(dplyr)

##### respaldo
graficar_mapa <- function(dataframe, variable, color = NULL, bins = NULL) {
  #c("viridis", "magma", "inferno", "plasma", "RdYlBu", "YlOrRd", "Blues", "Greens", "Oranges", "Purples", "Reds", "BuGn", "BuPu", "GnBu", "OrRd", "PuBu", "PuBuGn", "PuRd", "RdPu", "YlGn", "YlGnBu", "YlOrBr", "Spectral")
  
  # Definir los datos correspondientes a la variable seleccionada
  datos_variable <- switch(variable,
                           "A/B" = dataframe$AB_perc,
                           "C+" = dataframe$CMAS_perc,
                           "C" = dataframe$C_perc,
                           "C-" = dataframe$CMENOS_perc,
                           "D+" = dataframe$DMAS_perc,
                           "D" = dataframe$D_perc,
                           "E" = dataframe$E_perc
                           )
  
  # Selección de paleta de color
  if(is.null(color)){color <- "magma"}
  
  # Definir la escala de colores según la variable seleccionada
  if(length(bins) >= 2){
    # Caso discreto
    pal <- colorBin(color, domain = datos_variable, bins = bins,
                    na.color = "lightgrey")
  }else{
    # Caso continuo
    pal <- colorNumeric(color, datos_variable)
  }
  
  # Definir etiquetas
  labels <- sprintf("<strong>%s</strong><br/> %g",
                    dataframe$AGEB, round(datos_variable,2)) %>% 
    lapply(htmltools::HTML)
  
  # Graficar los datos con Leaflet
  m <- leaflet(dataframe) %>%
    setView(-111, 29.1, 12) %>%
    addTiles() %>% 
    addPolygons(
      fillColor = ~pal(datos_variable),
      weight = 1,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      highlightOptions = highlightOptions(
        weight = 5,
        color = "#666",
        dashArray = "",
        fillOpacity = 1,
        bringToFront = TRUE),
      label = labels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto")) %>% 
    addLegend(pal = pal, values = ~datos_variable, opacity = 0.7, title = NULL,
              position = "bottomright")
  
  return(m)
}

