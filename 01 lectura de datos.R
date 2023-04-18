
setwd("~")
getwd()
# Leer archivo de datos
ruta <- "~/data/shp_INEGI/26_sonora/conjunto_de_datos/"
ageb.shp <- readOGR(dsn = ruta, 
                    layer = "26a", verbose = FALSE);

# renonmbrar  CVE_AGEB
ageb.shp@data <- ageb.shp@data %>% rename(AGEB = CVE_AGEB)

# Transform to WGS84
ageb.shp <- spTransform(ageb.shp, CRS("+init=epsg:4326"))

# Print the new spatial reference system
proj4string(ageb.shp)

# Filtro para Hermosillo
ageb.shp <- subset(ageb.shp, CVE_MUN == "030")

# Vista de datos
ageb.shp@data %>% head()
ageb.shp@data %>% dim()

# Cargar el paquete foreign
library(foreign)

# Especificar la ruta completa del archivo .sav
ruta <- "~/data/amai_nse/NSE_por_AGEB_AMAI.sav"

# Leer el archivo .sav
nse.ageb <- read.spss(ruta) %>%  as.data.frame() 
nse.ageb %>% dim()

# Cambiar "N/D" por NA
#nse.ageb$NIVEL <- ifelse(nse.ageb$NIVEL == "N/D", NA, nse.ageb$NIVEL)
nse.ageb$NIVEL[nse.ageb$NIVEL == "9999"] <- NA

# Aplicar la función trimws() para eliminar los espacios en blanco en los extremos
nse.ageb$NOM_MUN <- trimws(nse.ageb$NOM_MUN)
nse.ageb$AGEB <- trimws(nse.ageb$AGEB)

# Reemplazar los espacios en blanco adicionales por un solo espacio
nse.ageb$NOM_MUN <- gsub("\\s+", " ", nse.ageb$NOM_MUN)
nse.ageb$AGEB <- gsub("\\s+", " ", nse.ageb$AGEB)

# Filtrar las filas que contienen "Hermosillo" en la columna NOM_MUN
nse.ageb <- nse.ageb %>% filter(ENT == 26)
nse.ageb <- nse.ageb %>% filter(NOM_MUN == "Hermosillo")
#nse.ageb <- nse.ageb %>% filter(grepl("Hermosillo", NOM_MUN))


# Hacer el merge de los datos
ageb.shp <- merge(ageb.shp, nse.ageb, by = "AGEB")
ageb.shp@data %>% head()

ageb.shp@data$NIVEL %>% table()

############################################################################
### CONVIERTE A NUMÉRICO
############################################################################
ageb.shp$AB <- as.numeric(as.character(ageb.shp$AB))
ageb.shp$CMAS <- as.numeric(as.character(ageb.shp$CMAS))
ageb.shp$C <- as.numeric(as.character(ageb.shp$C))
ageb.shp$CMENOS <- as.numeric(as.character(ageb.shp$CMENOS))
ageb.shp$DMAS <- as.numeric(as.character(ageb.shp$DMAS))
ageb.shp$D <- as.numeric(as.character(ageb.shp$D))
ageb.shp$E <- as.numeric(as.character(ageb.shp$E))
ageb.shp$VIVIENDAS <- as.numeric(as.character(ageb.shp$VIVIENDAS))


############################################################################
### CALCULA PORCENTAJE POR NSE RESPECTO A VIVIENDAS
############################################################################

ageb.shp@data <- ageb.shp@data %>%
  mutate(AB_perc = ifelse(is.na(AB) | is.na(VIVIENDAS), NA, AB/VIVIENDAS * 100),
         CMAS_perc = ifelse(is.na(CMAS) | is.na(VIVIENDAS), NA, CMAS/VIVIENDAS * 100),
         C_perc = ifelse(is.na(C) | is.na(VIVIENDAS), NA, C/VIVIENDAS * 100),
         CMENOS_perc = ifelse(is.na(CMENOS) | is.na(VIVIENDAS), NA, CMENOS/VIVIENDAS * 100),
         DMAS_perc = ifelse(is.na(DMAS) | is.na(VIVIENDAS), NA, DMAS/VIVIENDAS * 100),
         D_perc = ifelse(is.na(D) | is.na(VIVIENDAS), NA, D/VIVIENDAS * 100),
         E_perc = ifelse(is.na(E) | is.na(VIVIENDAS), NA, E/VIVIENDAS * 100))

