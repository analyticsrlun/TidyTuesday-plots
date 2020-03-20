library(leaflet)
library(leaflet.extras)
library(magrittr)
#library(plyr)

# Es necesario descargar las libreias usadas
# install.packages('leaflet')
# install.packages('leaflet.extras')
# install.packages("magrittr")
# install.packages("plyr")

# Obtener datos de csv
data <- read.csv(file="minas.csv", header=TRUE, sep=",")

# Primer mapa con eventos
m <-leaflet() %>% addTiles() %>%
  addMarkers(data$longitudcabecera, data$latitudcabecera, 
             clusterOptions = markerClusterOptions(), 
             popup = paste("<b>Ubicacion", data$municipio,"</b>", "<br/>", 
                           "Genero: ", data$genero, "<br/>",
                           "Estado: ", data$estado,"<br/>",
                           "Condicion: ", data$condicion, "<br/>",
                           "Rango de edad: ", data$rangoedad, "<br/>",
                           "El mes", data$mes, " del año", data$ano, "<br/>"
             )
  )
#Mostrar y centrarlo
m %>% fitBounds(-78.44238281249999,11.480024648555816, -67.2802734375,-4.565473550710278)

# Separacion de los datos segun tipo de eventos
dataMuse <- data[data$tipoevento == "Accidente por MUSE", ]
dataMap <- data[data$tipoevento == "Accidente por MAP", ]

# Frecuencias de eventos en departamentos
# sMap <- count(dataMap, 'departamento')
# sMuse <- count(dataMuse, 'departamento')

#Mapa de calor en base a tipo de evento Muse
mapaMuse <- leaflet() %>% addProviderTiles(providers$CartoDB.DarkMatter) %>%
  addHeatmap(lng = dataMuse$longitudcabecera, lat = dataMuse$latitudcabecera,
             blur = 20, max = 0.05, radius = 15)

#Mapa de calor en base a tipo de evento Map
mapaMap <- leaflet() %>% addProviderTiles(providers$CartoDB.DarkMatter) %>%
  addHeatmap(lng = dataMap$longitudcabecera, lat = dataMap$latitudcabecera,
             blur = 20, max = 0.05, radius = 15)