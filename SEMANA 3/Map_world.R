### Cargar Paquetes ###
library(ggplot2)
library(tidyverse)
library(tidytuesdayR)
library(mapdata)
library(ggrepel)
library(ggthemes)


### Cargar Datos ###
data<-tt_load('2020-09-01')
land_use<-data$land_use_vs_yield_change_in_cereal_production

land_use2<-land_use%>%filter(Year=="2014")%>%select(1,4,6)%>%na.omit()%>%
  filter(Entity!='World'& Entity!='North America')

### Renombrar paises ###
land_use2$Entity <- recode(land_use2$Entity , "United Kingdom" = "UK")
land_use2$Entity <- recode(land_use2$Entity , "United States" = "USA")
land_use2$Entity <- recode(land_use2$Entity , "Trinidad and Tobago" = "Trinidad")
land_use2$Entity <- recode(land_use2$Entity , "Cote d'Ivoire" = "Ivory Coast")
land_use2$Entity <- recode(land_use2$Entity , "Democratic Republic of Congo" = "Democratic Republic of the Congo")
land_use2$Entity <- recode(land_use2$Entity , "Saint Vincent and the Grenadines" = "Saint Vincent")
land_use2$Entity <- recode(land_use2$Entity , "Timor" = "Timor-Leste")
land_use2$Entity <- recode(land_use2$Entity , "Congo" = "Republic of Congo")

### Cargar Mapa Mundo ###
map_world<-map_data("world")%>%filter(region!="Antarctica")
map_world_land_use2<-land_use2%>%left_join(map_world,by = c('Entity'='region'))

### Gráfica ###
world_land_use<-ggplot()+geom_map(data=map_world,map=map_world,aes(x=long,y=lat,group=group,map_id=region),fill="#0C154A")+
  geom_map(data=map_world_land_use2,map=map_world,aes(x=long,y=lat,fill=map_world_land_use2$`Cereal yield index`,map_id=Entity))+
  theme_map()+scale_fill_continuous(name="")+labs(caption = "Our World In Data | @analyticsrl")+ggtitle("Índice de rendimiento de cultivos cereales para el año 2014")+
  theme(plot.title = element_text(family = "serif",size = 16),plot.caption = element_text(family = "sans",face="italic",size=8))

world_land_use
