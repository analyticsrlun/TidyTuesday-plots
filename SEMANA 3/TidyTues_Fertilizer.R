fertilizer <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/cereal_crop_yield_vs_fertilizer_application.csv')
df<- fertilizer
fertilizer<- df
library(dplyr)
library(ggplot2)
library(mapdata)
library(maps)
library(ggrepel)
library(ggthemes)
#### preparacion de los datos

lista<- c("Colombia","Argentina","Chile","Bolivia","Brazil",
          "Uruguay", "Venezuela", "Paraguay","Peru", "Ecuador",	
          "Guyana","Suriname")
##Eliminar filas vacias
fertilizer<- fertilizer[complete.cases(fertilizer),]
## filtrar los paies suramericanos
fertilizer<- dplyr::filter(fertilizer,Entity %in% lista)
## tomamos los datos para 2017
fertilizer<- dplyr::filter(fertilizer,Year==2017)
### Cambiamos el nombre de la variable de nitrogeno
fertilizer <- dplyr::rename(fertilizer, Nitrogen= `Nitrogen fertilizer use (kilograms per hectare)` )
#Obtenemos las coordenadas de todos los paises del mundo y filtramos Suramerica
mapa_mundo <- map_data("world")
america<-  dplyr::filter(mapa_mundo,region %in% lista)

## Unimos los datos de la base de fertilizantes a las coordenadas de cada pais
i=1
for (a in 1:10) {
  while (fertilizer[a,1]== america[i,5]) {
    america[i,7]<- fertilizer[a,5]
    i= i+1 }
  
}
#### Creacion del mapa
mapa_america<- america %>%
  ggplot() +
  geom_polygon(aes( x= long, y = lat,group= group),
               fill = "grey80",
               color = "white") +
  theme_minimal()+
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_rect(colour= "grey30", size= 1)) +
  labs(title= "Uso de fertilizante en América del Sur en 2017",
       subtitle = "Kg de Nitrógeno por hectareas",
       caption = "Fuente: Our World in Data
  Hecho por: @analyticsrl")+
  theme(plot.title=element_text(size=14, 
                                vjust=1.25, 
                                hjust = 0.5, face="italic", color="black"
  )) +
  theme(plot.subtitle=element_text(size=10, 
                                   vjust=1.25, 
                                   hjust = 0.5, face="italic", color="black"))  +
  theme(plot.caption=element_text(size=8,
                                  hjust=0.5, face="italic", color="black"))+
  coord_fixed (xlim= c(-100,-25),
               ylim= c(-60,20),
               ratio = 1.2)+
  geom_polygon(data= america,aes(long,lat,group= group,fill= Nitrogen), 
               color = "orange",
               alpha = 0.7,
                size = 0.2)

mapa_america


