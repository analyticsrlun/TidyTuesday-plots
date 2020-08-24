library(tidytuesdayR)
library(tidyverse)
library(magrittr)
library(tidyverse)
install.packages("ggridges")
library(ggridges)
install.packages("cowplot")
library(cowplot)
library(scales)
install.packages("magick")
library(magick)
install.packages("extrafont")
library(extrafont)
install.packages("ggpubr")
library(ggpubr)
install.packages("patchwork")
library(patchwork)
install.packages("rsvg")
library(rsvg)
library(plotly)


# Leemos los datos
tuesdata <- tidytuesdayR::tt_load('2020-08-18')
plants <- tuesdata$plants
threats <- tuesdata$threats
actions <- tuesdata$actions

#write.csv(plants, "plants.csv")
#write.csv(threats, "threats.csv")
#write.csv(actions, "actions.csv")


#Podemos usar la base de datos un poco mÃ¡s limpia dependiendo de las variables que vayamos a usar
#write.csv(plants, "plants.csv")
#write.csv(threats, "threats.csv")
#write.csv(actions, "actions.csv")

# Un vistazo a la estructura de los datos
str(plants)
str(threats)
str(actions)

# Numero de acciones tomadas por continente
actions %>% group_by(continent) %>% summarise(number=sum(action_taken))

#Tipos de plantas extintas por continente
plants %>% group_by(continent, group) %>% summarise(conteo = n())

#Especies con mayor nÃºmero de amenazas
 prueba<- as.data.frame(cbind("Especie"=plants$binomial_name, "Num_de_amenazas"=apply(plants[, 6:23], 1, sum)))
(m <- prueba %>% arrange(desc(Num_de_amenazas)) %>% head())

 (amenazas <- threats %>% group_by(continent, threat_type) %>% filter(threatened>0) %>% summarise(n=n()))








####                                              ####
####              HERE STARTS THE PLOT            ####
####                                              ####



# ---- BARPLOT SHOWING MAIN THREATS PER CONTINENT --- #


df_amenazas <- threats %>% group_by(continent, threat_type) %>% filter(threatened>0) %>% summarise(n=n())
ame_Africa <- df_amenazas %>% filter(continent == "Africa") %>% arrange(desc(n)) 
ame_Africa <- ame_Africa[1:5, ]
ame_Asia <- df_amenazas %>% filter(continent == "Asia") %>% arrange(desc(n))
ame_Asia <- ame_Asia[1:5, ]
ame_Europe <- df_amenazas %>% filter(continent == "Europe") %>% arrange(desc(n))
ame_Europe <- ame_Europe[1:5, ]
ame_Nort_America<- df_amenazas %>% filter(continent == "North America") %>% arrange(desc(n))
ame_Nort_America <- ame_Nort_America[1:5, ]
ame_Oceania <- df_amenazas %>% filter(continent == "Oceania") %>% arrange(desc(n))
ame_Oceania <- ame_Oceania[1:5, ]
ame_South_America <- df_amenazas %>% filter(continent == "South America") %>% arrange(desc(n))
ame_South_America <- ame_South_America[1:5, ]

font_install("fontcm")
fonttable()

# Africa Plot
p1 <-   ame_Africa %>% 
  ggplot(aes(x = reorder(threat_type, -n), y = n, fill=threat_type)) +
  geom_col(position = "identity", 
                 #alpha = 0.7
            ) +
  ylab("Number of plants") +
  xlab("") +
  ggtitle("AFRICA") + 
  scale_fill_manual(values=c("dodgerblue4","dodgerblue3","skyblue2","cadetblue1","skyblue3")) + 
  theme_minimal() +
  theme(legend.position = "none", # quitar etiqueta
        #legend.text = element_text(size = 11), #tamaño de letra leyenda
        #legend.title = element_blank(), #quitar título de la leyenda
        panel.grid.major = element_blank(), # cuadricula de la gráfica
        axis.title = element_text(color = "gray28", size = 10, family = "Georgia", face="bold"), #diseño de los titulos (ejes)
        plot.title = element_text(size = 15, family="Georgia", hjust=rel(0.5), color="gray28"), #diseño del título
        #plot.subtitle = element_text( size = 12, hjust = 1),  subtitulo 
        axis.text.x = element_text(angle = 59, hjust=1, color="gray28"), #eje x
        axis.line = element_line(color = "gray28", linetype = "solid"),  #linea de los ejes
        axis.text = element_text(family="Georgia")
  )
p1



# Asia Plot
p2 <- ame_Asia %>% 
  ggplot(aes(x = reorder(threat_type, -n), y = n, fill=threat_type)) +
  geom_col(position = "identity", 
           #alpha = 0.7
           ) +
  ylab("") +
  xlab("") +
  ggtitle("ASIA") + 
  scale_fill_manual(values=c("dodgerblue4","skyblue3", "dodgerblue3","cadetblue1","skyblue2")) +
  theme_minimal() +
  theme(legend.position = "none", # quitar etiqueta
        #legend.text = element_text(size = 11), #tamaño de letra leyenda
        #legend.title = element_blank(), #quitar título de la leyenda
        panel.grid.major = element_blank(), # cuadricula de la gráfica
        axis.title = element_text(color = "gray28", size = 10, family = "Georgia", face="bold"), #diseño de los titulos (ejes)
        plot.title = element_text(size = 15, family="Georgia", hjust=rel(0.5), color="gray28"), #diseño del título
        #plot.subtitle = element_text( size = 12, hjust = 1),  subtitulo 
        axis.text.x = element_text(angle = 59, hjust=1, color="gray28"), #eje x
        axis.line = element_line(color = "gray28", linetype = "solid"),  #linea de los ejes
        axis.text = element_text(family="Georgia")
  )+
  ylim(c(0,70))
p2

# Europe Plot
p3 <- ame_Europe %>% 
  ggplot(aes(x = reorder(threat_type, -n), y = n, fill=threat_type)) +
  geom_col(position = "identity", 
           #alpha = 0.7
           ) +
  ylab("") +
  xlab("") +
  ggtitle("EUROPE") + 
  scale_fill_manual(values=c("dodgerblue3","skyblue3","skyblue2", "cadetblue1","dodgerblue4")) +
  theme_minimal() +
  theme(legend.position = "none", # quitar etiqueta
        #legend.text = element_text(size = 11), #tamaño de letra leyenda
        #legend.title = element_blank(), #quitar título de la leyenda
        panel.grid.major = element_blank(), # cuadricula de la gráfica
        axis.title = element_text(color = "gray28", size = 10, family = "Georgia", face="bold"), #diseño de los titulos (ejes)
        plot.title = element_text(size = 15, family="Georgia", hjust=rel(0.5), color="gray28"), #diseño del título
        #plot.subtitle = element_text( size = 12, hjust = 1),  subtitulo 
        axis.text.x = element_text(angle = 59, hjust=1, color="gray28"), #eje x
        axis.line = element_line(color = "gray28", linetype = "solid"),  #linea de los ejes
        axis.text = element_text(family="Georgia")
  )+
  ylim(c(0,30))
p3


# North America Plot
p4 <- ame_Nort_America %>% 
  ggplot(aes(x = reorder(threat_type, -n), y = n, fill=threat_type)) +
  geom_col(position = "identity", 
           #alpha = 0.7
           ) +
  ylab("") +
  xlab("") +
  ggtitle("NORTH AMERICA") + 
  scale_fill_manual(values=c("skyblue2","skyblue3","dodgerblue4", "cadetblue1", "dodgerblue3")) +
  theme_minimal() +
  theme(legend.position = "none", # quitar etiqueta
        #legend.text = element_text(size = 11), #tamaño de letra leyenda
        #legend.title = element_blank(), #quitar título de la leyenda
        panel.grid.major = element_blank(), # cuadricula de la gráfica
        axis.title = element_text(color = "gray28", size = 10, family = "Georgia", face="bold"), #diseño de los titulos (ejes)
        plot.title = element_text(size = 15, family="Georgia", hjust=rel(0.5), color="gray28"), #diseño del título
        #plot.subtitle = element_text( size = 12, hjust = 1),  subtitulo 
        axis.text.x = element_text(angle = 59, hjust=1, color="gray28"), #eje x
        axis.line = element_line(color = "gray28", linetype = "solid"),  #linea de los ejes
        axis.text = element_text(family="Georgia")
  )+
  ylim(c(0,70))
p4


# Oceania Plot
p5 <- ame_Oceania %>% 
  ggplot(aes(x = reorder(threat_type, -n), y = n, fill=threat_type)) +
  geom_col(position = "identity", 
          # alpha = 0.7
          ) +
  ylab("") +
  xlab("") +
  ggtitle("OCEANIA") + 
  scale_fill_manual(values=c("dodgerblue3","cadetblue1","skyblue3", "skyblue2", "dodgerblue4")) +
  theme_minimal() +
  theme(legend.position = "none", # quitar etiqueta
        #legend.text = element_text(size = 11), #tamaño de letra leyenda
        #legend.title = element_blank(), #quitar título de la leyenda
        panel.grid.major = element_blank(), # cuadricula de la gráfica
        axis.title = element_text(color = "gray28", size = 10, family = "Georgia", face="bold"), #diseño de los titulos (ejes)
        plot.title = element_text(size = 15, family="Georgia", hjust=rel(0.5), color="gray28"), #diseño del título
        #plot.subtitle = element_text( size = 12, hjust = 1),  subtitulo 
        axis.text.x = element_text(angle = 59, hjust=1, color="gray28"), #eje x
        axis.line = element_line(color = "gray28", linetype = "solid"),  #linea de los ejes
        axis.text = element_text(family="Georgia")
  )+
  ylim(c(0,50))
p5

# South America Plot
p6 <- ame_South_America %>% 
  ggplot(aes(x = reorder(threat_type, -n), y = n, fill=threat_type)) +
  geom_col(position = "identity", 
           #alpha = 0.7
           ) +
  ylab("") +
  xlab("") +
  ggtitle("SOUTH AMERICA") + 
  scale_fill_manual(values=c("dodgerblue3","skyblue3", "skyblue2" ,"cadetblue1", "dodgerblue4")) +
  theme_minimal() +
  theme(legend.position = "none", # quitar etiqueta
        #legend.text = element_text(size = 11), #tamaño de letra leyenda
        #legend.title = element_blank(), #quitar título de la leyenda
        panel.grid.major = element_blank(), # cuadricula de la gráfica
        axis.title = element_text(color = "gray28", size = 10, family = "Georgia", face="bold"), #diseño de los titulos (ejes)
        plot.title = element_text(size = 15, family="Georgia", hjust=rel(0.5), color="gray28"), #diseño del título
        #plot.subtitle = element_text( size = 12, hjust = 1),  subtitulo 
        axis.text.x = element_text(angle = 59, hjust=1, color="gray28"), #eje x
        axis.line = element_line(color = "gray28", linetype = "solid"),  #linea de los ejes
        axis.text = element_text(family="Georgia")
  )+
  ylim(c(0,70))
p6


### Load images

#Africa
img_Afica <- ggdraw() +
  draw_image("Africa.png", width = 1.2, height = 1.3) 
#Asia
img_Asia <- ggdraw() +
  draw_image("Asia.png", width = 1.2, height = 1.3)
#Europe
img_Europe <- ggdraw() +
  draw_image("Europe.png", width = 1.2, height = 1.3) 
#North America
img_North_America <- ggdraw() +
  draw_image("North_America.png", width = 1.2, height = 1.3) 
#Oceania
img_Oceania <- ggdraw() +
  draw_image("Oceania.png", width = 1.2, height = 1.3)
#South America
img_South_America <- ggdraw() +
  draw_image("South_America.png", width = 1.2, height = 1.3)


#grid of plots
images <- plot_grid(img_Afica, img_Asia, img_Europe, img_North_America, img_Oceania, img_South_America, ncol=6)

#Title and subtitle
title_subtitle <- ggdraw() +
  draw_text("Number of plants threatened according to continent", x= 0.25, y = 0.85, size = 25, col="gray28", family="serif") +
  draw_text("In total, 500 plant species are considered extinct as of 2020. 19.6% of those were endemic to Madagascar, 12.8% to Hawaiian islands.", 
            y = 0.64, x = 0.42, size = 15, col="gray28", family="Georgia")

#caption
caption <- ggdraw() +
  draw_text("BY: Laura Fuentes, Francisco Pautt, Brian Llinás, José Navarro | Images: Sheehan et al., (2018)",
            size = 12, x = 0.58, y = 0.2, hjust = 0,
            color = "gray28", family="serif")

#axis x
axis_x <- ggdraw()+
  draw_text("Type of threat",
            size = 10, x = 0.59, y = 0.3, hjust = 2.3, fontface="bold",
            color = "black", family="Georgia")



#Plots
plots <- plot_grid(p1, p2, p3, p4, p5, p6, ncol=6)

#final plot
final_plot <- plot_grid(
  
  title_subtitle,
  
  images,
  
   plots,
  
  axis_x,
  
  caption,
  
  
  ## plot settings
  rel_heights = c(3.1,3.5,4.5,0.0,1.5),
  nrow = 5
) 




#Save plot on computer
ggsave("plants_threats.png", 
       final_plot, 
       height = 18, width = 40, 
       units = "cm", dpi = 900)
