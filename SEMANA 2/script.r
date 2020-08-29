

##### Paquetes y datos #####
library(tidyverse)
library(stringr)
library(ggplot2)
library(ggpubr)
library(cowplot)
library(grImport2)
tuesdata <- tidytuesdayR::tt_load('2020-08-25')
chopped <- tuesdata$chopped

##### Separación de caracteres #####
appetizer <- unlist(strsplit(chopped$appetizer, ","))
entree <- unlist(strsplit(chopped$entree, ","))
dessert <- unlist(strsplit(chopped$dessert, ","))
appetizer <- as.data.frame(appetizer)
entree <- as.data.frame(entree)
dessert <- as.data.frame(dessert)





##### Función donut plot #####
donutplot_ing <-  function(food_data_frame){
  app_data <- as.data.frame(table(food_data_frame))
  app_data <- arrange(app_data,-Freq)[1:6,]
  colnames(app_data) <- c("ingredient", "count") 
  app_data$fraction <- app_data$count/sum(app_data$count)
  app_data$ymax <- cumsum(app_data$fraction)
  app_data$ymin <- c(0, head(app_data$ymax, n=-1))
  app_data$labelPosition <- (app_data$ymax + app_data$ymin) / 2
  app_data$label <- paste0(app_data$ingredient, "\n value: ", app_data$count)
  
fills <- c("#9A470E","#A96C3D","#256666","#2F8832","#EAB200","#C00000")
cols <- c("#734835","#965F36","#1F5857","#28722A","#BF9000","#9E0000")
random <- sample(6)
p <- ggplot(app_data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=2.5)) +
    geom_rect(color=cols[random], fill=fills[random]) + 
    geom_text(aes(label=app_data$ingredient,
                x=3.5,y=(ymin+ymax)/2),inherit.aes = T,
            show.legend = FALSE, vjust= "inward", size = 2.5)+
    coord_polar(theta="y") +
    xlim(c(-1, 4)) +
    theme_void() +
    theme(legend.position = "none")

return(p)
}

##### Plot combinado #####

app_plot <- donutplot_ing(appetizer)
ent_plot <- donutplot_ing(entree)
des_plot <- donutplot_ing(dessert)



wallpaper <- png::readPNG("background2.png") #Fondo previamente diseñado

plot <- ggdraw()+ 
  background_image(wallpaper)+
  draw_plot(app_plot, x = 0.15, y = 0.243, width = 0.220, height = 0.220) + #1
  draw_plot(ent_plot, x = 0.380, y =0.705, width = 0.220, height = 0.220) + #2
  draw_plot(des_plot, x = 0.622, y = 0.240, width = 0.220, height = 0.220)+#3
  draw_label("Principales ingredientes seg?n el plato", size = 18, angle = 360, x = 0.5, y = 0.97, hjust = 0.5, vjust = 0.5,
             fontfamily = "", fontface = "plain", color = "white",
             lineheight = 0.9, alpha = 1)+
  draw_label("Aperitivo", size = 18, angle = 360, x = 0.26, y = 0.20, hjust = 0.5, vjust = 0.5,
             fontfamily = "", fontface = "plain", color = "white",
             lineheight = 0.9, alpha = 1)+
  draw_label("Entrada", size = 18, angle = 360, x = 0.48, y = 0.68, hjust = 0.5, vjust = 0.5,
             fontfamily = "", fontface = "plain", color = "white",
             lineheight = 0.9, alpha = 1)+
  draw_label("Postre", size = 18, angle = 360, x = 0.75, y = 0.20, hjust = 0.5, vjust = 0.5,
             fontfamily = "", fontface = "plain", color = "white",
             lineheight = 0.9, alpha = 1)

##### Exportación #####
ggsave(filename = "plotgg.png",plot = plot, device = "png", height = 10, width = 10)

