library(tidytuesdayR)
library(tidyverse)
library(magrittr)

# Leemos los datos
tuesdata <- tidytuesdayR::tt_load('2020-08-18')
plants <- tuesdata$plants
threats <- tuesdata$threats
actions <- tuesdata$actions

table(plants$group)
table(actions$action_type)
library(ggdark)
p<-ggplot(data=threats,mapping = aes(x=year_last_seen,fill=continent))+geom_bar(show.legend = T, colour = "white")+
  xlab("Años")+ scale_x_discrete(limit = c("Before 1900","1900-1919", "1920-1939", "1940-1959",
                                                 "1960-1979","1980-1999","2000-2020"))+hrbrthemes::theme_ft_rc( plot_title_family = "Times")+ggtitle("Cantidad de plantas extintas cada 20 años por continente")+
  theme(axis.title.x = element_text(face="bold",vjust=-0.5, colour="white", size=rel(1.3)))+theme(axis.title.y = element_text(face="bold",vjust=-0.5, colour="white", size=rel(1.3)))
p1<-p+theme (axis.text.x = element_text(face="italic", colour="white", size=rel(1)),
             axis.text.y = element_text(face="bold", colour="white", size=rel(1), angle=90, hjust=0.5))
p2<-p1+dark_theme_gray()+theme(plot.title = element_text(hjust = 0.5))
library(EBImage)
require(magick)
library(ggimage)
library(hrbrthemes)
library(ggpubr)
imag <- "https://static.dezeen.com/uploads/2014/03/Bioglow-Roosegaarde-Dezeen_644.jpg"
ggbackground(p2,imag)+ggsave(filename = "flores.jpg",width = 16,height = 10)

