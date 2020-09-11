devtools::install_github("EmilHvitfeldt/friends")
library(friends)
library(ggplot2)
library(tm)
library(wordcloud2)
library(SnowballC)
library(qdapRegex)     # Removing URLs 
library(tidytext)
library(tidyverse)
library(stringr)



#********************************EMOCIONES************************************
d<-friends_emotions
png<-readPNG("friends.png")
ggplot(d, aes(x=season, color= emotion)) +
     geom_bar(aes(fill = emotion))+ 
     facet_grid(~emotion)+
     #background_image(png)+
     labs(title = "Emociones después de cada temporada de Friends", 
          subtitle = "S01 - S04",
                   y="Cantidad", x="Temporada")+
     theme_minimal()+
     theme(text = element_text(size=12),
                     legend.text = element_text( size = 9),
                     legend.title = element_text(size=9),
                     legend.position = "top",
                     #borde de los cuadros,
                     legend.background = element_rect(fill = "white", color = NA),
                     legend.key = element_rect(fill = "white"))+
  theme(plot.title = element_text(hjust=0.5), 
        plot.subtitle = element_text(hjust = 0.5))

#********************************** How you doin'? *********************************
library(dplyr)

p<-friends %>%
  select(text,speaker,season)%>%
  filter(speaker=="Joey Tribbiani") %>%
  mutate(text = str_to_lower(text), 
         season = as.character(season)) %>%
  filter(str_detect(text, "how are you doing?") |
         str_detect(text, "how ya doin") |
         str_detect(text, "how you doin")|
         str_detect(text, "how're you doin")) %>%
  ggplot()+
  geom_bar(aes(x=season, fill=season))+
  coord_polar()+
  theme_minimal()+
  labs(y="Cantidad", x="Temporada")


#********************************* Intento 2 ********************************
Rachel <- nrow(friends %>% filter(speaker == "Rachel Green"))
Ross <- nrow(friends %>% filter(speaker == "Ross Geller"))
Monica <- nrow(friends %>% filter(speaker == "Monica Geller"))
Joey <- nrow(friends %>% filter(speaker == "Joey Tribbiani"))
Chandler <- nrow(friends %>% filter(speaker == "Chandler Bing"))
Phoebe <- nrow(friends %>% filter(speaker == "Phoebe Buffay"))

Speaker <- c("Rachel", "Ross", "Monica", "Joey", "Chandler", "Phoebe")
Lines <- c(Rachel,Ross,Monica,Joey,Chandler,Phoebe)

df<-data.frame(Speaker, Lines)
df

ggplot(df, aes(x=Speaker, y = Lines, color=Speaker))+
  geom_col(aes(fill=Speaker))+
  coord_flip()+
  theme_minimal()
  # transition_manual(Speaker) #Sí sirve, pero no aguanta
  
  


