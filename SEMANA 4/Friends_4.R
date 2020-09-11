library(ggraph)
library(igraph)
library(tidyverse)
library(RColorBrewer)
library(friends)
library(ggimage)

data <- friends[,c(1,2)]
data <- data[str_length(data$text)>10, ]

data_Joey <- data %>% 
  filter(speaker=="Joey Tribbiani")
data_Joey <- data.frame(table(data_Joey$text))
data_Joey <- data_Joey %>% 
  arrange(desc(Freq))
data_Joey <- data_Joey[1:10,]
data_Joey$Var1 <- as.character(data_Joey$Var1)
data_Joey$Var1 <- paste(data_Joey$Var1,"-.J",sep = " ")

data_Monica <- data %>% 
  filter(speaker=="Monica Geller")
data_Monica <- data.frame(table(data_Monica$text))
data_Monica <- data_Monica %>% 
  arrange(desc(Freq))
data_Monica <- data_Monica[1:10,]
data_Monica$Var1 <- as.character(data_Monica$Var1)
data_Monica$Var1 <- paste(data_Monica$Var1,"-.M",sep = " ")

data_Ross <- data %>% 
  filter(speaker=="Ross Geller")
data_Ross <- data.frame(table(data_Ross$text))
data_Ross <- data_Ross %>% 
  arrange(desc(Freq))
data_Ross <- data_Ross[1:10,]
data_Ross$Var1 <- as.character(data_Ross$Var1)
data_Ross$Var1 <- paste(data_Ross$Var1,"-.R",sep = " ")

data_Rachel <- data %>% 
  filter(speaker=="Rachel Green")
data_Rachel <- data.frame(table(data_Rachel$text))
data_Rachel <- data_Rachel %>% 
  arrange(desc(Freq))
data_Rachel <- data_Rachel[-c(4,8,11),]
data_Rachel <- data_Rachel[1:10,]
data_Rachel$Var1 <- as.character(data_Rachel$Var1)
data_Rachel$Var1 <- paste(data_Rachel$Var1,"-.Rch",sep = " ")

data_Chandler <- data %>% 
  filter(speaker=="Chandler Bing")
data_Chandler <- data.frame(table(data_Chandler$text))
data_Chandler <- data_Chandler %>% 
  arrange(desc(Freq))
data_Chandler <- data_Chandler[1:10,]
data_Chandler$Var1 <- as.character(data_Chandler$Var1)
data_Chandler$Var1 <- paste(data_Chandler$Var1,"-.Ch",sep = " ")

data_Phoebe <- data %>% 
  filter(speaker=="Phoebe Buffay")
data_Phoebe <- data.frame(table(data_Phoebe$text))
data_Phoebe <- data_Phoebe %>% 
  arrange(desc(Freq))
data_Phoebe <- data_Phoebe[1:10,]
data_Phoebe$Var1 <- as.character(data_Phoebe$Var1)
data_Phoebe$Var1 <- paste(data_Phoebe$Var1,"-.P",sep = " ")


df_to <- rbind(data_Joey,data_Monica,data_Ross,data_Rachel,data_Chandler,data_Phoebe)

#Make the edge for the graph
df1=data.frame(from="origin", to=c("Joey","Monica","Ross","Rachel","Chandler","Phoebe"))
df2=data.frame(from=rep(df1$to, each=10), to=df_to$Var1)
ejes=rbind(df1, df2)

#Make vertices
vertice = data.frame(
  name = c(unique(as.character(ejes$from)),as.character(ejes$to[7:66])), 
  value = runif(67)
) 
vertice$value[8:67] <- df_to$Freq
vertice$value[1:7] <- NA

vertice$group = ejes$from[ match( vertice$name, ejes$to ) ]

vertice$id=NA
myleave=which(is.na( match(vertice$name, ejes$from) ))
nleave=length(myleave)
vertice$id[ myleave ] = seq(1:nleave)
vertice$angle= 100+(90 - 360 * vertice$id / nleave)

vertice$hjust<-ifelse( vertice$angle < -90, 1, 0)
vertice$angle<-ifelse(vertice$angle < -90, vertice$angle+180, vertice$angle)

thegraph <- graph_from_data_frame( ejes, vertices=vertice )
#Make the color palette
pal <- c("#FF4238", "#ffdc00", "#42a3db", "#9A0006", "#FFF580", "#3535de")

#Charge the img
img <- vertice %>% 
  distinct(group) %>% 
  mutate(paste0(tolower(group),".png"))
img <- img[-c(1,2),]
colnames(img) <- c("group","imagen")
img$group <- as.character(img$group)
img$ycord <- c(2.5,1.5,0.5,-0.5,-1.5,-2.5)

#Make the Graph
plot <- ggraph(thegraph, layout = 'dendrogram', circular = TRUE) + 
  geom_edge_diagonal(colour="grey") +
  scale_edge_colour_distiller(palette = "RdPu") +
  geom_node_text(aes(x = x*1.15, y=y*1.15,filter=leaf,label=name, angle =angle, hjust=hjust, colour=group), size=4.5) +
  geom_node_point(aes(filter = leaf, x = x*1.07, y=y*1.07, colour=group, size=value)) +
  ggtitle("Phrases more repeated\nby characters")+
  theme_void() +
  annotate("text", x = -3, y = -4, hjust = 0, label = "Data: library(Friends)\nCoding by: Sergio Rueda", color = "white",
           family = "Gregor Miller's Friends Font", size = 4)+
  scale_color_manual(values = pal)+
  scale_size_continuous( range = c(0.1,5) )+
  geom_point(data = img, aes(x = 3.5, y = ycord, color = group), size = 30) +
  geom_image(data = img, aes(x = 3.5, y =ycord , image = imagen), size = 0.05, asp = 2) +
  theme(
    plot.background = element_rect(fill = "grey15", color = NA),
    legend.position="none",
    plot.margin=unit(c(0,0,0,0),"cm"),
    plot.title =element_text(size=30,family="Gregor Miller's Friends Font",colour = "white",hjust = 0.5,margin = margin(20,0,0,0))
  ) +
  expand_limits(x = c(-5, 5), y = c(-3, 3))

ggsave("TidytuesdayFriends.png",plot,path = "/home/ruedastorga/Documentos/Analytics/Tidytuesday/Septiembre_08_2020/FINAL",width = 15,height = 9,dpi = 400)
