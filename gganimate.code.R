install.packages("tidyverse")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("transformr")
install.packages("png")
install.packages("gifski")
install.packages("gganimate")
install.packages("pROC")
library(pROC)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(transformr)
library(png)
library(gifski)
library(gganimate)

#llamamos el archivo 
ve <- read.csv(file="BD-RAMV- ANONYMIZERED.csv",sep=";")

#solo nos quedamos con las columnas de interes
datos1 <- select(ve, OCUPACION.U.OFICIO,EDAD,NIVEL.DE.ESCOLARIDAD,CONVALIDADO,DEPARTAMENTO,GENERO,EXPERIENCIA.CERTIFICADA,SECTOR.DE.OCUPACION)

#agrupamos las categorías de ninguno, no aplica y desempleado
datos1$NIVEL.DE.ESCOLARIDAD=fct_collapse(datos1$NIVEL.DE.ESCOLARIDAD,Ninguno=c("Ninguno","No Aplica"))
datos1$OCUPACION.U.OFICIO=fct_collapse(datos1$OCUPACION.U.OFICIO,Desempleado=c("No Aplica","Desempleado"))

#filtramos las edades entre 18 y 60 años y eliminamos la categoria estudiante
ocupacion <- datos1%>%
  filter(EDAD %in% 18:60)%>%
  filter(OCUPACION.U.OFICIO!="Estudiante")

#GGanimate1 ocupacion segun edad animado
datos2<-datos1
datos2$OCUPACION.U.OFICIO=fct_collapse(datos2$OCUPACION.U.OFICIO,"Desempleado"=c("Hogar","Desempleado","Estudiante"))
datos2$OCUPACION.U.OFICIO=fct_collapse(datos2$OCUPACION.U.OFICIO,"Empleado"=c("Empleado Formal (Contrato Laboral)","Empleado Informal (Sin Contrato Laboral)","Independiente"))

ocupacion1 <- datos2%>%
  count(OCUPACION.U.OFICIO,EDAD)%>%
  filter(EDAD %in% 18:60)%>%
  filter(OCUPACION.U.OFICIO!="Estudiante")
pgg <- ggplot(
  ocupacion1,
  aes(EDAD, n, color=OCUPACION.U.OFICIO)) +   geom_line() +   scale_alpha() +
  labs(x = "Edades", y = "Cantidad de personas") +
  theme(legend.position = "top")
pgg+   
  geom_point(aes(group = seq_along(EDAD))) +
  transition_reveal(EDAD)