install.packages("lpSolve")
install.packages("linprog")
library(lpSolve)
library(linprog)

#Coeficientes de la funcion objetivo
z<-c(7,9,8,20,5,15)

#matriz de coeficientes de las restricciones
A<-matrix(c(0.25,0.5,0.25,0,0,1,
            0.4,0.2,0.4,0,0,1,
            0.1,0.3,0.6,0,0,1
            ,0.65,0.15,0.2,0,0,1,
            0.4,0.2,0.4,0,1,1,
            0.3,0.2,0.5,1,0,1),ncol=6)

#valores de disponibilidad
b<-c(30,40,35,10,12,100)

#sentido de las restricciones
dir<-c(">=","<=","<=","<=","<=","=")

#Solucion del problema
solucion<-solveLP(z,b,A,maximum = FALSE,dir,lpSolve = TRUE)
summary(solucion)
solucion
