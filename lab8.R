#Universidad del Valle de Guatemala
#08/10/2020
#Luis Esturban 17256
#Luis Fernandez 16344
#Juan Menchu 16150
#Lab8
setwd("../AnalisisR")
library("ggpubr")
library(corrplot)
library(plyr)
#################################Info General####################################
# Leer la entrada de los datos
HechoTransito<-read.csv("../Data/HechoTransito.csv",stringsAsFactors = FALSE)
VehiculosInvolucrados<-read.csv("../Data/VehiculosInvolucrados.csv",stringsAsFactors = FALSE)
fallecidosLesionados<- read.csv("../Data/FallecidosLesionados.csv", stringsAsFactors = FALSE)
importaciones <- read.csv("../Data/importacionesVehiculosSAT.csv", stringsAsFactors = FALSE)
hechosC1 <- read.csv("../Data/HechoTransitoC1.csv", stringsAsFactors = FALSE)
ane1 <- read.csv("../Data/Anexo2.csv", stringsAsFactors = FALSE)
an2 <- read.csv("../Data/Anexo3.csv", stringsAsFactors = FALSE)

# Tipo Cantidad de tipos de vehÃ?culsos durante todos los aÃ±os
cantTipoVeh <- table(importaciones[,"Tipo.de.Vehiculo"])
# Hacemos que el orden de la tabla sea descendiente
cantTipoVeh <- cantTipoVeh[order(cantTipoVeh, decreasing = TRUE)]      

totalMotos <- cantTipoVeh[1]
vectorTotalVeh <- c(totalMotos)
# GrÃ¡fico de barras
barplot(vectorTotalVeh,
        main = "Total de motos importados desde el aÃ±o 2011",
        xlab = "Moto", ylab = "Cantidad de importaciones",
        col = "royalblue")
cantHechosVeh <- table(VehiculosInvolucrados[,"tipo_veh"])
cantHechosVeh <- cantHechosVeh[order(cantHechosVeh, decreasing = TRUE)]
barp <- barplot(cantHechosVeh[1:1],
                main = "Cantidad de accidentes por moto desde el aÃ±o 2016",
                xlab = "Moto", ylab = "Cantidad de accidentes",
                col = "royalblue", 
                names.arg = c("Motocicleta"))
text(barp, 1, paste("total: ", vectorTotalAcc, sep="") ,cex=1, pos=3)

totalAccMotos <- cantHechosVeh[1]
vectorTotalAcc <- c(totalAccMotos)

#####################################################################
#graficas y frecuencias de los accidentes de moto
motos2011 <- count(importaciones[importaciones$Anio=="2011",], "Tipo.de.Vehiculo")
motos2011[42,]

motos2016 <- count(importaciones[importaciones$Anio=="2016",], "Tipo.de.Vehiculo")
motos2016[48,]

motos2017 <- count(importaciones[importaciones$Anio=="2017",], "Tipo.de.Vehiculo")
motos2017[47,]

motos2018 <- count(importaciones[importaciones$Anio=="2018",], "Tipo.de.Vehiculo")
motos2018[44,]

motosAccidentes <- count(VehiculosInvolucrados[VehiculosInvolucrados$tipo_veh=="4",], "año_ocu")
motosAccidentes[1,]
motosAccidentes[2,]
motosAccidentes[3,]

año_ocu<-c("2016","2017","2018")
accidentes<-c(motosAccidentes[1,2],motosAccidentes[2,2],motosAccidentes[3,2])
cantidad<-c(motos2016[48,2],motos2017[47,2],motos2018[44,2])
MotosFinal = data.frame(cbind(año_ocu,accidentes,cantidad))
fall_les <- table(fallecidosLesionados[,c("tipo_veh", "fall_les")])

ptable <- prop.table(t(fall_les[1:5,]), margin=2)
porcentajeFat <- c((format(round(ptable[1,1]*100,1),nsmall=1)),
                   (format(round(ptable[1,2]*100,1),nsmall=1)),
                   (format(round(ptable[1,3]*100,1),nsmall=1)),
                   (format(round(ptable[1,4]*100,1),nsmall=1)),
                   (format(round(ptable[1,5]*100,1),nsmall=1)))

porcentajeLes <- c((format(round(ptable[2,1]*100,1),nsmall=1)),
                   (format(round(ptable[2,2]*100,1),nsmall=1)),
                   (format(round(ptable[2,3]*100,1),nsmall=1)),
                   (format(round(ptable[2,4]*100,1),nsmall=1)),
                   (format(round(ptable[2,5]*100,1),nsmall=1)))



barp <- barplot(
  t(fall_les[1:5,]), col = c("royalblue", "grey"),
  main = "Proporcion de accidentes de motos en base al año",
  xlab = "Tipo de vehÃ?culo", ylab = "ProporciÃ³n",
  names.arg = c("AutomÃ³vil","Camioneta Sport","Pick Up","Motocicleta", "CamiÃ³n"),
  legend.text = c("Cantidad de fatalidades", "Cantidad de lesiones")
)
text(barp, 1, paste("lesiones: ", porcentajeLes, "%", sep="") ,cex=1, pos = 3)
text(barp, 1, paste("fatalidades: ", porcentajeFat, "%", sep="") ,cex=1)



barp <- barplot(
  accidentes,
  main = "Accidentes que involucran motos por año",
  xlab = "Año de ocurrencia",
  ylab = "cantidad de accidentes",
  names.arg = c("2016","2017","2018")
)
text(barp, 1, paste("Accidentes: ", accidentes, sep="") ,cex=1, pos = 3)

barp <- barplot(
  cantidad,
  main = "Motos importadas por año",
  xlab = "Año de importacion",
  ylab = "cantidad de motos",
  names.arg = c("2016","2017","2018")
)
text(barp, 1, paste("Motos: ", cantidad, sep="") ,cex=1, pos = 3)


PorcentajeAccidentes <- c(((4336*1000)/133195),((3111*1000)/124329),((3664*1000)/163821))
MotosFinal = data.frame(cbind(año_ocu,accidentes,cantidad,PorcentajeAccidentes))

