# Oscar Juarez
# Febrero 2019
# Script que sirve para descargar y unir archivos de datos de vehículos de la INE

#Paquetes necesarios
#install.packages("lubridate")
#install.packages("stringr")
#install.packages("openxlsx")
#install.packages("dplyr")

library("tools")
library("lubridate")
library("stringr")
library("openxlsx")
library("dplyr")

# Directorio principal
setwd("./Data")

# Para obtener los links usé la siguiente shiny apps disponible en:https://spannbaueradam.shinyapps.io/r_regex_tester/ con el 
# código fuente de la página de la SAT y el siguiente patrón: https://portal.sat.gob.gt/portal/descarga/5030/importacion-de-vehiculos/[[:digit:]]{4}/importacion_de_vehiculos_[[:digit:]]{4}_[[:lower:]]+.zip

download_without_overwrite <- function(url, folder, nombreArchivo, anio)
{
  filename <- basename(url)
  base <- tools::file_path_sans_ext(filename)
  ext <- tools::file_ext(filename)

  file_exists <- grepl(base, list.files(folder), fixed = TRUE)

  filename <- paste0(nombreArchivo, "_" , anio, ".", ext)

  download.file(url, file.path(folder, filename), mode = "wb", method = "libcurl")
}

links_vehiculos_involucrados<-"
https://www.ine.gob.gt/sistema/uploads/2017/05/30/MVtI7adfQzZWF5uefXZ4xmZVG0vRik3S.xlsx

https://www.ine.gob.gt/sistema/uploads/2018/06/01/2018060193914Nyto5KpgXeUsKGoT4SpRknBumA8etDe4.xlsx

https://www.ine.gob.gt/sistema/uploads/2019/06/10/20190610171521QZwxwRe5OLADYzMpAncpW3yR4defMHqN.xlsx
"

links_hechos_transito<-"
https://www.ine.gob.gt/sistema/uploads/2017/05/30/DX2BmYU5m4JfPRhrFHwDRDEs49V7fN5I.xlsx

https://www.ine.gob.gt/sistema/uploads/2018/06/01/2018060194026zskZfNalr2em0qLC5Wn6bxC1mBim617t.xlsx

https://www.ine.gob.gt/sistema/uploads/2019/06/06/20190606220636xaPevkVgXaNin0L0ZmXiN4fm18JAFoLG.xlsx
"

links_fallecidos_lesionados<-"
https://www.ine.gob.gt/sistema/uploads/2017/05/30/MVtI7adfQzZWF5uefXZ4xmZVG0vRik3S.xlsx

https://www.ine.gob.gt/sistema/uploads/2018/06/19/201806191110218FciGNFOtT2FJnkTOS0pTzPcDOW8FpLB.xlsx

https://www.ine.gob.gt/sistema/uploads/2019/06/06/20190606220307YRSvO7OHib0rQxKDCTAl2fkXMl05g9Uz.xlsx
"


links_vehiculos_involucrados<-str_trim(unlist(strsplit(links_vehiculos_involucrados,"[[:cntrl:]]")))
links_vehiculos_involucrados<-links_vehiculos_involucrados[links_vehiculos_involucrados!=""]

links_hechos_transito<-str_trim(unlist(strsplit(links_hechos_transito,"[[:cntrl:]]")))
links_hechos_transito<-links_hechos_transito[links_hechos_transito!=""]

links_fallecidos_lesionados<-str_trim(unlist(strsplit(links_fallecidos_lesionados,"[[:cntrl:]]")))
links_fallecidos_lesionados<-links_fallecidos_lesionados[links_fallecidos_lesionados!=""]

# Descargar vinculos de vehículos involucrados
anio <- 2016
for (vinculo in links_vehiculos_involucrados) {
	download_without_overwrite(
		vinculo,
		getwd(),
		"vehiculos_involucrados",
		anio
	)
	anio <- anio + 1
  Sys.sleep(1)
}

# Descargar vinculos de hechos de tránsito
anio <- 2016
for (vinculo in links_hechos_transito) {
	download_without_overwrite(
		vinculo,
		getwd(),
		"hechos_transito",
		anio
	)
	anio <- anio + 1
  Sys.sleep(1)
}

# Descargar vinculos de fallecidos y lesionados
anio <- 2016
for (vinculo in links_fallecidos_lesionados) {
	download_without_overwrite(
		vinculo,
		getwd(),
		"fallecidos_lesionados",
		anio
	)
	anio <- anio + 1
  Sys.sleep(1)
}


# Se leen los archivos del directorio xlsx
listaArchivos<-list.files(getwd())
head(listaArchivos,30)

# Por cada archivo..
for (archivo in listaArchivos){
  print(archivo)
  base <- tools::file_path_sans_ext(archivo)
  
  # Si es de tipo vehiculos involucrados
	if(substr(base, 1, nchar(base)-5) == "vehiculos_involucrados"){
	  
	  # Se crea el df si no existe
	  if(!exists("dfVehiculosInvolucrados"))
	  {
	    dfVehiculosInvolucrados <- data.frame() 
	    dfVehiculosInvolucrados <- read.xlsx(archivo)
	  }
	  
	  # Se adjuntan el resto de df's
	  else
	  {
	    temp_dataset <- read.xlsx(archivo)
	    dfVehiculosInvolucrados <- Reduce(function(...)merge (..., all=T), list(dfVehiculosInvolucrados, temp_dataset))
	    rm(temp_dataset)
	  }
	}
  
  # Repetir con otros archivos 
  else if (substr(base, 1, nchar(base)-5) == "hechos_transito"){
    if(!exists("dfHechoTransito"))
    {
      dfHechoTransito <- data.frame() 
      dfHechoTransito <- read.xlsx(archivo)
    }
    else
    {
      temp_dataset <- read.xlsx(archivo)
      dfHechoTransito <- Reduce(function(...)merge (..., all=T), list(dfHechoTransito, temp_dataset))
      rm(temp_dataset)
    }
  }

  else if (substr(base, 1, nchar(base)-5) == "fallecidos_lesionados"){
    if(!exists("dfFallecidosLesionados"))
    {
      dfFallecidosLesionados <- data.frame() 
      dfFallecidosLesionados <- read.xlsx(archivo)
    }
    else
    {
      temp_dataset <- read.xlsx(archivo)
      dfFallecidosLesionados <- Reduce(function(...)merge (..., all=T), list(dfFallecidosLesionados, temp_dataset))
      rm(temp_dataset)
    }
  }
}

#Dataframes que contiene las variables que vamos a utilizar
dfHechoTransitoLimpio <- select(dfHechoTransito,núm_corre, día_ocu,año_ocu,mes_ocu,día_sem_ocu,mupio_ocu,depto_ocu,tipo_veh,tipo_eve,g_hora,g_hora_5,)
dfVehiculosInvolucradosLimpio <- select(dfVehiculosInvolucrados,núm_corre, día_ocu,año_ocu,mes_ocu,día_sem_ocu,mupio_ocu,depto_ocu,sexo_per,edad_per,mayor_menor,tipo_veh,tipo_veh,g_edad_80ymás,g_edad_60ymás,edad_quinquenales,g_hora,g_hora_5)
dfFallecidosLesionadosLimpioselect <- select(dfFallecidosLesionados,núm_corre,año_ocu,día_ocu,g_hora,g_hora_5,mes_ocu,día_sem_ocu,mupio_ocu,depto_ocu,sexo_per,edad_per,g_edad_80ymás,g_edad_60ymás,edad_quinquenales,mayor_menor,tipo_veh,tipo_eve,fall_les,int_o_noint)


names(dfFallecidosLesionadosLimpioselect)
names(dfVehiculosInvolucradosLimpio)
names(dfHechoTransitoLimpio)

write.csv(dfHechoTransitoLimpio, file="HechoTransito.csv",row.names = F)
write.csv(dfVehiculosInvolucradosLimpio, file="VehiculosInvolucrados.csv",row.names = F)
write.csv(dfFallecidosLesionadosLimpioselect, file="FallecidosLesionados.csv",row.names = F)
# Salir del directorio de los .xlsx
setwd("../../AnalisisR/")
