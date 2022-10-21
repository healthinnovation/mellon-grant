setwd("~/Spatial-surveillance/Spatial_information_from_Arequipa2020/Manzanas _Arequipa/ASA//")

# upload packages
library(readxl)
library(rgeos)
library(geosphere)
library(rgdal)
library(rworldmap)
library(tidyr)
library(Stack)
library(SearchTrees)
library(data.table)
library(raster)
library(dplyr)
library(plyr)
#install.packages("tmap")
library(tmap)
library(ggmap)
library(maps)
library(GISTools)
library(ggmap)
library(maptools)
library(Cairo)
library(scales)
library(RColorBrewer)
library(spData) # example datasets
library(sp)
library(sf)
library(maptools)
library(stringr)
library(rvest)
library(dbplyr)
library(tidyverse)
library(ggplot2)

#reading in KML file (might remove)

# kml <- st_read("Alto Selva Alegre_Mz.kml") # incorrecto
layers<-st_layers("Alto Selva Alegre_Mz.kml")


layerslist <- list()
cnt <- 1
for (i in c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","16","17","19","21","23","24","25","26","27","28","29","30","31","32","33","34",
             "35", "36", "37", "38","39", "40", "41", "42", "43","44", "45", "46","47","48","49","50","57","58","60","61","63","67",
            "69","70","71","73","74","75","76","77","78","79", "80", "81","82", "83", "84", "85", "86","87", "88", "94", "95", "97","98", "99", "100","102")) {
  # load file
  layer.i <- switch(i
                    ,"1"="1.-Alto Selva Alegre Zona A"    
                    ,"2"="2.-Alto Selva Alegre Zona B"    
                    ,"3"="3.-Alto Selva Alegre Zona C"    
                    ,"4"="4.-Urb._Graficos"    
                    ,"5"="5.-Andres Avelino Caceres"   
                    ,"6"="6.-Coop. La Estrella"   
                    ,"7" ="7.-Asoc.de Viv.Antonio Jose de Sucre"    
                    ,"8" ="8.-San Jose"   
                    ,"9" ="9.-Coop. Viv. Villa el Sol P. Polanco"    
                    ,"10"="10.-UPIS Ramiro Priale"    
                    ,"11"="11.-Coop. Artesanal Aproma Pampas Polanco"
                    ,"12"="12.-Asoc.Viv. J. V. Alvarado"    
                    ,"13"= "13.-Asoc. Viv. Villa el Mirador P. Polanco"
                    ,"14"="14.-A.A.H.H. Javier Heraud Pampas Polanco"
                    ,"16"="16.-Asoc. de Vivienda Villa Arequipa"    
                    ,"17"="17.-Agusto Salazar Bondy"    
                    ,"19"="19.-Egasa Zona Rural"    
                    ,"21"="21.-Avis Cruce de Chilina"    
                    ,"23"="23.-Avis. Villa el Conquistador II"  
                    ,"24"="24.-Coop. de Viv. Vista Alegre Luis G. S."
                    ,"25"="25.-Coop. de Viv. Enatru"
                    ,"26"="26.-Coop. Cruce Chilina"    
                    ,"27"="27.-Coop. de Viv. los Eucaliptos"    
                    ,"28"="28.-Avis Rafael Hoyo Rubio"    
                    ,"29"= "29.-Asoc. Artesanal el Misti"    
                    ,"30"="30.-Avis Nestor Caceres Velasquez II"    
                    ,"31"="31.-Complejo Artesanal"   
                    ,"32"="32.-Avis Nestor C. Velasquez I"
                    ,"33"="33.-Ampliacion Apurimac"    
                    ,"34"="34.-Urb Apurimac"   
                    ,"35"="35.-AVIS 14 de Agosto"    
                    #,"36"="36.-AsociaciÃ³n Villa Vitarte y San Hilarion"
                    ,"36"="36.-Asociación Villa Vitarte y San Hilarion"
                    ,"37"="37.-AVIS Garcilazo de la Vega"    
                    ,"38"="38.-AVIS Leones del Misti"    
                    #,"39"="39.-AmpliaciÃ³n Villa Union" 
                    ,"39"="39.-Ampliación Villa Union" 
                    ,"40"="40.-Villa Union"   
                    ,"41" ="41.-AA. HH. Nueva Esperanza, San Luis"    
                    ,"42" ="42.-UPIS San Luis"   
                    ,"43" ="43.-AVIS Virgen de Chapi"    
                    #,"44"="44.-Habitacion Urbana Progresiva SeÃ±or de los Piedades"
                    ,"44"="44.-Habitacion Urbana Progresiva Señor de los Piedades"  
                    ,"45"="45.-Granjeros San Lazaro"
                    ,"46"="46.-Avis Mirador II"    
                    ,"47"= "47.-AAHH Bella Esperanza"
                    ,"48"="48.-Avis Javier Heraud parte alta"
                    ,"49"="49.-AAHH Amp. Javier Heraud"    
                    ,"50"="50.-AVIS Las Rocas"    
                    ,"57"="57.-Programa Municipal 1 Zona B"    
                    ,"58"="58.-Avis Los 3 Balcones del Misti"    
                    ,"60"="60.-Mirador Zona B"    
                    ,"61"="61.-Asociacion de vivienda Las Rocas del Mirador"    
                    ,"63"="63.-Chavin de Huantar"
                    ,"67"="67.-ASVIT El Huarangal B"
                    ,"69"="69.-Mirador Zeta 33"    
                    #,"70"="70.-Villa AsunciÃ³n"    
                    ,"70"="70.-Villa Asunción"    
                    ,"71"="71.-Villa Chachas"    
                    ,"73"= "73.-Lealtad Democratica"    
                    ,"74"="74.-Independencia Zona B"    
                    ,"75"="75.-Villa Independiente Zona A"   
                    ,"76"="76.-Villa Independiente Zona B"
                    ,"77"="77.-AA. HH. Primero de Enero"    
                    ,"78"="78.-Independencia Zona A I" 
                    ,"79"="79.-Independencia Zona A-II.kmz"    
                    ,"80"="80.-Villa San Pablo.kmz"    
                    ,"81"="81.-Villa Florida"    
                    ,"82"="82.-Pampa Chica"    
                    ,"83"="83.-Villa Salvador"  
                    ,"84"="84.-Asoc. Pro Vivienda San Lazaro"
                    ,"85"="85.-Andes del Misti"
                    ,"86"="86.-Balcones de Chilina"    
                    ,"87"="87.-Villa Confraternidad A-I"    
                    ,"88"="88.-Villa Confraternidad A-II"    
                    ,"94"= "94.-Villa Confraternidad B-II"    
                    ,"95"="95.-Villa Confraternidad Zona B III Margen Derecho"    
                    ,"97"="97.-Villa Ecologica Zona C I Margen Derecho"   
                    ,"98"="98.-Villa Ecologica Zona C II Margen Izquierda"
                    ,"99"="99.-Villa Ecologica Zona D-1"    
                    ,"100"="100.-Villa Ecologica Zona D-2 Margen Derecho"
                    ,"102"="102.-Villa Ecologica Zona E-2 Margen Izquierda")   
  
  asalayer<-st_read("Alto Selva Alegre_Mz.kml",layer.i)
  layerslist[[cnt]] <- asalayer
  cnt <- cnt + 1
}
layer <- do.call(rbind, layerslist)
plot(layer)

centroid <- st_centroid(layer)

plot(st_geometry(layer))
plot(centroid, add = T, col = 'red', pch = 18)
fortify(centroid)
class(centroid)

########Leyendo las casas y juntando con asa ####### encuestas rabia  

merge_mz<-read.csv("C:/Users/Cayetano/PETM-shiny/autoModel/model/input/MERGES_BLOCKS_GPS_ROCIADO/asa_gps_rociado.csv")
merge_mz<-merge_mz[,c(1:8)]

encuesta_mz<-read.csv("C:/Users/Cayetano/Rabies/rabia_ASA_encuesta_2016/resultados/houses_data.csv")
encuesta_mz<-encuesta_mz[,c(2,48)]


todo_mz<-merge(merge_mz,encuesta_mz, by="UNICODE", all.y=TRUE)
todo_mz<-unique(todo_mz)
todo_mz <- todo_mz[!(is.na(todo_mz$LATITUDE)),]

todo_mz=dplyr::mutate(todo_mz,code=paste(P,D,L,sep = "."))
todo_mz=dplyr::mutate(todo_mz,Name=paste(code,block,sep = "-"))

todo_mz_agregado<-aggregate(VIVEN_TOTAL~ Name,data = todo_mz,FUN = sum)
todo_mz_agregado$Name<-factor(todo_mz_agregado$Name)
levels(todo_mz_agregado$Name)

centroidmzasa<-merge(centroid,todo_mz_agregado,by="Name",all.y=TRUE)
plot(st_geometry(layer))
plot(st_geometry(centroidmzasa))



############# COMPARACION INEI DATA ###############
###################################################

INEIDATA <- readOGR("C:/Users/Cayetano//Spatial-surveillance/INEI_information_by_block/Mapa/Mz_PrincipalesCiudades.shp", 
                   "Mz_PrincipalesCiudades") 
INEIDATA <- subset(INEIDATA,NOMBDIST == "ALTO SELVA ALEGRE")
plot(INEIDATA)

library(rgdal)
head(INEIDATA@data)
INEIDATA$NOMCCPP <- factor(INEIDATA$NOMCCPP)
levels(INEIDATA$NOMCCPP) # "CAYMA"
levels(INEIDATA$CODZONA)
levels(INEIDATA$SUFZONA)


data_pob <- read_xlsx("C:/Users/Cayetano//Spatial-surveillance/INEI_information_by_block/Dato/Poblacion_1.xlsx") # old
data_pob <- data_pob[,c(1:10,22:30)] 
data_pob<-data_pob%>% filter(`Nombre de Distrito`=="ALTO SELVA ALEGRE")

data_completo <- merge(INEIDATA, data_pob, 
                       by.x="IDMANZANA", by.y="Idmanzana", all.x=T, all.y=F)

plot(data_completo)
sum(is.na(data_completo$`Población Total`))
#[1] 1153
sum(!is.na(data_completo$`Según sexo Mujer`))
#[1] 876

hist(data_completo$`Población Total`)
tmp1_map <- data_completo
tmp1_map@data$`Población Total`[is.na(tmp1_map@data$`Población Total`)]<-NA

png(filename="INEI_ASA_POBLACION.png", width=1100, height=600)
qtm(tmp1_map, fill = 'Población Total', fill.palette="Blues",
    fill.title="Poblacion")
dev.off()


# comparacion
# data_completo$POBTOTAL <- data_completo[,25]
# names(data_completo@data)
# hist(data_completo@data$POBTOTAL)
# data_completo_df <- data_completo@data

names(data_completo)

png(filename="comparacion.png", width=1100, height=600)
ggplot() +
  geom_sf(data = data_completo, aes(fill="Población Total"))+
  geom_sf_text(data = centroidmzasa, aes(geometry,label = "VIVEN_TOTAL"),
               size=1.2, colour = "RED")
dev.off()


png(filename="comparacionpervac.png", width=1100, height=600)
ggplot()+
  geom_sf(data = data_completo, aes(fill="POBTOTAL"))+
  geom_sf_text(data = centroidmzasa, aes(geometry,label = "VIVEN_TOTAL"),
               size=1.2, 
               colour = "RED")
dev.off()
