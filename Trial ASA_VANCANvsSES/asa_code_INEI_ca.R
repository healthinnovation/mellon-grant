### MERGE HOUSES AND DOGS DATASET TO EVALUTE PARTICIPATION ###
#library(tidyr)
#library(Stack)
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
library(stringr)
library(rvest)
library(dbplyr)
library(tidyverse)
library(ggplot2)

#setwd("~/Rabies")
data <- read.csv("~/Rabies/rabia_ASA_encuesta_2018/resultados/houses_merged_2019-06-02.csv")
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
                    ,"36"="36.-Asociación Villa Vitarte y San Hilarion"    
                    ,"37"="37.-AVIS Garcilazo de la Vega"    
                    ,"38"="38.-AVIS Leones del Misti"    
                    ,"39"="39.-Ampliación Villa Union"   
                    ,"40"="40.-Villa Union"   
                    ,"41" ="41.-AA. HH. Nueva Esperanza, San Luis"    
                    ,"42" ="42.-UPIS San Luis"   
                    ,"43" ="43.-AVIS Virgen de Chapi"    
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
# Warning messages:
#   1: In st_centroid.sf(layer) :
#   st_centroid assumes attributes are constant over geometries of x
# 2: In st_centroid.sfc(st_geometry(x), of_largest_polygon = of_largest_polygon) :
#   st_centroid does not give correct centroids for longitude/latitude data

plot(st_geometry(layer))
plot(centroid, add = T, col = 'red', pch = 18)
fortify(centroid)
class(centroid)


########### check single VANCAN  ###########
table(data$year2016, useNA = "ifany") # 2853 houses with dogs

# check in 2016
data <- subset(data, year2016 == 1)
table(data$VIVIENDA_VANCAN_2016, useNA = "ifany") # 2853 houses with dogs
# 0    1 
# 1018 1835 

# general part in VANCAN 2016
table(data$VIVIENDA_VANCAN_2016)[2]/nrow(data) # 0.6431826 
names(data)
datasub <- data[,c("UNICODE", "TYPE_L",
                   "VIVIENDA_VANCAN_2016")]

table(data$VIVIENDA_VANCAN_2016)[2]/nrow(data) # 0.6431826 


## agregar manzana nuestra
merge_mz<-read.csv("~/Rabies/rabia_ASA_encuesta_2016/asa_gps_rociado.csv")
merge_mz<-merge_mz[,c(1:8)]


encuesta_mz<-read.csv("~/Rabies/rabia_ASA_encuesta_2016/resultados/houses_data.csv")
encuesta_mz<-encuesta_mz[,c(2,48)]


#todo_mz<-merge(merge_mz, encuesta_mz, by="UNICODE", all.y=TRUE)
todo_mz<-merge(merge_mz, datasub, by="UNICODE", all.y=TRUE)
todo_mz<-unique(todo_mz)
todo_mz <- todo_mz[!(is.na(todo_mz$LATITUDE)),]

todo_mz=dplyr::mutate(todo_mz,code=paste(P,D,L,sep = "."))
todo_mz=dplyr::mutate(todo_mz,Name=paste(code,block,sep = "-"))

todo_mz_agregado<- aggregate(VIVIENDA_VANCAN_2016~ Name,
                             data = todo_mz,FUN = mean)
todo_mz_agregado$VIVIENDA_VANCAN_2016 <- round(todo_mz_agregado$VIVIENDA_VANCAN_2016,
                                               digits = 4)
todo_mz_agregado$Name<-factor(todo_mz_agregado$Name)
levels(todo_mz_agregado$Name)

centroidmzasa<-merge(centroid,
                     todo_mz_agregado,
                     by="Name",all.y=TRUE)
plot(st_geometry(layer))
plot(st_geometry(centroidmzasa))

centroidmzasa$geometry1 <- gsub('^.|.$', '', centroidmzasa$geometry)
centroidmzasa$geometry1 <- sub('.', '', centroidmzasa$geometry1)

centroidmzasa <- centroidmzasa %>%
  separate(geometry1, c("LONGITUDE", "LATITUDE"), ", ")

names(centroidmzasa)
# [1] "Name"                 "Description"          "VIVIENDA_VANCAN_2016" "LONGITUDE"           
# [5] "LATITUDE"             "geometry" 

write.csv(centroidmzasa, "vacpercent_per_mz.csv", row.names = F)

# TO SPATIALDF
# ORDER: LONG LAT
str(centroidmzasa) # hay un sf que no deja subset rapido.

xydf <- data.frame(cbind(centroidmzasa$Name,
                         centroidmzasa$LONGITUDE, centroidmzasa$LATITUDE,
                         centroidmzasa$VIVIENDA_VANCAN_2016))
colnames(xydf) <- c("Name","long", "lat", "VANCAN_2016")
xydf <- na.exclude(xydf)

xy <- xydf[,c(2,3)]
str(xy)
xy$long <- as.numeric(xy$long)
xy$lat <- as.numeric(xy$lat)

centroidmzasa_sp <- SpatialPointsDataFrame(coords = xy, 
                                           data = xydf,
                                           proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))


writeOGR(centroidmzasa_sp, 
         dsn=".", layer=paste("vacpercent_per_mz",
                              sep=""), driver="ESRI Shapefile")

# projections
latlon_CRS <- CRS("+proj=longlat +ellps=WGS84")
summary(centroidmzasa_sp)
centroidmzasa_spnew <- spTransform(centroidmzasa_sp,latlon_CRS)
# This step is not necessary anymore.
latlong = "+proj=lcc +lat_1=9 +lat_2=3 +lat_0=6 +lon_0=-66 +x_0=1000000 +y_0=1000000 +ellps=intl +towgs84=-288,175,-376,0,0,0,0 +units=m +no_def"
proj4string(centroidmzasa_spnew) <- latlong
summary(centroidmzasa_spnew)
# Is projected: TRUE 
plot(centroidmzasa_spnew)

# match de manzanas
INEIDATA <- readOGR("~/Spatial-surveillance/INEI_information_by_block/Mapa/Mz_PrincipalesCiudades.shp", 
                    "Mz_PrincipalesCiudades") 
INEIDATA<-subset(INEIDATA,NOMBDIST == "ALTO SELVA ALEGRE")
plot(INEIDATA)
summary(INEIDATA)
INEIDATAnew <- spTransform(INEIDATA,latlon_CRS)
proj4string(INEIDATAnew) <- latlong
summary(INEIDATAnew)
# Is projected: TRUE 
plot(INEIDATAnew)

centroidmzasa_spnewdf <- centroidmzasa_spnew@data
dim(centroidmzasa_spnewdf) # 317   4
INEIDATAnewdf <- INEIDATAnew@data
dim(INEIDATAnewdf) #  2029   15

library(rgeos)
library(sp)
library(rgdal)
popol <- over(centroidmzasa_spnew, INEIDATAnew)
popol2 <- over(centroidmzasa_spnew, INEIDATAnew)
?over

# use popol 2. has manzana
dataset <- cbind(xydf, popol2[15])

# 
# library(sf)
# # Shapefile from ABS: 
# 
# pnts <- centroidmzasa_spnew %>% mutate(
#   intersection = as.integer(st_intersects(geometry, INEIDATAnew)),
#   area = if_else(is.na(intersection), '', 
#                    INEIDATAnew$CODMZNA[intersection])
# ) 
# 
# pnts
# 
# library(mgcv)
# in.out(INEIDATAnew,centroidmzasa_spnew)
# 
# library(secr)
# names(centroidmzasa_spnew)
# class(centroidmzasa_spnewdf)
# str(centroidmzasa_spnewdf)
# centroidmzasa_spnewdf$long <- as.numeric(centroidmzasa_spnewdf$long)
# centroidmzasa_spnewdf$lat <- as.numeric(centroidmzasa_spnewdf$lat)
# popol3 <- data.frame(pointsInPolygon(data.frame(centroidmzasa_spnewdf[,c(2,3)]), 
#                                      INEIDATAnew))
# 

### ss status
data_pob <- read_xlsx("C:/Users/Cayetano//Spatial-surveillance/INEI_information_by_block/Dato/Poblacion_1.xlsx") # old
data_pob2 <- read_xlsx("C:/Users/Cayetano//Spatial-surveillance/INEI_information_by_block/Dato/Poblacion_2.xlsx") # old
hogar <- read_xlsx("C:/Users/Cayetano//Spatial-surveillance/INEI_information_by_block/Dato/hogar.xlsx",  skip=1) # old
vivienda <- read_xlsx("C:/Users/Cayetano//Spatial-surveillance/INEI_information_by_block/Dato/vivienda.xlsx",  skip=1) # old

names(data_pob)
names(data_pob2)
names(hogar)
names(vivienda)

# mientras
vivienda<-vivienda%>% filter(`Nombre de Distrito`=="ALTO SELVA ALEGRE")

data_completo <- merge(INEIDATA, vivienda, 
                       by.x="IDMANZANA", by.y="Idmanzana", 
                       all.x=F, all.y=T)
plot(data_completo)
data_completodf <- data_completo@data
names(data_completodf)
data_completo$proxy <- data_completo@data[,27]
data_completodf <- data_completo@data
table(data_completo$proxy)
# 0   1   2   3   4   5   6   8  12 
# 800  50  12   6   4   1   1   1   1 

names(data_completo)
data_completosub <- data_completo[,c(1,177)]
data_completosubdf <- data_completosub@data

names(dataset)
names(data_completosubdf)

dataset_full <- merge(dataset, data_completosubdf, by="IDMANZANA", all.x=T, all.y=F)
#### este es el data set que necesitamos. proxy es el modelo de nuestra variable INEI,
# la que debemos crear.