library(sp)
library(sf)
library(maptools)
library(stringr)
library(rvest)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(data.table)
library(readxl)
library(gridExtra)
library(tidyr)
setwd("~/Spatial-surveillance/INEI_information_by_block/Mapa/")



###### LEYENDO INEI DATA ########
#################################
INEIDATA<-st_read("~/Spatial-surveillance/INEI_information_by_block/Mapa/Mz_PrincipalesCiudades.shp")
str(INEIDATA)

INEIDATA<-subset(INEIDATA,NOMBDIST == "ALTO SELVA ALEGRE")
INEIDATA<-subset(INEIDATA,NOMBDIST == "CERRO COLORADO")
INEIDATA<-subset(INEIDATA,NOMBDIST == "MARIANO MELGAR")

data_pob <- read_xlsx("~/Spatial-surveillance/INEI_information_by_block/Dato/Poblacion_2.xlsx") # old
data_pob <- data_pob[,c(1:10,136:146)] 
data_pob<-data_pob %>% rename_all(toupper)
#data_pob= mutate ( data_pob,IDmanzana=paste0(`CÓDIGO DE DEPARTAMENTO`,`CÓDIGO DE PROVINCIA`,`CÓDIGO DE DISTRITO`,`CODIGO DE ZONA`,`CÓDIGO DE MANZANA`,sep = "" ))
data_pob<-data_pob%>% filter(`NOMBRE DE DISTRITO`=="ALTO SELVA ALEGRE")
data_pob<-data_pob%>% filter(`NOMBRE DE DISTRITO`=="CERRO COLORADO")

merge_data_raza<-merge(INEIDATA, data_pob, 
                       by="IDMANZANA",  all.x=T)

  names(merge_data_raza)[25] <- "QUECHUA"
  names(merge_data_raza)[26] <- "AIMARA"
  names(merge_data_raza)[27] <- "AMAZONIA"
  names(merge_data_raza)[28] <- "PINDIGENA"
  names(merge_data_raza)[29] <- "AFRODESCEN"
  names(merge_data_raza)[30] <- "BLANCO"
  names(merge_data_raza)[31] <- "MESTIZO"
  names(merge_data_raza)[32] <- "OTRO"
  names(merge_data_raza)[33] <- "NOSABE"
  names(merge_data_raza)[34] <- "NIKKEI"
  names(merge_data_raza)[35] <- "TUSAN"
  

merge_data_raza<-merge_data_raza[,c(1,25:35,36)]
#### the data has to be in long format ######
long <- merge_data_raza %>% gather(RAZA, value, -c(IDMANZANA, geometry))
long <- long %>%
  dplyr::mutate(colour = case_when(RAZA == "QUECHUA" ~ "#07cc00",
                                   RAZA == "AIMARA" ~ "#8C0315",
                                   RAZA == "AMAZONIA" ~ "#066F6F",
                                   RAZA == "PINDIGENA" ~ "#ff6700",
                                   RAZA == "AFRODESCEN" ~ "#00239f",
                                   RAZA == "BLANCO" ~ "#9f0048",
                                   RAZA == "MESTIZO" ~ "#9f6000",
                                   RAZA == "OTRO" ~ "#ff00db",
                                   RAZA == "NOSABE" ~ "#325600",
                                   RAZA == "NIKKEI"~ "#673ab7",
                                   RAZA == "TUSAN"~ "#ff0000"))

### seetting borders of map #######
map_theme <- theme_void() + 
  theme(plot.title=element_text(face="bold", hjust = 0.5)) + 
  theme(plot.subtitle=element_text(hjust = 0.5)) + 
  theme(plot.caption=element_text(size=8, margin=margin(t=10), hjust = 0.95))+
  theme(legend.position = "right")


### ggplot normal using virdis color 

png("prueba_normal.png", width=1100, height=600)
ggplot(long) + 
  geom_sf(aes(fill = value, colour = value)) + 
  facet_wrap(~RAZA, ncol = 3) + 
  map_theme + scale_fill_viridis_c("", guide = FALSE, option = "magma") + 
  coord_sf(datum = NA) + 
  scale_colour_viridis_c("", guide = FALSE, option = "magma")
dev.off()




#####creando una funcion para cada entrada 
small_map <- function(df, i) {
  sub_df <- df %>% filter(RAZA == race_list[i])
  
  map <- ggplot(sub_df) + 
    geom_sf(aes(fill = value, colour = value)) + 
    map_theme + coord_sf(datum = NA) +
    scale_fill_gradient(space = "Lab", name=" ", low="#F4F4F4", high=unique(sub_df$colour),na.value = '#dbdbdb', guide=FALSE) + labs(title = race_list[i]) +
    scale_colour_gradient(space = "Lab", name=" ", low="#F4F4F4", high=unique(sub_df$colour), na.value = '#dbdbdb',guide=FALSE) + labs(title = race_list[i]) +
    theme(plot.title = element_text(colour = unique(sub_df$colour)))
  return(map)
}
##### iterando sobre las zrazas 
race_list <- unique(long$RAZA)
plotlist <- list() # initialize an empty list to fill
for(i in 1:length(race_list)) {
  plotlist[[i]] <- small_map(long, i)
}

##### saving as png######
png("CC_raceI.png", width=1100, height=600)
do.call("grid.arrange", c(plotlist, ncol=3)) 
dev.off()

