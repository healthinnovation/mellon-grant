library(innovar)
library(tidyverse)
library(sf)
# library(tricolore)
# Database
data(Peru)
areas_list <- Peru %>% st_set_geometry(NULL) %>%
  select(ubigeo,dep.code:distr) 

# Nombres de los departamentos y provincias
peru_prov_shp <- Peru %>% group_by(prov) %>% summarise() 
peru_dep_shp <- Peru %>% group_by(dep) %>% summarise() 
dep_list <- c("TODOS",sort((unique(areas_list$dep))))

names(dep_list) <- c("Todos los departamentos",
                          str_to_title(sort(unique(areas_list$dep))))

# Mapa de tres colores
# 
# data <- st_read("data/example.gpkg")
# legenda <- data %>% 
#   Tricolore(p1 = "quechua_1", p2 ="aimara_1", p3 ="nativo_1")
# data$rgb <- legenda$rgb
# write_sf(data,'data/example.gpkg')

data <- st_read("data/example.gpkg")





