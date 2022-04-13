library(tidyverse)
library(tidygeocoder)
library(sf)
library(innovar)
library(viridis)
source("utils.R")
source("https://raw.githubusercontent.com/ambarja/utilsR/main/remove_accent_enie.R")
data("Peru")

# 1. Reading spatial data -------------------------------------------------
susalud <- readxl::read_xls("raw/SUSALUD/USLRC20220316224445_xp.xls") %>% 
  mutate(country = "PERU") %>% 
  mutate_if(is.character,rm_accent)
names(susalud) <- variables

ss_geo <- susalud %>% 
  filter(!is.na(lat))

##> Geocoding
ss_noge <- susalud %>%
  filter(is.na(lat)) %>% 
  mutate(direccion = paste0(
    direccion,",",
    dist,",",
    prov,",",
    dep,",",
    country)
    ) %>% 
  select(-c(lat,long))

lista <- list()
for(i in 1:nrow(susalud)){
  a <- ss_noge[i,]
  b <- geocode_mapbox(ss_noge$direccion[[i]]) %>% 
    rename(direccion = address,
           )
  c <- left_join(a,b,"direccion")
  lista[[i]] <- c
}

# lista
# saveRDS(lista,'lista_datos_nogeo_geo.rds')
dato_encontrado <- lista %>% 
  map_df(.f = as.data.frame)

ss_noge <- susalud %>%
  filter(is.na(lat)) %>% 
  mutate(direccion = paste0(
    direccion,",",
    dist,",",
    prov,",",
    dep,",",
    country)
  ) %>% 
  select(institucion:estado,cota,camas,ruc,country,lat,long)

total_datos <- left_join(
  ss_noge,
  dato_encontrado,
  by = c("codigo")
)

names(total_datos)

##> Clean data
newdata <- total_datos %>%
  rename_at(
  vars(ends_with(".y")), function(x) { gsub(".y","",x) }
  ) %>% 
  select(variables2)


##> Export Healthcare in a gpkg format

nogeo_to_geo <- newdata %>% 
  filter(!is.na(lat)) %>% 
  rename(country = count)

# names(ss_geo) == names(nogeo_to_geo)
cs_geo <- rbind(ss_geo,nogeo_to_geo) %>% 
  st_as_sf(coords = c("long","lat"),crs = 4326)
plot(st_geometry(cs_geo))
write_sf(cs_geo,"centros_salud.gpkg")

##> Missing data
missing_data <- newdata %>%
  group_by(dep,prov,dist) %>% 
  summarise(
    noge = sum(table(dist)),
    geo  = sum(!is.na(lat)),
    missing = sum(is.na(lat))
  ) %>% 
  mutate(
    perct_missig = missing*100/noge
  ) %>% 
  rename(distr = dist)

missing_data %>% 
  drop_na(dep) %>% 
  arrange(desc(perct_missig)) %>% 
  write_csv("missing_data.csv")
  
# 2. Final maps ---------------------------------------------------
data_map <- Peru %>% 
  left_join(
    missing_data,
    by = c("dep","prov","distr")
  )
names(data_map)
data_dist <- data_map %>% 
  mutate(perct_missig = ifelse(is.na(perct_missig), 0, perct_missig))
  
dist <- ggplot() +
  geom_sf(data = Peru,aes(fill = NULL),lwd = 0.01,color = "black") +
  geom_sf(
    data = data_dist,
    aes(fill = perct_missig),
    lwd = 0.05,
    color = "white"
    ) +
  scale_fill_viridis("% missing data",option = "plasma") + 
  theme_minimal()

data_prov <- data_map %>% 
  group_by(prov) %>% 
  summarise(perct_missig = sum(perct_missig,na.rm = TRUE))

prov <- ggplot() +
  geom_sf(data = Peru,aes(fill = NULL),color = "white") +
  geom_sf(
    data = data_prov,
    aes(fill = perct_missig),
    lwd = 0.05,
    color = "white"
  ) +
  scale_fill_viridis("% missing data",option = "plasma") + 
  theme_minimal()


data_dep <- data_map %>% 
  group_by(dep) %>% 
  summarise(perct_missig = sum(perct_missig,na.rm = TRUE))

dep <- ggplot() +
  geom_sf(data = Peru,aes(fill = NULL),lwd = 0.01,color = "black") +
  geom_sf(
    data = data_dep,
    aes(fill = perct_missig),
    lwd = 0.05
  ) +
  scale_fill_viridis("% missing data",option = "plasma") + 
  theme_minimal()

ggsave(
  filename = "missing_data_dep.png",
  plot = dep,
  width = 5,
  height = 5,
  bg = "white"
  )

  


