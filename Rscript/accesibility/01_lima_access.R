library(tidyverse)
library(accessibility)
library(r5r)
library(sf)
library(here)
library(gstat)
library(automap)
library(tidyterra)
library(terra)
library(raster)
library(ggspatial)
library(cptcity)
library(showtext)
library(patchwork)
source("utils.R")
options(java.parameters = "-Xmx8G")
dir.create(here::here("Lima/pbf"))
font_add_google("Bebas Neue", "bebas")
showtext_auto()
# 1. Configure of the calculate of travel time ---------------------------
r5r_core <- r5r::setup_r5(
  data_path = "Lima/pbf",
  verbose = FALSE
)
# 2. Reading spatial data ------------------------------------------------
# cp - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
cp <- st_read("DataBase/centro_poblado_INEI.gpkg")|> 
  filter(DEPARTAMEN == "LIMA" & PROVINCIA %in% c("LIMA","CALLAO")) |> 
  rename(CODCP = IDCCPP_17)
# cs - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
cs <- st_read("DataBase/personal_salud.gpkg") |> 
  filter(dep == "LIMA" & prov %in% c("LIMA","CALLAO")) |> 
  dplyr::select(codigo, Medico)
cs[is.na(cs)] <- 0

# buffer - - - - - - - - - - - - - - - - - - - - - - - - - -
catchment <- cp |>
  st_transform(crs = 32718) |> 
  st_buffer(dist = 5000) |> 
  st_transform(crs = 4326) |> 
  dplyr::select(CODCP)
# count cp - - - - - - - - - - - - - - - - - - - - - - - - - -
catchment$ncp <- lengths(st_intersects(catchment,cp))
# count cs - - - - - - - - - - - - - - - - - - - - - - - - - -
id_medicos <- st_contains(catchment,cs)
catchment$nmedicos <- lapply(1:length(id_medicos),FUN = conteo) |> 
  unlist()

catchment <- catchment |> st_centroid() |> 
  mutate(lat = st_coordinates(geom)[,2],
         lon = st_coordinates(geom)[,1],
         CODCP = as.character(CODCP)) |>  
  rename(id = CODCP) |> 
  dplyr::select(id,lat,lon,ncp,nmedicos) |> 
  st_drop_geometry()

# 3. Calculate a travel time matrix -------------------
# set inputs: # TRANSIT" todos los transporte público disponibles.
mode <- c("WALK", "TRANSIT") 
max_walk_dist <- 5000
max_trip_duration <- 120
departure_datetime <- as.POSIXct(
  "13-05-2017 7:00:00",
  format = "%d-%m-%Y %H:%M:%S"
  )

ttm <- travel_time_matrix(
  r5r_core = r5r_core,
  origins = catchment ,
  destinations = catchment,
  mode = mode,
  departure_datetime = departure_datetime,
  max_trip_duration = max_trip_duration,
  verbose = FALSE
  ) |> 
  rename(
    from_id = fromId ,
    to_id = toId
    )

# 4. Gravity model -------------------------------------------------
gravity_model <- gravity(
  ttm,
  catchment,
  opportunity = "nmedicos",
  travel_cost = "travel_time",
  decay_function = decay_exponential(decay_value = 0.2)
) %>% 
  rename(gravity_model = nmedicos)

# 5. FCA Models ----------------------------------
bfca_model <- floating_catchment_area(
  ttm,
  catchment,
  opportunity = "nmedicos",
  travel_cost = "travel_time",
  demand = "ncp",
  method = "bfca",
  decay_function = decay_exponential(decay_value = 0.2)
) %>% 
  rename(bfca = nmedicos)

fca_model <- floating_catchment_area(
  ttm,
  catchment,
  opportunity = "nmedicos",
  travel_cost = "travel_time",
  demand = "ncp",
  method = "2sfca",
  decay_function = decay_binary(cutoff = 20)
) %>% 
  rename(sfca_2 = nmedicos)

# 6. models ---------------------------------------------------------------
access_modelos  <- cp |>  
  dplyr::rename(id = CODCP) |>  
  dplyr::select(id) |> 
  merge(
    gravity_model,
    by = "id"
  )

access_modelos <- access_modelos |> 
  merge(
    bfca_model,
    by = "id"
  )

access_modelos <- access_modelos |>   
  merge(
    fca_model,
    by = "id"
  )

# 7. plots of models --------------------------------------------------------
m1 <- mapping_lima(
  data = access_modelos,
  modelo = "gravity_model",
  name_legend = "Gravity model"
  )

m2 <- mapping_lima(
  data = access_modelos,
  modelo = "bfca",
  name_legend = "BFCA model"
  )

m3 <- mapping_lima(
  data = access_modelos,
  modelo = "sfca_2",
  name_legend = "2SFCA model"
  )

m1 | m2 | m3 

ggsave(
  filename = "Lima/png/lima_modelos.png",
  plot = last_plot(),
  width = 11,
  height = 5,
  bg = "white"
  )
write_csv(access_modelos |> st_drop_geometry(),"lima_access_models.csv")
# 8. new raster of accesibility ---------------------------------------------
lima <- st_read("https://github.com/ambarja/gpkg-pe/raw/main/lima_distritos.gpkg")

variograma <- variogram(
  gravity_model ~ 1,
  access_modelos,
  cressie = F
  )
model_vario <- fit.variogram(variograma,vgm(c("Sph","Gau","Exp")))
plot(variograma,model_vario)
grilla <- st_read("Lima/resources/lima_grid.gpkg")
ko <- krige(gravity_model ~ 1,access_modelos,st_centroid(grilla),model_vario)
grilla$ko <- ko$var1.pred

index_gm <- st_rasterize(grilla %>% dplyr::select(ko, geom)) |> 
  rast() |> 
  mask(lima)

ggplot() +
  geom_spatraster(data = index_gm) +
  scale_fill_whitebox_c() +
  coord_sf(expand = FALSE) +
  labs(fill = "Gravity model") + 
  theme_minimal()
