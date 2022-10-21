library(tidyverse)
library(accessibility)
library(sf)
library(tidyterra)
library(patchwork)
library(r5r)
source("utils.R")

# 1. Reading spatial data ------------------------------------------------
# Villages
cp <- st_read("data/centro_poblado_INEI.gpkg")|> 
  rename(CODCP = IDCCPP_17)
# Health facilities
cs <- st_read("data/personal_salud.gpkg") |> 
  dplyr::select(codigo, Medico)
cs[is.na(cs)] <- 0

# Catchment areas based in a travel time (min) 
catchment <- st_read("catchmean-area/Iquitos.gpkg") |> 
  filter(time == 30) |>  
  dplyr::select(layer,geom)

# Name with ubigeo 
ubigeo_w_name <- read_csv("data/ubigeo_iquitos.csv") |> 
  rename(layer = new_name)
# Ubigeo with cathment areas
newcathment <- catchment |> left_join(y = ubigeo_w_name, by = "layer")

# 2. Count villages and health facilities ---------------------------------
# count cp 
newcathment$ncp <- lengths(st_intersects(newcathment,cp))
# count cs 
id_medicos <- st_contains(newcathment,cs)
newcathment$nmedicos <- lapply(1:length(id_medicos),FUN = conteo) |> 
  unlist()
# new database for models
newcathment_model <- newcathment |> st_centroid() |> 
  mutate(lat = st_coordinates(geom)[,2],
         lon = st_coordinates(geom)[,1],
         CODCP = as.character(IDCCPP_17)) |>  
  rename(id = CODCP) |> 
  dplyr::select(id,lat,lon,ncp,nmedicos) |> 
  st_drop_geometry()

# 3. Calculate a travel time matrix -------------------
# set inputs: # TRANSIT" todos los transporte público disponibles.
data_path <- "Iquitos/pbf"
r5r_core <- r5r::setup_r5(
  data_path = data_path,
  verbose = FALSE
)
mode <- c("WALK", "TRANSIT") 
max_walk_dist <- 5000
max_trip_duration <- 120
departure_datetime <- as.POSIXct(
  "13-05-2017 7:00:00",
  format = "%d-%m-%Y %H:%M:%S"
)

ttm <- travel_time_matrix(
  r5r_core = r5r_core,
  origins = newcathment_model ,
  destinations = newcathment_model,
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
  newcathment_model,
  opportunity = "nmedicos",
  travel_cost = "travel_time",
  decay_function = decay_exponential(decay_value = 0.2)
) %>% 
  rename(gravity_model = nmedicos)

# 5. FCA Models ----------------------------------
bfca_model <- floating_catchment_area(
  ttm,
  newcathment_model,
  opportunity = "nmedicos",
  travel_cost = "travel_time",
  demand = "ncp",
  method = "bfca",
  decay_function = decay_exponential(decay_value = 0.2)
) %>% 
  rename(bfca = nmedicos)

fca_model <- floating_catchment_area(
  ttm,
  newcathment_model,
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
m1 <- mapping_iquitos(
  data = access_modelos,
  modelo = "gravity_model",
  name_legend = "Gravity model"
)

m2 <- mapping_iquitos(
  data = access_modelos,
  modelo = "bfca",
  name_legend = "BFCA model"
)

m3 <- mapping_iquitos(
  data = access_modelos,
  modelo = "sfca_2",
  name_legend = "2SFCA model"
)

m1 | m2 | m3 

ggsave(
  filename = "plots/iquitos-accessibility-30min.png",
  plot = last_plot(),
  width = 11,
  height = 5,
  bg = "white"
)

# 8. Export value of accessibility ----------------------------------------
write_csv(
  access_modelos |> st_drop_geometry(),
  "accessibility_index/iquitos_access_models_30min.csv"
  )