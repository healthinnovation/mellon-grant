cp  <- st_read("Lima/resources/centros_poblados.gpkg") %>% 
  dplyr::select(IDCCPP_17,NOMCCPP_17)
cp$new_name <- cp |> 
  select(NOMCCPP_17) |> 
  st_drop_geometry() |>
  pull() %>% 
  gsub(" ","",.) %>% 
  gsub(" ","",.)

write_csv(
  cp %>% st_set_geometry(NULL) |> select(IDCCPP_17,new_name),
  "data/ubigeo_lima.csv"
  )