geocode_mapbox <- function(x) {
  tidygeocoder::geo(
    address = x,
    method = "mapbox"
  )
}

variables <- c(
  "institucion","codigo","nombre","clasificacion",
  "tipo","dep","prov","dist","ubigeo","direccion",
  "codigo_disa","codig_red","codigo_microred","disa",
  "red","microrred","codigo_ue","unidad_ejecutora",
  "categoria","telefono","tipo_categorizacion",
  "nro_categorizacion","horario","inicio_actividad",
  "director","estado","lat","long","cota","camas","ruc",
  "country"
  )

variables2 <- c(
  "institucion","codigo","nombre","clasificacion",
  "tipo","dep","prov","dist","ubigeo","direccion",
  "codigo_disa","codig_red","codigo_microred","disa",
  "red","microrred","codigo_ue","unidad_ejecutora",
  "categoria","telefono","tipo_categorizacion",
  "nro_categorizacion","horario","inicio_actividad",
  "director","estado","lat","long","cota","camas","ruc",
  "count"
)

