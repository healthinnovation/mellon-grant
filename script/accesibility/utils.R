# Conteo de puntos dentro de poligono
conteo <- function(x){
  suma <- sum(cs[id_medicos[[x]],]$Medico)
  return(suma)
}

# Mapas para lima 
mapping_lima <- function(data, modelo,name_legend = "Gravity model"){
  lima_shp <- st_read(
    "https://github.com/healthinnovation/sdb-gpkg/raw/main/lima_distritos.gpkg"
  )
  ggplot() + 
    geom_sf(
      data = lima_shp,
      lwd = 0.1,
      color = "black",
      alpha = 0
    ) +
    geom_sf(
      data = data,
      aes(color = .data[[modelo]]),
      size = 2.0,
      alpha = 0.7
    ) + 
    scale_color_gradientn(
      name_legend,
      colours = cpt(
        pal = "jjg_ccolo_alpen_natural_light")
    ) +
    theme_minimal() + 
    annotation_north_arrow(
      location = "tl",
      height = unit(0.6,"cm"),
      width = unit(0.6,"cm"),
      style = north_arrow_orienteering(
        line_width = 0.1,
        line_col = "black",
        fill = c("white", "black"),
        text_col = "black",
        text_size = 6,
        text_angle = 0)
    ) + 
    annotation_scale(
      line_width = 0.05,
      height = unit(0.1,"cm"),
      text_face = 6
    ) +
    labs(title = name_legend) + 
    theme(
      plot.title = element_text(
        family = "bebas",
        face = "bold",
        size = 12,
        hjust = 0.5),
      legend.title = element_text(
        family = "bebas",
        size = 6),
      legend.text =  element_text(
        size = 12),
      axis.text = element_text(size = 5)
      )
}


# Mapas para iquitos
mapping_iquitos <- function(data, modelo,name_legend = "Gravity model"){
  iquitos_shp <- st_read(
    "Iquitos/resources/iquitos_polygon.gpkg"
  )
  ggplot() + 
    geom_sf(
      data = iquitos_shp,
      lwd = 0.1,
      color = "black",
      alpha = 0
    ) +
    geom_sf(
      data = data,
      aes(color = .data[[modelo]]),
      size = 2.0,
      alpha = 0.7
    ) + 
    scale_color_gradientn(
      name_legend,
      colours = cpt(
        pal = "jjg_ccolo_alpen_natural_light")
    ) +
    theme_minimal() + 
    annotation_north_arrow(
      location = "tl",
      height = unit(0.6,"cm"),
      width = unit(0.6,"cm"),
      style = north_arrow_orienteering(
        line_width = 0.1,
        line_col = "black",
        fill = c("white", "black"),
        text_col = "black",
        text_size = 6,
        text_angle = 0)
    ) + 
    annotation_scale(
      line_width = 0.05,
      height = unit(0.1,"cm"),
      text_face = 6 
    ) +
    labs(title = name_legend) + 
    theme(
      plot.title = element_text(
        family = "bebas",
        face = "bold",
        size = 12,
        hjust = 0.5),
      legend.title = element_text(
        family = "bebas",
        size = 6),
      legend.text =  element_text(
        size = 12),
      axis.text = element_text(size = 5)
    )
}

# Mapas para Arequipa

mapping_arequipa <- function(data, modelo,name_legend = "Gravity model"){
  arequipa_shp <- st_read(
    "Arequipa/resources/arequipa_poly.gpkg"
  )
  ggplot() + 
    geom_sf(
      data = arequipa_shp,
      lwd = 0.1,
      color = "black",
      alpha = 0
    ) +
    geom_sf(
      data = data,
      aes(color = .data[[modelo]]),
      size = 2.0,
      alpha = 0.7
    ) + 
    scale_color_gradientn(
      name_legend,
      colours = cpt(
        pal = "jjg_ccolo_alpen_natural_light")
    ) +
    theme_minimal() + 
    annotation_north_arrow(
      location = "tl",
      height = unit(0.6,"cm"),
      width = unit(0.6,"cm"),
      style = north_arrow_orienteering(
        line_width = 0.1,
        line_col = "black",
        fill = c("white", "black"),
        text_col = "black",
        text_size = 6,
        text_angle = 0)
    ) + 
    annotation_scale(
      line_width = 0.05,
      height = unit(0.1,"cm"),
      text_face = 6 
    ) +
    labs(title = name_legend) + 
    theme(
      plot.title = element_text(
        family = "bebas",
        face = "bold",
        size = 12,
        hjust = 0.5),
      legend.title = element_text(
        family = "bebas",
        size = 6),
      legend.text =  element_text(
        size = 12),
      axis.text = element_text(size = 5)
    )
}
