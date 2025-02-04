# 1. BIBLIOTECAS
#-------------
install.packages("magick")
install.packages("pacman")
pacman::p_load(
  terra,
  elevatr,
  sf,
  tidyverse,
  rayshader,
  magick,
  scales
)

# 2. CARREGAR SHAPEFILE
#-------------------
shape_path <- "C:/Users/biote/Documents/RSTUDIO/mapas_artisticos/shapes/Maciço de Baturité wgs84.shp"
region_sf <- sf::st_read(shape_path)

plot(sf::st_geometry(region_sf))

# 3. DESCARREGAR E CARREGAR RIOS
#-----------------------------
url <- "https://data.hydrosheds.org/file/HydroRIVERS/HydroRIVERS_v10_sa_shp.zip"
destfile <- basename(url)

if (!file.exists(destfile)) {
  download.file(
    url = url,
    destfile = destfile,
    mode = "wb"
  )
  unzip(destfile)
}

filename <- list.files(
  path = "HydroRIVERS_v10_sa_shp",
  pattern = ".shp",
  full.names = TRUE
)

# Converter o bounding box da região em geometria WKT
region_bbox <- sf::st_as_sfc(sf::st_bbox(region_sf))  # Bounding box como geometria
region_wkt <- sf::st_as_text(region_bbox)             # Converter para WKT

# Filtrar rios diretamente pelo recorte espacial usando wkt_filter
country_rivers <- sf::st_read(
  filename,
  wkt_filter = region_wkt  # Filtra rios dentro do bounding box da região
)

# Realizar a interseção para garantir o recorte exato
country_rivers <- sf::st_intersection(country_rivers, region_sf)

# Plotar os rios para verificação
#plot(sf::st_geometry(country_rivers)) # demora a carregar

# 4. RIVER WIDTH (largura dos rios)
#---------------
country_river_width <- country_rivers |>
  dplyr::mutate(
    width = as.numeric(ORD_FLOW),
    width = dplyr::case_when(
      width == 8 ~ 1.6,  # Rios menores
      width == 7 ~ 1.8,  # Moderados
      width == 6 ~ 2,  # Rios principais (mais largos)
      TRUE ~ 0         # Excluir outros valores, se houver
    )
  ) |>
  sf::st_as_sf()


# 5. DEM
#-------
dem <- elevatr::get_elev_raster(
  locations = region_sf,
  z = 9, clip = "locations"
)

dem_matrix <- rayshader::raster_to_matrix(dem)

# 6. RENDERIZAR CENA
#----------------
dem_matrix |>
  rayshader::height_shade(
    texture = colorRampPalette(
      c("#f2ecc5","#a9604b","#733536", "#491e28")  # Suas cores selecionadas
    )(128)
  ) |>
  rayshader::add_overlay(
    rayshader::generate_line_overlay(
      geometry = country_river_width,
      extent = terra::ext(dem),
      heightmap = dem_matrix,
      color = "#0b0066",  # Cor para rios
      linewidth = country_river_width$width
    ), alphalayer = 0.8
  ) |>
  rayshader::plot_3d(
    dem_matrix,
    zscale = 20,
    solid = FALSE,
    shadow = TRUE,
    background = "white",
    windowsize = c(700, 700),
    zoom = 1,
    phi = 80,
    theta = 5
  )

# 7. RENDERIZAR OBJETO
#-----------------
hdri_url <- "https://dl.polyhaven.org/file/ph-assets/HDRIs/hdr/4k/photo_studio_loft_hall_4k.hdr"
hdri_file <- basename(hdri_url)

if (!file.exists(hdri_file)) {
  download.file(
    url = hdri_url,
    destfile = hdri_file,
    mode = "wb"
  )
}

output_file <- "serra_baturite_3d_elevation_rivers3.png"

rayshader::render_highquality(
  filename = output_file,
  light = FALSE,              # Desabilitar luz artificial
  environment_light = hdri_file,   # Remover HDRI
  intensity_env = 1,
  interactive = FALSE,
  width = 3000,
  height = 3000,
  samples = 300
)


# 8. ANOTAÇÃO DO MAPA
#----------------
map1 <- magick::image_read(output_file)

cols <- rev(c("#276604", "#ddb746", "#ffd3af", "#ffeadb"))

map2 <- magick::image_annotate(
  map1, "Topografia e Rios",
  font = "Goudy Old Style",
  color = alpha("#3B170B", 0.8),
  size = 110, gravity = "north",
  location = "+0+140", weight = 500
)

map3 <- magick::image_annotate(
  map2, "MACIÇO DE BATURITÉ",
  font = "Goudy Old Style",
  color = "#3B170B",
  size = 150, gravity = "north",
  location = "+0+275", weight = 900
)

map4 <- magick::image_annotate(
  map3, "©2025 Lidia Castro",
  font = "Lucida Bright",
  color = alpha("black", 0.5),
  size = 40, gravity = "south",
  location = "+0+144"
)

map5 <- magick::image_annotate(
  map4, "Data: World Wildlife Fund, Inc. (2006 - 2013) HydroSHEDS database | http://www.hydrosheds.org",
  font = "Lucida Bright",
  color = alpha("black", 0.5),
  size = 30, gravity = "south",
  location = "+0+100"
)

output_annotated <- "serra_baturite_3d_elevation_annotated.png"
magick::image_write(
  map5,
  output_annotated
)

# BONUS: SALVAR EM PDF
#-------------------
pdf_file <- "serra_baturite_3d_elevation.pdf"
magick::image_write(
  map5,
  path = pdf_file,
  format = "pdf"
)

message("PDF salvo como: ", pdf_file)
