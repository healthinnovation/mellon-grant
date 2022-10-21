##  Travel time to school facilities as a marker of geographical      ## 
##         accessibility across heterogeneous land cov                ##
##' @GabrielCarrasco-Escobar, @EdgarManrique, @KellyTello-Lizarraga,  ##
##'              @J.JaimeMiranda,@AntonyBarja                         ##
##'-------------------------------------------------------------------##

# Requerements
library(tidyverse)
library(rgee)
library(cptcity)
ee_Initialize()

# Preparing dataset
peru  <- ee$FeatureCollection('users/edgarmanrique30/Peru_geometry/Limite_departamental')
dem   <- ee$Image('USGS/SRTMGL1_003') 
landc <- ee$ImageCollection('MODIS/006/MCD12Q1')
vias_dep <- ee$FeatureCollection('users/edgarmanrique30/Peru_geometry/red_vial_departamental_dic16')
vias_nac <- ee$FeatureCollection('users/edgarmanrique30/Peru_geometry/red_vial_nacional_dic16')
vias_vec <- ee$FeatureCollection('users/edgarmanrique30/Peru_geometry/red_vial_vecinal_dic16')
rios  <- ee$Image('WWF/HydroSHEDS/15ACC')
anp   <- ee$FeatureCollection('users/edgarmanrique30/Peru_geometry/ANP-Nacional')
inputPoints <- ee$FeatureCollection('users/edgarmanrique30/Peru_geometry/eess_20190319')

# Construccion our friction surface 
# Topographyc variables
dem <- dem$clip(peru)
slope <- ee$Terrain$slope(dem)

# LandCover for Peru 
landc <- landc$select("LC_Type1")$
  filterDate("2017-01-01","2017-12-31")$
  median()$
  clip(peru)
# Hydrology variables
peru_rios <- peru$
  filter(ee$Filter$inList('NOMBDEP',c("LORETO", "MADRE DE DIOS", "UCAYALI")))
rios <- rios$gt(5000) 
rios <- rios$remap(c(0,1),c(0,9))
black <- ee$Image(0)$byte()
vias_nac <- black$paint(vias_nac, 80)$clip(peru$geometry())
vias_dep <- black$paint(vias_dep, 50)$clip(peru$geometry())
vias_vec <- black$paint(vias_vec, 30)$clip(peru$geometry())
# LC_Type1, Remapping the pixel values of each category of land cover to their respective speed in km/h.
landcspeed <- landc$
  remap(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13 ,14, 15, 16, 17),
        c(3.24, 1.62, 3.24, 4, 3.24, 3, 4.2, 4.86, 4.86, 4.86, 2, 2.5, 5, 3.24, 1.62, 3, 1))
# Filtering urban areas to multiply the roads speed by .7
landc_urban <- landc$eq(13)
# Filtering urban areas to multiply the roads speed by .7
landc_urban <- landc_urban$remap(c(0,1),c(1,0.7)) 
vias_nac <- vias_nac$multiply(landc_urban) # Multiplying roads layers by 0.7 in urban areas
vias_dep <- vias_dep$multiply(landc_urban) # Multiplying roads layers by 0.7 in urban areas
vias_vec <- vias_vec$multiply(landc_urban) # Multiplying roads layers by 0.7 in urban areas

# Creating Natural protected areas layer
black <- ee$Image(0)$byte()
anp <- black$paint(anp, 1)$clip(peru$geometry()) 
#Remapping values to 0.2 km/h of Natural protected areas to multiply Landcover speed
anp <- anp$remap(list(0,1),list(1,0.2)) 
landcspeed <- landcspeed$multiply(anp) # Multiplying Landcover speed by 0.2 on Natural protected areas
landcspeed <- landcspeed$toDouble()$select(list(0),list("speed"))
rios <- rios$toDouble()$select(list(0),list("speed"))
vias_nac <- vias_nac$toDouble()$select(list(0),list("speed")) # unifying the band name
vias_dep <- vias_dep$toDouble()$select(list(0),list("speed")) # unifying the band name
vias_vec <- vias_vec$toDouble()$select(list(0),list("speed")) # unifying the band name

# Mergging all layers into a collection
collection <- ee$ImageCollection(
  list(landcspeed,
       rios,
       vias_nac,
       vias_dep,
       vias_vec
  )
) 
fsurface <- collection$max() # Calculating the maximum value of speed on a single pixel
# eaf <- function(x) {1.01*exp(-0.0001072*x)} # Elevation adjustment factor
eaf <- dem$expression(
  c('1.01*exp(-0.0001072*DEM)'),list(
    'DEM'= dem$select('elevation')))

# thf <- function(x) {6*exp(-3.5*abs((tan(x/57.296) + 0.05)))/5} # Tobler's hikking function adjustment
thf <- slope$expression(
  c('6*exp(-3.5*abs((tan(slope/57.296) + 0.05)))/5'), list(
    'slope'= slope$select(list(0))
  ))
# Adjusting the friction surface by EAF and THF
fsurface <- fsurface$multiply(eaf)$multiply(thf)
# convert <- function(x) {(x * 1000 / 60) ^ -1} # converts km/h to min/m
fsurface <- fsurface$expression(
  c('(x * 1000 / 60) ** -1'),
  list('x'= fsurface$select(list(0)))
)

# Paint the input points, essentially converting them to a raster.
# Theoretically this will merge any points that fall within the same pixel (of the resulting 30-arc-second resolution).
black <- ee$Image(0)$byte()
sources <- black$paint(inputPoints, 1)
sources <- sources$updateMask(sources)
# Compute the min cost path distance map, with a horizon of 1500 km.
# This can be reduced for high-latitude areas and/or to shorten processing time.
distance <- fsurface$cumulativeCost(sources, 400000)  # The function accepts meters rather than km.
distance <- distance$toInt() # Here we convert the output to integer to make the output .tif smaller (and the code more likely to run successfully).
distance <- distance$clip(peru)
Map$addLayer(distance)

# Final Map
viz <- list(
  min = 0,
  max = 600,
  palette = c("#dd554b","#e2b43f","#dbda92","#60b27f","#4866c9")
)

Map$addLayer(distance,viz,'accessibility') + 
  Map$addLayer(fsurface$clip(peru),viz,'friction')