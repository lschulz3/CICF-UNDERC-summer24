library(lidR)
install.packages("whitebox", repos="http://R-Forge.R-project.org")
install.packages("rayshader")

install.packages("rgdal", type = "source")

library(tidyverse)
library(raster)
library(sf)
library(whitebox)
library(tmap)
library(stars)
library(rayshader)
library(rgl)

whitebox::wbt_init()

knitr::knit_hooks$set(webgl = hook_webgl)

theme_set(theme_classic())

tmap_mode("view")

##reading in the DTM and plotting it

dem <- raster("waterdel/writtenraster.tif")

writeRaster(dem, "waterdel/writtenraster_crs.tif", overwrite = TRUE)

dem[dem < 1500] <- NA

tm_shape(dem)+
  tm_raster( style = "cont", palette = "PuOr", legend.show = TRUE)+
  tm_scale_bar()

##generating a hillshade and plotting it

wbt_hillshade(dem = "waterdel/writtenraster_crs.tif",
              output = "waterdel/brush_hillshade.tif",
              azimuth = 115)

hillshade <- raster("waterdel/brush_hillshade.tif")

tm_shape(hillshade)+
  tm_raster(style = "cont",palette = "-Greys", legend.show = FALSE)+
  tm_scale_bar()

##Filling pits and breaching depressions

wbt_fill_single_cell_pits(
                    dem = "waterdel/writtenraster_crs.tif",
                   output = "waterdel/bmstationdem_filled.tif")

wbt_breach_depressions_least_cost(
  dem = "waterdel/writtenraster_crs.tif",
  output = "waterdel/bmstationdem_breached.tif",
  dist = 5,
  fill = TRUE)

wbt_fill_depressions_wang_and_liu(
  dem = "waterdel/bmstationdem_breached.tif",
  output = "waterdel/bmstationdem_filled_breached.tif"
)

##creating D8 flow accumilation grid and pointer grid

wbt_d8_flow_accumulation(input = "waterdel/bmstationdem_filled_breached.tif",
                         output = "waterdel/D8FA.tif")

wbt_d8_pointer(dem = "waterdel/bmstationdem_filled_breached.tif",
               output = "waterdel/D8pointer.tif")


##setting pour points

ppoints <- tribble(
  ~East, ~North,
  306822, 5124136, ##306924.3, 5124054, ## 1 Hummingbird Lake
  309205.9, 5120561, ## 2 Crampton Lake
  305601, 5125902, ## 3 Morris Lake
  307372.9, 5123303, ## 4 West_long
  307464.7, 5123228, ## 5 East_long
  308203, 5123722, ## 6 Bay Lake 
  308632, 5121270, ## 7 Brown Lake
  307699, 5122687, ## 8 Bolger Lake
  305852.8, 5125424, ## 9 Ward Lake
  301901, 5123089, ## 10 Cranberry Lake (north)
  301792, 5123032, ## 11 Cranberry Lake (south)
  ##301830.6, 5123088, ##Cranberry Lake (middle)
  306974.7, 5125064, ## 12 Paul Lake
  307057.9, 5125281, ## 13 Peter Lake
  305070, 5122276, ## 14 Tenderfoot Lake/Creek
  306789, 5120339, ## 15 Palmer Lake (north)
  307050, 5121843, ## 16 Plum Lake
  306218, 5124681, ## 17 Bergner Lake
  308166, 5119831, ## 18 Palmer Lake (east)
  305382, 5120629 ## 19 Tenderfoot Lake (south)

)

ppointsSP <- SpatialPoints(ppoints, proj4string = CRS('+proj=utm +zone=16 +datum=WGS84'))

shapefile(ppointsSP, filename = "waterdel/pourpoints.shp", overwrite = TRUE)

##extracting and plotting streams

wbt_extract_streams(flow_accum = "waterdel/D8FA.tif",
                    output = "waterdel/raster_streams.tif",
                    threshold = 6000)

wbt_jenson_snap_pour_points(pour_pts = "waterdel/pourpoints.shp",
                            streams = "waterdel/raster_streams.tif",
                            output = "waterdel/snappedpp.shp",
                            snap_dist = 50) 

pp <- shapefile("waterdel/snappedpp.shp")
streams <- raster("waterdel/raster_streams.tif")

tm_shape(streams)+
  tm_raster(legend.show = TRUE, palette = "Blues")+
  tm_shape(pp)+
  tm_dots(col = "red")

##Delineating watersheds!

wbt_watershed(d8_pntr = "waterdel/D8pointer.tif",
              pour_pts = "waterdel/snappedpp.shp",
              output = "waterdel/brush_watersheds.tif")

ws <- raster("waterdel/brush_watersheds.tif")

tm_shape(hillshade)+
  tm_raster(style = "cont",palette = "-Greys", legend.show = FALSE)+
  tm_shape(ws)+
  tm_raster(legend.show = TRUE, alpha = 0.5, style = "cat")+
  tm_shape(pp)+
  tm_dots(col = "red")

##converting watersheds to shape files

wsshape <- st_as_stars(ws) %>% st_as_sf(merge = T)

ws1shp <- wsshape %>% filter(brush_watersheds == "1")

tm_shape(hillshade)+
  tm_raster(style = "cont",palette = "-Greys", legend.show = FALSE)+
  tm_shape(ws1shp)+
  tm_borders(col = "red")

