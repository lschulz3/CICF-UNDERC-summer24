setwd("C:/Lisa.LiDAR/smoothingdtm")

library(lidR)


##using LAScatalog on all files

filter_birds = function(las, sensitivity)
{
  if (is(las, "LAS"))
  {
    p99 <- pixel_metrics(las, ~quantile(Z, probs = 0.01), 10)
    las <- merge_spatial(las, p99, "p99")
    las <- filter_poi(las, Z > quantile(las$Z, 0.01))
    las$p99 <- NULL
    return(las)
  }
  
  if (is(las, "LAScatalog"))
  {
    options <- list(
      need_output_file = TRUE,    # Throw an error if no output template is provided
      need_buffer = TRUE)         # Throw an error if buffer is 0
    res <- catalog_map(las, filter_birds, sensitivity = sensitivity, .options = options)
    return(res)
  }
}

filter_noise = function(las, sensitivity)
{
  if (is(las, "LAS"))
  {
    p95 <- pixel_metrics(las, ~quantile(Z, probs = 0.95), 10)
    las <- merge_spatial(las, p95, "p95")
    las <- filter_poi(las, Z < p95*sensitivity)
    las$p95 <- NULL
    return(las)
  }
  
  if (is(las, "LAScatalog"))
  {
    options <- list(
      need_output_file = TRUE,    # Throw an error if no output template is provided
      need_buffer = TRUE)         # Throw an error if buffer is 0
    res <- catalog_map(las, filter_noise, sensitivity = sensitivity, .options = options)
    return(res)
  }
}

##modifying buffers and creating unfiltered dtm portion

opt_chunk_buffer(ctg) <- 50

opt_output_files(ctg) <- opt_output_files(ctg) <- paste0(tempdir(), "/{*}_dtm")

##filtering data and creating dtm
output <- filter_noise(ctg, tolerance = 1.2)
output2 <- filter_birds(ctg, sensitivity = 1.2)

dtm <- rasterize_terrain(output2, 1, kriging())
plot(dtm)
plot_dtm3d(dtm, bg = "white")


##final smoothing of raster
w = matrix(1,49,49)

smoothed.raster = terra::focal(dtm, w, fun = mean, na.rm = TRUE)
plot(smoothed.raster)
plot(dtm)







