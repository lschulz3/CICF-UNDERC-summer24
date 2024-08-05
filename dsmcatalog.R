
library(lidR)

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

opt_chunk_buffer(ctg) <- 50
opt_output_files(ctg) <- paste0(tempdir(), "/finaldsm_{*}")

output2 <- filter_birds(ctg, sensitivity = 1.2)

dsm <- rasterize_canopy(output2, 1, p2r(0.15))
plot(dsm, col = height.colors(50))


#test on 2 tiles
smallctg <- readLAScatalog("C:/Lisa.LiDAR/2tiles")
opt_chunk_buffer(smallctg) <- 50
opt_output_files(smallctg) <- paste0(tempdir(), "/2dsm_{*}")

output3 <- filter_birds(smallctg, sensitivity = 1.2)

smalldsm <- rasterize_canopy(output3, res = .5, pitfree(subcircle = .15), overwrite = TRUE)
plot(smalldsm, col = height.colors(50))

##test on 9 tiles
tile9ctg <- readLAScatalog("C:/Lisa.LiDAR/smoothingdtm/lasfiles")
opt_chunk_buffer(tile9ctg) <- 50
opt_output_files(tile9ctg) <- paste0(tempdir(), "/9dsm_{*}")

output9 <- filter_birds(tile9ctg, sensitivity = 1.2)

dsm9 <- rasterize_canopy(output9, res = .5, pitfree(subcircle = .15), overwrite = TRUE)
plot(dsm9, col = height.colors(50))




