
library(MFEUtilities)
library(deSolve)
library(prism)
library(raster)
library(tidyr)
library(tibble)
library(lubridate)
library(sf)
library(dplyr)

#lake characteristics for all lakes in MFE UNDERC
get_lake_chars2 <- function(){
  
  dbdir <- "~/Desktop/underc24/hydrology_model"
  db <- "MFEdb.db"
  setwd(dbdir)
  
  DTM = raster("/Users/lisa/Downloads/no_NA_vals_dtm_catalog.tif")
  
  lakes <- dbTable("LAKES")
  lakes <- drop_na(lakes, lat)
  lakes <- lakes %>% filter(city == "UNDERC")
  lake_coords <- data.frame(x = lakes$long, y = lakes$lat)
  lake_coords <- na.omit(lake_coords)
  lake_pointSF <- st_as_sf(lake_coords, coords = c("x","y"),crs=4030)
  lake_pointSFutm_vect <- st_transform(lake_pointSF,crs(DTM))
  terrain_heights <- terra::extract(DTM,lake_pointSFutm_vect)
  extracted_heights <- data_frame(lake_id = lakes$lakeID, lake_surface_elevation = terrain_heights)
  
  MFE_lakes <- dbTable("LAKES")
  
  lake_depth = c()
  lake_surface_area = c()
  
  #getting lake depths and surface areas
  for (i in 1:length(extracted_heights$lake_id)){
    for (j in 1:length(MFE_lakes$lakeID)){
      if (extracted_heights$lake_id[i] == MFE_lakes$lakeID[j]){
        lake_depth = c(lake_depth, MFE_lakes$maxDepth[j])
        lake_surface_area = c(lake_surface_area, MFE_lakes$surfaceArea[j] * 10000)
      }}}
  
  final <- data_frame(extracted_heights, lake_depth, lake_surface_area)
  final <- final[apply(final != 0, 1, all),] #removing rows that have 0 as surface area or depth
  final <- na.omit(final) #removing rows with na values
  ##note that there are some lakes with depth/surface area 0 that have not been removed
  final <- final[order(final$lake_surface_elevation, decreasing = TRUE),] #reordering lakes so the ones with the highest elevation come first
  
  lake_byth <- dbTable("LAKE_BATHYMETRY")
  i = 1
  lake_names = c(lake_byth$lakeID[1])
  current_lake = lake_byth$lakeID[1]
  depths = c()
  current_depth = c()
  while (i < length(lake_byth$lakeID)){
    lake_row = lake_byth$lakeID[i]
    if (current_lake != lake_row){
      current_lake = lake_row
      lake_names = c(lake_names, current_lake)
      depths = c(depths, mean(current_depth, na.rm = TRUE))
      current_depth = c()
    }
    current_depth = c(current_depth, as.numeric(lake_byth$depth_m[i]))
    i = i + 1
  }
  depths = c(depths, mean(current_depth, na.rm = TRUE))
  df_avg_depths <- data_frame(lake_id = lake_names, avg_depth = depths)
  in_order = rep(NA, length = length(final$lake_id))
  for (i in 1:length(final$lake_id)){
    for (j in 1:length(df_avg_depths$lake_id)){
      if (final$lake_id[i] == df_avg_depths$lake_id[j]){
        in_order[i] = df_avg_depths$avg_depth[j]
      }
  }
  }  
  final <- data_frame(final, avg_depth = in_order)
  final <- na.omit(final)
  
  ##getting catchment area and perimeters data
  catchements_perimeter_file <- read.csv("/Users/Lisa/Downloads/lake_info_from_arcgis.csv")
  catchment_area <- catchements_perimeter_file$catchment_area_corrected
  perimeter <- catchements_perimeter_file$perimeter
  arc_gis_SA <- catchements_perimeter_file$surface_area_arcgis
  
  final <- data_frame(final, arc_gis_SA, catchment_area, perimeter)
  
  return(final)
  }

#wind speed
  
sensor_dbdir <- "~/Desktop/underc24/hydrology_model"
sensor_db <- "MFEsensordb.db"
  
Met <- sensordbTable("HOBO_METSTATION_CORR")
  
Met_sorted <- Met %>% arrange(Met$dateTime)
Met_sorted[Met_sorted == 0] <- NA
Met_sorted[Met_sorted == 0.1256147] <- NA

i = 1
dates = c()
current_date = NA
winds = c()
current_wind = c()
while (i < length(Met_sorted$dateTime)){
  current_wind = c(current_wind, Met_sorted$cleanedWindSpeed_m_s[i])
  date_of_row = substr(Met_sorted$dateTime[i],1,10)
  if (is.na(current_date) | current_date != date_of_row){
    current_date = date_of_row
    dates = c(dates, current_date)
    winds = c(winds, mean(current_wind, na.rm = TRUE))
    current_wind = c()
    }
    i = i + 1
  }
  
df <- data_frame(dates, winds)
df$dates <- ymd(df$dates)
  
date_range <- seq(as.Date("2002-08-26"), as.Date("2022-09-25"), by = "days")
missing_dates <- date_range[!date_range %in% df$dates]
na_list <- rep(NA, length = length(missing_dates))

winds <- c(winds, na_list)
dates <- c(dates, as.character(missing_dates))
  
df <- data_frame(dates, winds)
df$dates <- ymd(df$dates)
df <- df %>% arrange(df$dates)
as_nums <- 1:length(dates)
df <- data_frame(as_nums, df)
df$winds[is.nan(df$winds)] <- NA
plot(x = df$as_nums, y = df$winds)
  
df <- na.omit(df)
plot(x = df$as_nums, y = df$winds) 
colnames(df) <- c("t", "dates", "winds")
dates <- df$dates
winds <- df$winds
  
wind_speed_approx <- approxfun(x = df$t, y = df$winds, rule = 2)
plot(x = as_nums, y = wind_speed_approx(as_nums), xlab = "Days from Start", ylab = "Avg Wind Speed", ylim = c(0,6))

##getting prism data

prism_set_dl_dir("~/Desktop/underc24/hydrology_model/prismtemp")
underc <- c(-89.51171, 46.24625) #a point to create UNDERC square to get PRISM data from (Lake id BE)
  
#mean daily air temp [C]
'get_prism_dailys(
  type = "tmean", 
  minDate = "2002-08-26",
  maxDate = "2022-09-25", 
  keepZip = FALSE
)
T_air_slice <- prism_archive_subset("tmean", "daily")
Temp <- pd_plot_slice(T_air_slice, underc)
Tair_data <- data_frame(Date = Temp[["data"]][["date"]], avg_temp = Temp[["data"]][["data"]])
Tair_data <- Tair_data[-c(1:11),] 
#^the first few days of 2002 were included in this set so this removes them, this might just be because I already had them downloaded
write.csv(Tair_data, "/Users/lisa/Downloads/Tair_data.csv") '

Tair_data <- read.csv("/Users/lisa/Downloads/Tair_data.csv")
Tair_approx <- approxfun(x = 1:length(Tair_data$Date), y = Tair_data$avg_temp, rule = 2)

#dew point [C]
'prism_set_dl_dir("~/Desktop/underc24/hydrology_model/prismtemp2") #getting a new temp dir everytime because they fill up otherwise
get_prism_dailys(
  type = "tdmean", 
  minDate = "2002-08-26", 
  maxDate = "2022-09-25",
  keepZip = FALSE
)
T_d_slice <- prism_archive_subset("tdmean", "daily")
dew <- pd_plot_slice(T_d_slice, underc)
T_d_data <- data_frame(Date = dew[["data"]][["date"]], dew_point = dew[["data"]][["data"]])
write.csv(T_d_data, "/Users/lisa/Downloads/T_d_data.csv") '''

T_d_data <- read.csv("/Users/lisa/Downloads/T_d_data.csv")
T_d_approx <- approxfun(x = 1:length(T_d_data$Date), y = T_d_data$dew_point, rule = 2)

#precipitation [mm]
'prism_set_dl_dir("~/Desktop/underc24/hydrology_model/prismtemp3")
get_prism_dailys(
  type = "ppt", 
  minDate = "2002-08-26", 
  maxDate = "2022-09-25",
  keepZip = FALSE
)
P_slice <- prism_archive_subset("ppt", "daily")
prec <- pd_plot_slice(P_slice, underc) 
P_data <- data_frame(Date = prec[["data"]][["date"]], precipitation = prec[["data"]][["data"]])
write.csv(P_data, "/Users/lisa/Downloads/P_data.csv") '''

P_data <- read.csv("/Users/lisa/Downloads/P_data.csv")
P_approx <- approxfun(x = 1:length(P_data$Date), y = P_data$precipitation, rule = 2)  

#radiation [MJ m^-2 day^-1]
#prism only has monthly normals available for solar radiation that need to be downloaded online, not daily averages
sol <- read.csv("/Users/lisa/Downloads/PRISM_solclear_stable_4km_monthly_normals_46.2500_-89.5046.csv")
row <- sol$PRISM.Time.Series.Data
one_year <- c(row[13], row[15], row[17], row[19], row[21], row[23], row[25], row[27], row[29], row[31], row[33], row[35])
Rs_data <- rep(NA, length = length(T_d_data$Date))
Rs_data <- data_frame(Date = T_d_data$Date, sol_radiation = Rs_data)
Rs_data$sol_radiation[1] = one_year[8]
for (i in 1:length(Rs_data$Date)){
  if (substr(Rs_data$Date[i], 9, 10) == "01"){
    Rs_data$sol_radiation[i] = one_year[as.numeric(substr(Rs_data$Date[i], 6, 7))]
  }
}
Rs_data <- data_frame(as_nums, Rs_data)
#plot(x = Rs_data$Date[129:463], y = Rs_data$sol_radiation[129:463])
Rs_data <- na.omit(Rs_data)
Rs_approx <- approxfun(x = Rs_data$as_nums, y = Rs_data$sol_radiation, rule = 2)
#plot(x = as_nums[0:500], y = Rs_approx(as_nums[0:500]), xlab = "Days from Start", ylab = "Avg Sol Radiation", ylim = c(0,35))

##getting month at current time step
get_month <- function(t){
  d = t + 238 #day of the year, our data starts at 2002-08-26, which is the 238th day of the year
  while (d > 365){
    d = d - 365
  } #should we worry about leap years?
  if (d <= 31){
    i = 1 #i is the month number
  }else if (d <= 59){
    i = 2
  }else if (d <= 90){
    i = 3
  }else if (d <= 120){
    i = 4
  }else if (d <= 151){
    i = 5
  }else if (d <= 181){
    i = 6
  }else if (d <= 212){
    i = 7
  }else if (d <= 243){
    i = 8
  }else if (d <= 273){
    i = 9
  }else if (d <= 304){
    i = 10
  }else if (d <= 334){
    i = 11
  }else {
    i = 12
  }
  return(i)
}




