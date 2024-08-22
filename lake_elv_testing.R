library(MFEUtilities)
library(lidR)

dbdir <- "~/Desktop/underc24/hydrology_model"
db <- "MFEdb.db"
setwd(dbdir)

longer_lake_list <- get_lake_chars2()

## plotting each lake one by one from staff gauge table
staff_gauges <- dbTable("STAFF_GAUGES")
staff_gauges <- filter(staff_gauges, staff_gauges$siteName == "WholeLake")

i = 0
row_lake <- staff_gauges$lakeID[1]
dates <- c()
heights <- c()

while (i < length(staff_gauges$projectID)){
  i = i + 1
  curr_row <- staff_gauges$lakeID[i]
  if (curr_row == row_lake){
    dates <- c(dates, staff_gauges$dateSample[i])
    heights <- c(heights, staff_gauges$waterHeight_m[i])
  } else{
    plot(x = dates, y = heights, xlab = "Date", ylab = "Elevation", main = paste0(staff_gauges$lakeID[i], " elevation across time"))
    dates <- c(staff_gauges$dateSample[i])
    heights <- c(staff_gauges$waterHeight_m[i])
    row_lake <- staff_gauges$lakeID[i]
  }
}


## creating data frame with all known points from manual staff gauge table
date_range <- seq(as.Date("2012-05-15"), as.Date("2022-09-25"), by = "days")

staff_gauges <- dbTable("STAFF_GAUGES")
staff_gauges <- staff_gauges %>% arrange(staff_gauges$dateSample)
staff_gauges <- staff_gauges %>% arrange(staff_gauges$lakeID)
staff_gauges <- filter(staff_gauges, staff_gauges$siteName == "WholeLake")

final_frame <- data_frame(date_range)

i = 0
row_lake <- staff_gauges$lakeID[1]
heights <- rep(NA, length = length(date_range))

for (i in 1:length(staff_gauges$lakeID)){
  curr_row = staff_gauges$lakeID[i]
  if (curr_row == row_lake){
    for (j in 1:length(date_range)){
      if (date_range[j] == staff_gauges$dateSample[i]){
        heights[j] = staff_gauges$waterHeight_m[i]
      }
    }
  } else {final_frame <- data_frame(heights, final_frame)
  colnames(final_frame)[1] <- curr_row
  row_lake = curr_row
  heights <- rep(NA, length = length(date_range))
  }}
  
matplot(x = final_frame$date_range, y = cbind(final_frame[,1:8]), type = "h", pch = "*", lty = 1, lwd = 5, col = height.colors(8), xlab = "Date", ylab = "Lake heights [m]")

##manual for CR
date_range <- seq(as.Date("2013-05-10"), as.Date("2022-08-25"), by = "days")

staff_gauges <- dbTable("STAFF_GAUGES")
staff_gauges <- staff_gauges %>% arrange(staff_gauges$dateSample)

staff_gauges_CR <- filter(staff_gauges, staff_gauges$lakeID == "CR")

i = 0

curr_h <- c()
final_h <- rep(NA, length = length(date_range))
j = 1

while (i < length(date_range)){
  i = i + 1
  while (date_range[i] == as.Date(staff_gauges_CR$dateSample[j]) & j < length(staff_gauges_CR$dateSample)){
    curr_h <- c(curr_h, staff_gauges_CR$waterHeight_m[j])
    j = j + 1
  }
  final_h[i] = mean(curr_h) + longer_lake_list$lake_surface_elevation[12]
  print(final_h[i])
  curr_h = c()
}

volumes <- rep(NA, length = length(date_range))
for (i in 1:length(date_range)){
  lake_volume = longer_lake_list$lake_surface_area[12] * (final_h[i] - lake_bot_elv)
  volumes[i] = lake_volume
}

date_range <- seq(as.Date("2002-08-25"), as.Date("2022-09-25"), by = "days")
diff_frame_manual <- data_frame(time = 0:7336, Date = date_range, vol_flux = LAKE[,"12"])
diff_frame_manual$Date[1] <- NA

i = 1
j = 1
vol_press <- rep(NA, length = length(diff_frame_manual$time))

while (i < length(diff_frame_manual$time)){
  i = i + 1
  while (diff_frame_manual$Date[i] != press_frame_CR$date_range[j] & j < length(press_frame_CR$date_range)){
    j = j + 1
  }
  if (j == length(press_frame_CR$date_range)){
    j = 1
  }else{
    vol_press[i] = press_frame_CR$volumes[j]
  }}

diff_frame <- data_frame(diff_frame, vol_press)
diff_frame_no_na <- na.omit(diff_frame)



###using pressure measurements
pressure_table <- sensordbTable("HOBO_PRESS_CORR")
pressure_table <- filter(pressure_table, !is.na(pressure_table$cleanedPress_kPa))
pressure_table <- filter(pressure_table, pressure_table$location == "StaffGauge")

## lake CR
pressure_table_CR <- filter(pressure_table, pressure_table$lakeID == "CR")
pressure_table_CR <- filter(pressure_table_CR, pressure_table_CR$location == "StaffGauge")
pressure_table_CR$dateTime <- floor_date(pressure_table_CR$dateTime, "day")
pressure_table_CR <- pressure_table_CR %>% arrange(pressure_table_CR$dateTime)

date_range <- seq(as.Date("2013-06-02"), as.Date("2022-09-25"), by = "days")

i = 0

curr_press <- c()
final_press <- rep(NA, length = length(date_range))
press_frame_CR <- data_frame(date_range)
j = 1

while (i < length(date_range)){
  i = i + 1
  while (date_range[i] == as.Date(pressure_table_CR$dateTime[j]) & j < length(pressure_table_CR$dateTime)){
    curr_press <- c(curr_press, pressure_table_CR$cleanedPress_kPa[j])
    j = j + 1
  }
  final_press[i] = mean(curr_press)
  print(final_press[i])
  curr_press = c()
}
press_frame_CR <- data_frame(press_frame_CR, pressure = final_press)
plot(press_frame_CR)

## NEON collected data from UNDERC on _____
## so the dtm elevation is from that date, meaning that the inital lake elevation for
## lake CR (511.35 m) comes from that date
## the average depth was collected on ___
## 

## pressure to lake elevation calculation

lake_bot_elv = longer_lake_list$lake_surface_elevation[12] - longer_lake_list$avg_depth[12] # [m]
lake_surface_elv_ini = longer_lake_list$lake_surface_elevation[12]
water_density = 1000 # [kg m^-3]
press_at_lake_surface_initial = 101325 * ((1 - (2.25577 * (10^-5) * (lake_surface_elv_ini)))^5.25588) / 1000 #[kpa]
press_at_lake_bot_inital = press_at_lake_surface_initial + ((longer_lake_list$avg_depth[12] * water_density * 9.8 )/1000) #[kpa]

heights <- rep(NA, length = length(press_frame_CR$date_range))
lake_depth <- heights

for (i in 1:length(press_frame_CR$date_range)){
  lake_depth[i] = 1000 * ((9.8 * water_density)^-1) * (press_frame_CR$pressure[i] - press_at_lake_surface_initial)
  surface_height = lake_bot_elv + lake_depth[i]
  heights[i] = surface_height
}

press_frame_CR <- data_frame(press_frame_CR, heights, lake_depth)

volumes <- rep(NA, length = length(press_frame_CR$date_range))
ini_diff <- 255587

for (i in 1:length(press_frame_CR$date_range)){
  lake_volume = longer_lake_list$lake_surface_area[12] * (lake_depth[i] + longer_lake_list$avg_depth[12]) #(heights[i] - lake_bot_elv)
  volumes[i] = lake_volume - avg_diff
}

press_frame_CR <- data_frame(press_frame_CR, volumes)
plot(x = press_frame_CR$date_range, y = press_frame_CR$volumes, xlab = "Date", ylab = "Volume [m^3]", main = "Lake CR volume over time from pressure")

## volume from pressure, vs volume from fluxes

date_range <- seq(as.Date("2002-08-25"), as.Date("2022-09-25"), by = "days")
diff_frame <- data_frame(time = 0:7336, Date = date_range, vol_flux = LAKE[,"12"])
diff_frame$Date[1] <- NA

i = 1
j = 1
vol_press <- rep(NA, length = length(diff_frame$time))

while (i < length(diff_frame$time)){
  i = i + 1
  while (diff_frame$Date[i] != press_frame_CR$date_range[j] & j < length(press_frame_CR$date_range)){
    j = j + 1
  }
  if (j == length(press_frame_CR$date_range)){
    j = 1
  }else{
    vol_press[i] = press_frame_CR$volumes[j]
}}

diff_frame <- data_frame(diff_frame, vol_press)
diff_frame_no_na <- na.omit(diff_frame)

matplot(diff_frame_no_na$Date, cbind(diff_frame_no_na$vol_flux, diff_frame_no_na$vol_press), type = "l", lty = 1, col = height.colors(2), xlab = "Date", ylab = "Lake Volumes", main = "Lake CR Volumes")
legend("bottomright", legend = c("Flux Vol", "Pressure Vol"), col = height.colors(2), lty = 1)

difference <- diff_frame_no_na$vol_press - diff_frame_no_na$vol_flux
perc_diff <- 100 * (difference/diff_frame_no_na$vol_flux)
per_diff_abs <- 100 * abs(difference/diff_frame_no_na$vol_flux)
plot(x = diff_frame_no_na$Date, y = perc_diff, xlab = "Date", ylab = "Percent Difference in Volume", main = "Percent Differnce in Volume for Lake CR")


avg_diff <- mean(difference)
start_press <- (volumes[367] + 296499.5)
ini_diff <- start_press - initial_volumes[12]

## testing the slopes between points each week

slope_frame_CR <- na.omit(diff_frame)
slope_flux <- c()
slope_press <- c()
for (i in (1:length(slope_frame_CR$time)/7)){
  if (i + 7 < length(slope_frame_CR$time)){
    diff_flux <- c()
    diff_press <- c()
    for (j in 0:6){
      diff_flux <- c(diff_flux, slope_frame_CR$vol_flux[i+j+1] - slope_frame_CR$vol_flux[i+j])
      diff_press <- c(diff_press, slope_frame_CR$vol_press[i+j+1] - slope_frame_CR$vol_press[i+j])
    }
    slope_flux <- c(slope_flux, mean(diff_flux))
    slope_press <- c(slope_press, mean(diff_press))
    }
  }
matplot(x = 1:1208, y = cbind(slope_flux, slope_press/2), type = "l", lty = 1, col = height.colors(2))

slope_flux <- c(0, slope_flux)
slope_press <- c(0, slope_press)
slope_frame_CR <- data_frame(slope_frame_CR, slope_flux, slope_press)
matplot(slope_frame_CR$time, cbind(slope_frame_CR$slope_flux, slope_frame_CR$slope_press), type = "l", lty = 1, col = height.colors(2), xlab = "time", ylab = "difference between points", main = "Lake CR difference between points")
avg_diff_slope <- mean(abs(slope_frame_CR$slope_press - slope_frame_CR$slope_flux))
slope_frame_CR <- data_frame(slope_frame_CR, slope_diff = slope_press - slope_flux)
matplot(slope_frame_CR$Date, slope_frame_CR$slope_diff, xlab = "Date", ylab = "difference", main = "Lake CR difference in slope", type = "l", lty = 1)

## comparing intial volumes

vol_table <- data_frame(lake_id = longer_lake_list$lake_id, estimated_vol = initial_volumes)
lake_byth_vol = rep(NA, length = length(longer_lake_list$lake_id))
for (i in 1:length(vol_table$lake_id)){
  j = 1
  while (vol_table$lake_id[i] != lake_byth$lakeID[j] & j < length(lake_byth$lakeID)){
    j = j + 1
  }
  lake_byth_vol[i] = lake_byth$volumeToBottom_m3[j]
}
vol_table <- data_frame(vol_table, lake_byth_vol)
vol_table$lake_byth_vol[12] = lake_byth$volumeToBottom3D_m3[25]
diff_vols <- vol_table$estimated_vol - as.numeric(vol_table$lake_byth_vol)
perc_diff_vols <- 100 * (diff_vols / as.numeric(vol_table$lake_byth_vol))
plot(x = 1:length(vol_table$lake_id), y = perc_diff_vols, xlab = "Lakes", ylab = "percent difference", main = "Percent Differnce in Volumes, estimated cylindircal vs lake byth MFE")
