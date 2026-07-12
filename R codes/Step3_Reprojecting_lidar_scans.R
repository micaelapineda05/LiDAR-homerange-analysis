library(sf)
library(dplyr)
library(ggplot2)

laser_grid_stakes <- st_as_sf(laser_grid_stakes)

coords <- st_coordinates(laser_grid_stakes)

laser_grid_stakes <- cbind(laser_grid_stakes, coords)

plot_models <- list()

plots <- unique(laser_grid_stakes$id_plot)

plot_models <- list()

plot_models <- list()

for (p in plots) {
  
  dat <- laser_grid_stakes %>%
    filter(id_plot == p)
  
  if(all(is.na(dat$laser_stake_x)) ||
     all(is.na(dat$laser_stake_y))) {
    
    message("Skipping ", p)
    next
  }
  
  dat_model <- dat %>%
    rename(
      x = laser_stake_x,
      y = laser_stake_y
    )
  
  fit_x <- lm(X ~ x + y, data = dat_model)
  fit_y <- lm(Y ~ x + y, data = dat_model)
  
  plot_models[[p]] <- list(
    fit_x = fit_x,
    fit_y = fit_y
  )
}


plot_num <- "1.2"

## Read in LiDAR CSV files

layering_1.2 <- read.csv("C:/Users/PinedaMicaelaTonatsi/Documents/LiDAR Collab/New_2026_normalized/Wetransfer_results_2026-02-06/July/layering csv/1.2_Subsampling_Remove Outliers_Normalize by Ground Points_Convert to ASCII.txt_xyLayering.csv", sep = ";", dec = ",")

filling_1.2 <- read.csv("C:/Users/PinedaMicaelaTonatsi/Documents/LiDAR Collab/New_2026_normalized/Wetransfer_results_2026-02-06/July/filling csv/1.2_Subsampling_Remove Outliers_Normalize by Ground Points_Convert to ASCII.txt_xyFilling_layer1-10.csv", sep = ",", dec = ".")

filling_2_1.2 <- read.csv2("C:/Users/PinedaMicaelaTonatsi/Documents/LiDAR Collab/New_2026_normalized/wetransfer_results_2026-02-06/July/filling2 csv/1.2_Subsampling_Remove Outliers_Normalize by Ground Points_Convert to ASCII.txt_xyFilling_layer11-20.csv", sep = ",", dec = ".")

canopycover_1.2 <- read.csv("C:/Users/PinedaMicaelaTonatsi/Documents/LiDAR Collab/New_2026_normalized/Wetransfer_results_2026-02-06/July/canopy cover csv/1.2_Subsampling_Remove Outliers_Normalize by Ground Points_Convert to ASCII.txt_xyCanopycover.csv", sep = ";", dec = ",")

roughness_1.2 <- read.csv("C:/Users/PinedaMicaelaTonatsi/Documents/LiDAR Collab/New_2026_normalized/Wetransfer_results_2026-02-06/July/roughness csv/1.2_Subsampling_Remove Outliers_Normalize by Ground Points_Convert to ASCII.txt_xyRauigkeiten.csv", sep = ";", dec = ",")

cover <- canopycover_1.2

cover$UTM_X <- predict(
  plot_models[[plot_num]]$fit_x,
  newdata = cover
)

cover$UTM_Y <- predict(
  plot_models[[plot_num]]$fit_y,
  newdata = cover
)


###Test one

canopycover_sf <- st_as_sf(
  cover,
  coords = c("UTM_X", "UTM_Y"),
  crs = 25832
)

cover_1.2 <- st_transform(
  canopycover_sf,
  4326
)

##Find an individual for the plot

ud <- res[["900200000718873"]]

ud95 <- SpatialPolygonsDataFrame.UD(
  ud,
  level.UD = 0.95
)

ud95_sf <- st_as_sf(ud95)

st_crs(ud95_sf)

graphics.off()

###Plot
ggplot() +
  geom_sf(data = cover_1.2,
          aes(color = canopy_cover),
          size = 0.5) +
  geom_sf(data = ud95_sf,
          fill = NA,
          color = "red",
          linewidth = 1)

