library(sf)
library(dplyr)
library(ggplot2)

laser_grid_stakes <- st_as_sf(laser_grid_stakes)

plot32 <- laser_grid_stakes %>%
  filter(id_plot == "3.2")

coords <- st_coordinates(plot32)

plot32 <- cbind(plot32, coords)

head(plot32)

fit_x <- lm(X ~ laser_stake_x + laser_stake_y, data = plot32)

fit_y <- lm(Y ~ laser_stake_x + laser_stake_y, data = plot32)

summary(fit_x)
summary(fit_y)

plot32$X_pred <- predict(fit_x)
plot32$Y_pred <- predict(fit_y)

sqrt((plot32$X - plot32$X_pred)^2 +
       (plot32$Y - plot32$Y_pred)^2) |> summary()


###Offset coords
plot_offsets <- laser_grid_stakes %>%
  st_as_sf() %>%
  mutate(
    UTM_X = st_coordinates(.)[,1],
    UTM_Y = st_coordinates(.)[,2]
  ) %>%
  st_drop_geometry() %>%
  group_by(id_plot) %>%
  summarise(
    offset_x = mean(UTM_X - laser_stake_x),
    offset_y = mean(UTM_Y - laser_stake_y),
    .groups = "drop"
  )


## Read in LiDAR CSV files

layering_1.2 <- read.csv("C:/Users/PinedaMicaelaTonatsi/Documents/LiDAR Collab/New_2026_normalized/Wetransfer_results_2026-02-06/July/layering csv/1.2_Subsampling_Remove Outliers_Normalize by Ground Points_Convert to ASCII.txt_xyLayering.csv", sep = ";", dec = ",")

filling_1.2 <- read.csv("C:/Users/PinedaMicaelaTonatsi/Documents/LiDAR Collab/New_2026_normalized/Wetransfer_results_2026-02-06/July/filling csv/1.2_Subsampling_Remove Outliers_Normalize by Ground Points_Convert to ASCII.txt_xyFilling_layer1-10.csv", sep = ",", dec = ".")

filling_2_1.2 <- read.csv2("C:/Users/PinedaMicaelaTonatsi/Documents/LiDAR Collab/New_2026_normalized/wetransfer_results_2026-02-06/July/filling2 csv/1.2_Subsampling_Remove Outliers_Normalize by Ground Points_Convert to ASCII.txt_xyFilling_layer11-20.csv", sep = ",", dec = ".")

canopycover_1.2 <- read.csv("C:/Users/PinedaMicaelaTonatsi/Documents/LiDAR Collab/New_2026_normalized/Wetransfer_results_2026-02-06/July/canopy cover csv/1.2_Subsampling_Remove Outliers_Normalize by Ground Points_Convert to ASCII.txt_xyCanopycover.csv", sep = ";", dec = ",")

roughness_1.2 <- read.csv("C:/Users/PinedaMicaelaTonatsi/Documents/LiDAR Collab/New_2026_normalized/Wetransfer_results_2026-02-06/July/roughness csv/1.2_Subsampling_Remove Outliers_Normalize by Ground Points_Convert to ASCII.txt_xyRauigkeiten.csv", sep = ";", dec = ",")

offset_1.2 <- plot_offsets %>%
  filter(id_plot == "1.2")

cover_1.2 <- canopycover_1.2 %>%
  mutate(
    UTM_X = x + offset_1.2$offset_x,
    UTM_Y = y + offset_1.2$offset_y
  )

roughness_1.2 <- roughness_1.2 %>%
  mutate(
    UTM_X = x + offset_1.2$offset_x,
    UTM_Y = y + offset_1.2$offset_y
  )

layering_1.2 <- layering_1.2 %>%
  mutate(
    UTM_X = x + offset_1.2$offset_x,
    UTM_Y = y + offset_1.2$offset_y
  )

filling_1.2 <- filling_1.2 %>%
  mutate(
    UTM_X = x + offset_1.2$offset_x,
    UTM_Y = y + offset_1.2$offset_y
  )


filling_2_1.2 <- filling_2_1.2 %>%
  mutate(
    UTM_X = x + offset_1.2$offset_x,
    UTM_Y = y + offset_1.2$offset_y
  )

###Test one

canopycover_sf <- st_as_sf(
  cover_1.2,
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

ggplot() +
  geom_sf(data = canopycover_sf)

ggplot() +
  geom_sf(data = ud95_sf)

pts <- st_as_sf(
  joined_df %>%
    filter(PITnum == "900200000718873"),
  coords = c("laser_stake_X", "laser_stake_Y"),
  crs = 25832
)

ggplot() +
  geom_raster(
    data = canopycover_sf,
    aes(lasers_X, UTM_Y, fill = canopy_cover)
  ) +
  geom_sf(data = ud95_sf,
          fill = NA,
          colour = "red") +
  geom_sf(data = pts,
          colour = "yellow",
          size = 2)
