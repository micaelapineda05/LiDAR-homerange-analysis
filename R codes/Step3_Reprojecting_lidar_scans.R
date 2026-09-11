library(sf)
library(dplyr)
library(ggplot2)

###Set stake locations as sf

laser_grid_stakes <- st_as_sf(laser_grid_stakes)

##Extract coords

coords <- st_coordinates(laser_grid_stakes)

laser_grid_stakes <- cbind(laser_grid_stakes, coords)

##Make list of plot IDs

plot_models <- list()

plots <- unique(laser_grid_stakes$id_plot)

plot_models <- list()

##Predict UTM coordinates using a transformation

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

layering_1.2 <- read.csv("data/normalized_scans_with_headers/July/layering csv/1.2_Subsampling_Remove Outliers_Normalize by Ground Points_Convert to ASCII.txt_xyLayering.csv", sep = ";", dec = ",")

filling_1.2 <- read.csv("data/normalized_scans_with_headers/July/filling csv/1.2_Subsampling_Remove Outliers_Normalize by Ground Points_Convert to ASCII.txt_xyFilling_layer1-10.csv", sep = ",", dec = ".")

filling_2_1.2 <- read.csv2("data/normalized_scans_with_headers/July/filling2 csv/1.2_Subsampling_Remove Outliers_Normalize by Ground Points_Convert to ASCII.txt_xyFilling_layer11-20.csv", sep = ",", dec = ".")

canopycover_1.2 <- read.csv("data/normalized_scans_with_headers/July/canopy cover csv/1.2_Subsampling_Remove Outliers_Normalize by Ground Points_Convert to ASCII.txt_xyCanopycover.csv", sep = ";", dec = ",")

roughness_1.2 <- read.csv("data/normalized_scans_with_headers/July/roughness csv/1.2_Subsampling_Remove Outliers_Normalize by Ground Points_Convert to ASCII.txt_xyRauigkeiten.csv", sep = ";", dec = ",")

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
## Turn the canopy cover into an SF object using the model predicted UTMs derived above
canopycover_sf <- st_as_sf(
  cover,
  coords = c("UTM_X", "UTM_Y"),
  crs = 25832
)

## Convert the UTM locations to WGS84 for plotting
## (SMA note: this is not necessary unless you want the axis labels to be in decimal degrees)
cover_1.2 <- st_transform(
  canopycover_sf,
  4326
)

## Assign the canopy cover object with the UTM CRS as the one to be used below
cover_1.2 <- canopycover_sf

##Find an individual for the plot

ud <- res[["900200000718873"]]

ud95 <- SpatialPolygonsDataFrame.UD(
  ud,
  level.UD = 0.95
)

ud95_sf <- st_as_sf(ud95)

st_crs(ud95_sf)

graphics.off()

## Get a Niedersachsen outline to make sure the plots are in the right area
library(rnaturalearth)
# Import all German federal states as an 'sf' object
germany_states <- ne_states(country = "germany", returnclass = "sf")
# Filter to Niedersachsen
niedersachsen_outline <- germany_states |> filter(name == "Niedersachsen")


###Plot w/ capture locations for this individual
ggplot() +
  #geom_sf(data = niedersachsen_outline, fill = "lightblue", color = "black", size = 0.5) + # Niedersachsen underlying map, comment out to zoom in on a plot
  geom_sf(data = cover_1.2,
          aes(color = canopy_cover),
          size = 0.5) +
  geom_sf(data = ud95_sf,
          fill = NA,
          color = "red",
          linewidth = 1) +
  geom_sf(data = laser_grid_stakes |>
            filter(id_plot=="1.2"),
          color="purple") +
  geom_sf(data=joined_df |>
            filter(id_plot=="1.2") |>
            filter(TrapNum%in%(joined_df|>filter(PITnum=="900200000718873",id_plot=="1.2")|>pull(TrapNum)))|>
            pull(geometry),
            color="red")

