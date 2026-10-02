library(sf)
library(dplyr)
library(ggplot2)

rm(list = setdiff(ls(), c("plot_models", "laser_grid_stakes", "res", "joined_df")))

##----------------------
# Lidar habitat folders
##----------------------

layering_dir <- "LiDAR Collab/New_2026_normalized/Wetransfer_results_2026-02-06/July/layering csv"

filling_dir <- "LiDAR Collab/New_2026_normalized/Wetransfer_results_2026-02-06/July/filling csv"

filling2_dir <- "LiDAR Collab/New_2026_normalized/wetransfer_results_2026-02-06/July/filling2 csv"

canopy_dir <- "LiDAR Collab/New_2026_normalized/Wetransfer_results_2026-02-06/July/canopy cover csv"

roughness_dir <- "LiDAR Collab/New_2026_normalized/Wetransfer_results_2026-02-06/July/roughness csv"


# Empty list
lidar_corrected <- list()


# Loop through plots
for (plot_num in names(plot_models)) {
  
  message("Processing plot: ", plot_num)

  if (is.null(plot_models[[plot_num]])) {
    message("  No model found - skipping")
    next
  }
  
  ### --------------------------------------------------
  ### File names
  ### --------------------------------------------------
  
  layering_file <- file.path(
    layering_dir,
    paste0(
      plot_num,
      "_Subsampling_Remove Outliers_Normalize by Ground Points_Convert to ASCII.txt_xyLayering.csv"
    )
  )
  
  filling_file <- file.path(
    filling_dir,
    paste0(
      plot_num,
      "_Subsampling_Remove Outliers_Normalize by Ground Points_Convert to ASCII.txt_xyFilling_layer1-10.csv"
    )
  )
  
  filling2_file <- file.path(
    filling2_dir,
    paste0(
      plot_num,
      "_Subsampling_Remove Outliers_Normalize by Ground Points_Convert to ASCII.txt_xyFilling_layer11-20.csv"
    )
  )
  
  canopy_file <- file.path(
    canopy_dir,
    paste0(
      plot_num,
      "_Subsampling_Remove Outliers_Normalize by Ground Points_Convert to ASCII.txt_xyCanopycover.csv"
    )
  )
  
  roughness_file <- file.path(
    roughness_dir,
    paste0(
      plot_num,
      "_Subsampling_Remove Outliers_Normalize by Ground Points_Convert to ASCII.txt_xyRauigkeiten.csv"
    )
  )
  
  
  files <- c(
    layering = layering_file,
    filling = filling_file,
    filling2 = filling2_file,
    canopycover = canopy_file,
    roughness = roughness_file
  )
  
  if (!all(file.exists(files))) {
    
    message(
      "  Missing file(s): ",
      paste(names(files)[!file.exists(files)], collapse = ", ")
    )
    
    next
  }
  
  
  ### --------------------------------------------------
  ### Read CSVs
  ### --------------------------------------------------
  
  layering <- read.csv(
    layering_file,
    sep = ";",
    dec = ","
  )
  
  filling <- read.csv(
    filling_file,
    sep = ",",
    dec = "."
  )
  
  filling2 <- read.csv2(
    filling2_file,
    sep = ",",
    dec = "."
  )
  
  canopycover <- read.csv(
    canopy_file,
    sep = ";",
    dec = ","
  )
  
  roughness <- read.csv(
    roughness_file,
    sep = ";",
    dec = ","
  )
  
  
  # --------------------------------------------------
  # Predict corrected UTM coordinates
  # --------------------------------------------------
  
  datasets <- list(
    layering = layering,
    filling = filling,
    filling2 = filling2,
    canopycover = canopycover,
    roughness = roughness
  )
  
  
  datasets_corrected <- lapply(datasets, function(dat) {
    
    dat$UTM_X <- predict(
      plot_models[[plot_num]]$fit_x,
      newdata = dat
    )
    
    dat$UTM_Y <- predict(
      plot_models[[plot_num]]$fit_y,
      newdata = dat
    )
    
    # Convert to sf
    dat_sf <- st_as_sf(
      dat,
      coords = c("UTM_X", "UTM_Y"),
      crs = 25832,
      remove = FALSE
    )
    
    return(dat_sf)
  })
  
  
  # Store results
  lidar_corrected[[plot_num]] <- datasets_corrected
  
}

lidar_corrected_wgs84 <- lapply(
  lidar_corrected,
  function(plot) {
    
    lapply(
      plot,
      function(x) st_transform(x, 4326)
    )
    
  }
)

joined_df <- joined_df %>%
  mutate(PITnum = format(PITnum, scientific = FALSE, trim = TRUE))

res[["900200000718590"]][["900200000718590"]]@info

for (id in names(res)) {
  
  # Only process nested ctmm objects
  if (!is.list(res[[id]])) next
  if (!id %in% names(res[[id]])) next
  if (!inherits(res[[id]][[id]], "ctmm")) next
  
  # Find matching plot
  plot_id <- pit_plot_lookup_clean$plot_id[pit_plot_lookup_clean$PITnum == id]
  
  if (length(plot_id) == 1) {
    res[[id]][[id]]@info$plot_id <- plot_id
  }
}

res[["900200000718590"]][["900200000718590"]]@info$plot_id

for (id in names(res)) {
  
  if (!is.list(res[[id]])) next
  if (!id %in% names(res[[id]])) next
  if (!inherits(res[[id]][[id]], "ctmm")) next
  
  cat(
    id, "->",
    res[[id]][[id]]@info$plot_id,
    "\n"
  )
}

successful_ids <- names(res)[!sapply(areas, is, "try-error")]

length(successful_ids)

head(successful_ids)

res_success <- res_ids %>%
  filter(PITnum %in% successful_ids)

View(res_success)

sum(is.na(res_success$plot_id))

ud <- res[[ud_ids[1]]]

ud50 <- SpatialPolygonsDataFrame.UD(
  ud,
  level.UD = 0.50
)

ud50_sf <- st_as_sf(ud50)

ud50_sf

length(successful_ids)

length(ud_ids)

table(
  successful = names(res) %in% successful_ids,
  is_UD = sapply(res, function(x) inherits(x, "UD"))
)

get_kde50 <- function(ud) {
  
  ud50 <- SpatialPolygonsDataFrame.UD(
    ud,
    level.UD = 0.50
  )
  
  ud50_sf <- st_as_sf(ud50)
  
  # Keep only the estimated 50% isopleth
  ud50_est <- ud50_sf %>%
    filter(grepl("50% est", name))
  
  # Transform to LiDAR CRS
  ud50_est <- st_transform(ud50_est, 25832)
  
  return(ud50_est)
}


pit <- ud_ids[1]

kde50_test <- get_kde50(res[[pit]])

kde50_test

st_crs(kde50_test)

res_success %>%
  filter(PITnum == pit) %>%
  select(PITnum, SpeciesID, plot_id_clean)

plot <- unique(res_success %>%
  filter(PITnum == pit) %>%
  pull(plot_id_clean))

plot

canopy <- lidar_corrected[[4.2]]$canopycover

canopy_kde50 <- st_filter(
  canopy,
  kde50_test
)

nrow(canopy_kde50)

ggplot() +
  geom_sf(data = canopy, size = 0.5) +
  geom_sf(data = kde50_test_utm, fill = NA, linewidth = 1) +
  theme_minimal()
