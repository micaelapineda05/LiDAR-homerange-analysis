library(sf)
library(terra)

# Get all unique plot names
plot_names <- names(plot_lists)

# Set your base directory
base_dir <- "C:/Users/PinedaMicaelaTonatsi/Documents/LiDAR Collab/New Data Winter & Summer/July/"

# Create a list to store all final dataframes
all_plots_data <- list()

# Loop through each plot
for(plot_name in plot_names) {
  
  cat("Processing", plot_name, "\n")
  
  # Wrap everything in tryCatch for error handling
  tryCatch({
    
    plot_num <- sub("Plot_", "", plot_name)
    
    # ---- CANOPY COVER ----
    cover <- read.csv(paste0(base_dir, "July canopy cover csv/", plot_num, ".txt_xyCanopycover.csv"), 
                      sep = ";", dec = ",", header = TRUE,
                      col.names = c("x", "y", "canopy_cover"), fill = TRUE)
    
    cover$x <- as.numeric(gsub("[^0-9.-]", "", cover$x))
    cover$y <- as.numeric(gsub("[^0-9.-]", "", cover$y))
    
    df_sf <- st_as_sf(cover, coords = c("x", "y"), crs = st_crs(plot_lists[[plot_name]][[1]]))
    
    extracted_values <- list()
    for(i in seq_along(plot_lists[[plot_name]])) {
      geom <- plot_lists[[plot_name]][[i]]
      points_in_poly <- st_intersection(df_sf, st_sf(geometry = geom))
      # Only store if there are points
      if(nrow(points_in_poly) > 0) {
        extracted_values[[names(plot_lists[[plot_name]])[i]]] <- points_in_poly$canopy_cover
      }
    }
    
    # Skip if no data extracted
    if(length(extracted_values) == 0) {
      cat("  No data found for", plot_name, "- skipping\n\n")
      next
    }
    
    extracted_df_list <- lapply(names(extracted_values), function(name) {
      data.frame(polygon = name, canopy_cover = extracted_values[[name]])
    })
    extracted_df <- do.call(rbind, extracted_df_list)
    
    # ---- ROUGHNESS ----
    roughness <- read.csv(paste0(base_dir, "July roughness csv/", plot_num, ".txt_xyRauigkeiten.csv"), sep = ",")
    
    roughness$x <- as.numeric(gsub("[^0-9.-]", "", roughness$x))
    roughness$y <- as.numeric(gsub("[^0-9.-]", "", roughness$y))
    
    df_sf_rough <- st_as_sf(roughness, coords = c("x", "y"), crs = st_crs(plot_lists[[plot_name]][[1]]))
    
    roughness_extracted <- list()
    for(i in seq_along(plot_lists[[plot_name]])) {
      geom <- plot_lists[[plot_name]][[i]]
      name <- names(plot_lists[[plot_name]])[i]
      points_in_poly <- st_intersection(df_sf_rough, st_sf(geometry = geom))
      if(nrow(points_in_poly) > 0) {
        roughness_extracted[[name]] <- points_in_poly$roughness
      }
    }
    
    roughness_df_list <- lapply(names(roughness_extracted), function(name) {
      data.frame(polygon = name, roughness = roughness_extracted[[name]])
    })
    roughness_df <- do.call(rbind, roughness_df_list)
    
    # ---- MERGE ----
    extracted_df$point_id <- ave(rep(1, nrow(extracted_df)), extracted_df$polygon, FUN = seq_along)
    roughness_df$point_id <- ave(rep(1, nrow(roughness_df)), roughness_df$polygon, FUN = seq_along)
    combined_df <- merge(extracted_df, roughness_df, by = c("polygon", "point_id"), all = TRUE)
    
    # ---- LAYERING ----
    layering <- read.csv(paste0(base_dir, "July layering csv/", plot_num, ".txt_xyLayering.csv"), sep = ",")
    
    layering$x <- as.numeric(gsub("[^0-9.-]", "", layering$x))
    layering$y <- as.numeric(gsub("[^0-9.-]", "", layering$y))
    
    layering_sf <- st_as_sf(layering, coords = c("x", "y"), crs = st_crs(plot_lists[[plot_name]][[1]]))
    
    layering_extracted <- list()
    for(i in seq_along(plot_lists[[plot_name]])) {
      geom <- plot_lists[[plot_name]][[i]]
      name <- names(plot_lists[[plot_name]])[i]
      points_in_poly <- suppressWarnings(st_intersection(layering_sf, st_sf(geometry = geom)))
      
      if(nrow(points_in_poly) > 0) {
        layering_extracted[[name]] <- data.frame(
          ENL0D = points_in_poly$ENL0D,
          ENL1 = points_in_poly$ENL1,
          ENL2D = points_in_poly$ENL2D
        )
      }
    }
    
    layering_df_list <- lapply(names(layering_extracted), function(name) {
      data.frame(polygon = name, layering_extracted[[name]])
    })
    layering_df <- do.call(rbind, layering_df_list)
    layering_df$point_id <- ave(rep(1, nrow(layering_df)), layering_df$polygon, FUN = seq_along)
    
    final_df <- merge(combined_df, layering_df, by = c("polygon", "point_id"), all = TRUE)
    
    # ---- FILLING 1-10 ----
    filling <- read.csv(paste0(base_dir, "July filling csv/", plot_num, ".txt_xyFilling_layer1-10.csv"), sep = ",")
    
    filling$x <- as.numeric(gsub("[^0-9.-]", "", filling$x))
    filling$y <- as.numeric(gsub("[^0-9.-]", "", filling$y))
    
    filling_sf <- st_as_sf(filling, coords = c("x", "y"), crs = st_crs(plot_lists[[plot_name]][[1]]))
    
    filling_extracted <- list()
    for(i in seq_along(plot_lists[[plot_name]])) {
      geom <- plot_lists[[plot_name]][[i]]
      name <- names(plot_lists[[plot_name]])[i]
      points_in_poly <- suppressWarnings(st_intersection(filling_sf, st_sf(geometry = geom)))
      
      if(nrow(points_in_poly) > 0) {
        filling_extracted[[name]] <- data.frame(
          X0.25m = points_in_poly$X0.25m,
          X0.5m = points_in_poly$X0.5m,
          X0.75m = points_in_poly$X0.75m,
          X1m = points_in_poly$X1m,
          X1.25m = points_in_poly$X1.25m,
          X1.5m = points_in_poly$X1.5m,
          X1.75m = points_in_poly$X1.75m,
          X2m = points_in_poly$X2m,
          X2.25m = points_in_poly$X2.25m,
          X2.5m = points_in_poly$X2.5m
        )
      }
    }
    
    filling_df_list <- lapply(names(filling_extracted), function(name) {
      data.frame(polygon = name, filling_extracted[[name]])
    })
    filling_df <- do.call(rbind, filling_df_list)
    filling_df$point_id <- ave(rep(1, nrow(filling_df)), filling_df$polygon, FUN = seq_along)
    
    final_df_2 <- merge(final_df, filling_df, by = c("polygon", "point_id"), all = TRUE)
    
    # ---- FILLING 11-20 ----
    filling2 <- read.csv(paste0(base_dir, "July filling2 csv/", plot_num, ".txt_xyFilling_layer11-20.csv"), sep = ",")
    
    filling2$x <- as.numeric(gsub("[^0-9.-]", "", filling2$x))
    filling2$y <- as.numeric(gsub("[^0-9.-]", "", filling2$y))
    
    filling2_sf <- st_as_sf(filling2, coords = c("x", "y"), crs = st_crs(plot_lists[[plot_name]][[1]]))
    
    filling2_extracted <- list()
    for(i in seq_along(plot_lists[[plot_name]])) {
      geom <- plot_lists[[plot_name]][[i]]
      name <- names(plot_lists[[plot_name]])[i]
      points_in_poly <- suppressWarnings(st_intersection(filling2_sf, st_sf(geometry = geom)))
      
      if(nrow(points_in_poly) > 0) {
        filling2_extracted[[name]] <- data.frame(
          X2.75m = points_in_poly$X2.75m,
          X3m = points_in_poly$X3m,
          X3.25m = points_in_poly$X3.25m,
          X3.5m = points_in_poly$X3.5m,
          X3.75m = points_in_poly$X3.75m,
          X4m = points_in_poly$X4m,
          X4.25m = points_in_poly$X4.25m,
          X4.5m = points_in_poly$X4.5m,
          X4.75m = points_in_poly$X4.75m,
          X5m = points_in_poly$X5m
        )
      }
    }
    
    filling2_df_list <- lapply(names(filling2_extracted), function(name) {
      data.frame(polygon = name, filling2_extracted[[name]])
    })
    filling2_df <- do.call(rbind, filling2_df_list)
    filling2_df$point_id <- ave(rep(1, nrow(filling2_df)), filling2_df$polygon, FUN = seq_along)
    
    filling_final_df <- merge(final_df_2, filling2_df, by = c("polygon", "point_id"), all = TRUE)
    
    # Store result
    all_plots_data[[plot_name]] <- filling_final_df
    
    cat("Completed", plot_name, "\n\n")
    
  }, error = function(e) {
    cat("ERROR processing", plot_name, ":", conditionMessage(e), "\n\n")
  })
}

# Combine all plots into one big dataframe (optional)
final_all_plots <- do.call(rbind, all_plots_data)

# Or save each plot separately
for(plot_name in names(all_plots_data)) {
  write.csv(all_plots_data[[plot_name]], 
            paste0(base_dir, "extracted_data_", plot_name, ".csv"), 
            row.names = FALSE)
}


library(dplyr)

# Calculate summary statistics for each polygon
polygon_summaries <- final_all_plots %>%
  group_by(polygon) %>%
  summarise(
    mean_canopy_cover = mean(canopy_cover, na.rm = TRUE),
    mean_roughness = mean(roughness, na.rm = TRUE),
    mean_ENL0D = mean(ENL0D, na.rm = TRUE),
    mean_ENL1 = mean(ENL1, na.rm = TRUE),
    mean_ENL2D = mean(ENL2D, na.rm = TRUE),
    mean_X0.25m = mean(X0.25m, na.rm = TRUE),
    mean_X0.5m = mean(X0.5m, na.rm = TRUE),
    mean_X0.75m = mean(X0.75m, na.rm = TRUE),
    mean_X1m = mean(X1m, na.rm = TRUE),
    mean_X1.25m = mean(X1.25m, na.rm = TRUE),
    mean_X1.5m = mean(X1.5m, na.rm = TRUE),
    mean_X1.75m = mean(X1.75m, na.rm = TRUE),
    mean_X2m = mean(X2m, na.rm = TRUE),
    mean_X2.25m = mean(X2.25m, na.rm = TRUE),
    mean_X2.5m = mean(X2.5m, na.rm = TRUE),
    mean_X2.75m = mean(X2.75m, na.rm = TRUE),
    mean_X3m = mean(X3m, na.rm = TRUE),
    mean_X3.25m = mean(X3.25m, na.rm = TRUE),
    mean_X3.5m = mean(X3.5m, na.rm = TRUE),
    mean_X3.75m = mean(X3.75m, na.rm = TRUE),
    mean_X4m = mean(X4m, na.rm = TRUE),
    mean_X4.25m = mean(X4.25m, na.rm = TRUE),
    mean_X4.5m = mean(X4.5m, na.rm = TRUE),
    mean_X4.75m = mean(X4.75m, na.rm = TRUE),
    mean_X5m = mean(X5m, na.rm = TRUE),
    n_points = n()  # Number of points per polygon
  )

# View the results
head(polygon_summaries)

write.csv(polygon_summaries, "C:/Users/PinedaMicaelaTonatsi/Documents/LiDAR Collab/Plot extractions/average_vaules_July.csv")
all_plots_data_july <- all_plots_data
