# =============================================================================
# LiDAR Data Extraction for ctmm Utilization Distribution Objects
# =============================================================================
# This script extracts LiDAR-derived metrics (canopy cover, roughness, layering, 
# and filling) for home ranges defined by ctmm UD objects instead of manual polygons.
#
# Required packages: ctmm, sf, dplyr
# =============================================================================

library(ctmm)
library(sf)
library(dplyr)

# =============================================================================
# CONFIGURATION
# =============================================================================

# Set base directory (adjust this path)
base_dir <- "C:/Users/PinedaMicaelaTonatsi/Documents/LiDAR Collab/New_2026_normalized/wetransfer_results_2026-02-06/March/"

# Define which contour levels you want to extract
# Common choices: 0.50 (core area), 0.95 (home range), 0.99 (full range)
contour_levels <- c(0.50, 0.95)

# Use lookup table:
use_lookup <- TRUE

# =============================================================================
# MAIN EXTRACTION LOOP
# =============================================================================

# Create a list to store all final dataframes
all_ud_data <- list()

# Loop through each UD object
for(ud_name in names(res)) {
  
  cat("========================================\n")
  cat("Processing", ud_name, "\n")
  cat("========================================\n")
  
  tryCatch({
    
    # Get the file identifier (either from lookup or use ud_name directly)
    if(use_lookup) {
      file_id <- ud_file_lookup$file_id[ud_file_lookup$ud_name == ud_name]
      if(length(file_id) == 0) {
        cat("  No file_id found for", ud_name, "- skipping\n\n")
        next
      }
    } else {
      file_id <- ud_name
    }
    
    cat("  File ID:", file_id, "\n")
    
    # -------------------------------------------------------------------------
    # Convert ctmm UD to spatial polygons for each contour level
    # -------------------------------------------------------------------------
    
    ud_sf_list <- list()
    
    for(level in contour_levels) {
      cat("  Extracting", level*100, "% contour...\n")
      
      # Convert UD to SpatialPolygonsDataFrame then to sf
      # The level.UD parameter specifies the probability level
      ud_sp <- SpatialPolygonsDataFrame.UD(res[[ud_name]], level.UD = level)
      ud_sf <- st_as_sf(ud_sp)
      
      # Create a descriptive name for this polygon
      polygon_name <- paste0(ud_name, "_", sprintf("%.0f", level*100), "pct")
      
      # Store just the geometry
      ud_sf_list[[polygon_name]] <- st_geometry(ud_sf)
    }
    
    # Get CRS from the UD object
    crs_to_use <- st_crs(ud_sf)
    cat("  CRS:", st_crs(ud_sf)$input, "\n")
    
    # -------------------------------------------------------------------------
    # CANOPY COVER
    # -------------------------------------------------------------------------
    
    cat("  Reading canopy cover data...\n")
    
    cover_file <- paste0(base_dir, "canopy cover csv/", file_id, 
                         "_Subsampling_Remove Outliers_Normalize by Ground Points_Convert to ASCII.txt_xyCanopycover.csv")
    
    if(!file.exists(cover_file)) {
      cat("  WARNING: Canopy cover file not found:", cover_file, "\n")
      cat("  Skipping", ud_name, "\n\n")
      next
    }
    
    cover <- read.csv(cover_file, sep = ";", dec = ",", header = TRUE)
    colnames(cover) <- c("x", "y", "canopy_cover")
    
    # Convert to sf points
    df_sf <- st_as_sf(cover, coords = c("x", "y"), crs = crs_to_use)
    
    # Extract values for each polygon (contour level)
    extracted_values <- list()
    for(polygon_name in names(ud_sf_list)) {
      geom <- ud_sf_list[[polygon_name]]
      points_in_poly <- st_intersection(df_sf, st_sf(geometry = geom))
      if(nrow(points_in_poly) > 0) {
        extracted_values[[polygon_name]] <- points_in_poly$canopy_cover
      }
    }
    
    if(length(extracted_values) == 0) {
      cat("  No canopy cover data found for", ud_name, "- skipping\n\n")
      next
    }
    
    # Convert to dataframe
    extracted_df_list <- lapply(names(extracted_values), function(name) {
      data.frame(polygon = name, canopy_cover = extracted_values[[name]])
    })
    extracted_df <- do.call(rbind, extracted_df_list)
    
    cat("  Extracted", nrow(extracted_df), "canopy cover points\n")
    
    # -------------------------------------------------------------------------
    # ROUGHNESS
    # -------------------------------------------------------------------------
    
    cat("  Reading roughness data...\n")
    
    roughness_file <- paste0(base_dir, "roughness csv/", file_id, 
                             "_Subsampling_Remove Outliers_Normalize by Ground Points_Convert to ASCII.txt_xyRauigkeiten.csv")
    
    if(!file.exists(roughness_file)) {
      cat("  WARNING: Roughness file not found\n")
      roughness_df <- data.frame(polygon = character(), roughness = numeric())
    } else {
      roughness <- read.csv(roughness_file, sep = ";", dec = ",", header = TRUE)
      colnames(roughness) <- c("x", "y", "roughness")
      
      df_sf_rough <- st_as_sf(roughness, coords = c("x", "y"), crs = crs_to_use)
      
      roughness_extracted <- list()
      for(polygon_name in names(ud_sf_list)) {
        geom <- ud_sf_list[[polygon_name]]
        points_in_poly <- st_intersection(df_sf_rough, st_sf(geometry = geom))
        if(nrow(points_in_poly) > 0) {
          roughness_extracted[[polygon_name]] <- points_in_poly$roughness
        }
      }
      
      roughness_df_list <- lapply(names(roughness_extracted), function(name) {
        data.frame(polygon = name, roughness = roughness_extracted[[name]])
      })
      roughness_df <- do.call(rbind, roughness_df_list)
      
      cat("  Extracted", nrow(roughness_df), "roughness points\n")
    }
    
    # -------------------------------------------------------------------------
    # MERGE CANOPY COVER AND ROUGHNESS
    # -------------------------------------------------------------------------
    
    extracted_df$point_id <- ave(rep(1, nrow(extracted_df)), extracted_df$polygon, FUN = seq_along)
    
    if(nrow(roughness_df) > 0) {
      roughness_df$point_id <- ave(rep(1, nrow(roughness_df)), roughness_df$polygon, FUN = seq_along)
      combined_df <- merge(extracted_df, roughness_df, by = c("polygon", "point_id"), all = TRUE)
    } else {
      combined_df <- extracted_df
      combined_df$roughness <- NA
    }
    
    # -------------------------------------------------------------------------
    # LAYERING
    # -------------------------------------------------------------------------
    
    cat("  Reading layering data...\n")
    
    layering_file <- paste0(base_dir, "layering csv/", file_id, 
                            "_Subsampling_Remove Outliers_Normalize by Ground Points_Convert to ASCII.txt_xyLayering.csv")
    
    if(!file.exists(layering_file)) {
      cat("  WARNING: Layering file not found\n")
      final_df <- combined_df
      final_df$ENL0D <- NA
      final_df$ENL1 <- NA
      final_df$ENL2D <- NA
    } else {
      layering <- read.csv(layering_file, sep = ";", dec = ",", header = TRUE)
      colnames(layering) <- c("x", "y", "ENL0D", "ENL1", "ENL2D")
      
      layering_sf <- st_as_sf(layering, coords = c("x", "y"), crs = crs_to_use)
      
      layering_extracted <- list()
      for(polygon_name in names(ud_sf_list)) {
        geom <- ud_sf_list[[polygon_name]]
        points_in_poly <- suppressWarnings(st_intersection(layering_sf, st_sf(geometry = geom)))
        
        if(nrow(points_in_poly) > 0) {
          layering_extracted[[polygon_name]] <- data.frame(
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
      
      cat("  Extracted", nrow(layering_df), "layering points\n")
    }
    
    # -------------------------------------------------------------------------
    # FILLING 1-10
    # -------------------------------------------------------------------------
    
    cat("  Reading filling (1-10) data...\n")
    
    filling_file <- paste0(base_dir, "filling csv/", file_id, 
                           "_Subsampling_Remove Outliers_Normalize by Ground Points_Convert to ASCII.txt_xyFilling_layer1-10.csv")
    
    if(!file.exists(filling_file)) {
      cat("  WARNING: Filling 1-10 file not found\n")
      final_df_2 <- final_df
    } else {
      filling <- read.csv(filling_file, sep = ",", dec = ".", header = TRUE)
      
      filling_sf <- st_as_sf(filling, coords = c("x", "y"), crs = crs_to_use)
      
      filling_extracted <- list()
      for(polygon_name in names(ud_sf_list)) {
        geom <- ud_sf_list[[polygon_name]]
        points_in_poly <- suppressWarnings(st_intersection(filling_sf, st_sf(geometry = geom)))
        
        if(nrow(points_in_poly) > 0) {
          filling_extracted[[polygon_name]] <- st_drop_geometry(points_in_poly)
        }
      }
      
      filling_df_list <- lapply(names(filling_extracted), function(name) {
        data.frame(polygon = name, filling_extracted[[name]])
      })
      filling_df <- do.call(rbind, filling_df_list)
      filling_df$point_id <- ave(rep(1, nrow(filling_df)), filling_df$polygon, FUN = seq_along)
      
      final_df_2 <- merge(final_df, filling_df, by = c("polygon", "point_id"), all = TRUE)
      
      cat("  Extracted", nrow(filling_df), "filling (1-10) points\n")
    }
    
    # -------------------------------------------------------------------------
    # FILLING 11-20
    # -------------------------------------------------------------------------
    
    cat("  Reading filling (11-20) data...\n")
    
    filling2_file <- paste0(base_dir, "filling2 csv/", file_id, 
                            "_Subsampling_Remove Outliers_Normalize by Ground Points_Convert to ASCII.txt_xyFilling_layer11-20.csv")
    
    if(!file.exists(filling2_file)) {
      cat("  WARNING: Filling 11-20 file not found\n")
      filling_final_df <- final_df_2
    } else {
      filling2 <- read.csv(filling2_file, sep = ",", dec = ".", header = TRUE)
      
      filling2_sf <- st_as_sf(filling2, coords = c("x", "y"), crs = crs_to_use)
      
      filling2_extracted <- list()
      for(polygon_name in names(ud_sf_list)) {
        geom <- ud_sf_list[[polygon_name]]
        points_in_poly <- suppressWarnings(st_intersection(filling2_sf, st_sf(geometry = geom)))
        
        if(nrow(points_in_poly) > 0) {
          filling2_extracted[[polygon_name]] <- st_drop_geometry(points_in_poly)
        }
      }
      
      filling2_df_list <- lapply(names(filling2_extracted), function(name) {
        data.frame(polygon = name, filling2_extracted[[name]])
      })
      filling2_df <- do.call(rbind, filling2_df_list)
      filling2_df$point_id <- ave(rep(1, nrow(filling2_df)), filling2_df$polygon, FUN = seq_along)
      
      filling_final_df <- merge(final_df_2, filling2_df, by = c("polygon", "point_id"), all = TRUE)
      
      cat("  Extracted", nrow(filling2_df), "filling (11-20) points\n")
    }
    
    # -------------------------------------------------------------------------
    # Store result
    # -------------------------------------------------------------------------
    
    all_ud_data[[ud_name]] <- filling_final_df
    
    cat("✓ Successfully completed", ud_name, "\n")
    cat("  Total points extracted:", nrow(filling_final_df), "\n\n")
    
  }, error = function(e) {
    cat("✗ ERROR processing", ud_name, ":", conditionMessage(e), "\n\n")
  })
}

# =============================================================================
# SAVE RESULTS
# =============================================================================

cat("========================================\n")
cat("SAVING RESULTS\n")
cat("========================================\n")

# Save individual UD extractions
for(ud_name in names(all_ud_data)) {
  output_file <- paste0(base_dir, "extracted_data_", ud_name, ".csv")
  write.csv(all_ud_data[[ud_name]], output_file, row.names = FALSE)
  cat("Saved:", output_file, "\n")
}

# Combine all UDs into one big dataframe
if(length(all_ud_data) > 0) {
  final_all_uds <- do.call(rbind, all_ud_data)
  combined_file <- paste0(base_dir, "extracted_data_all_UDs_combined.csv")
  write.csv(final_all_uds, combined_file, row.names = FALSE)
  cat("Saved combined file:", combined_file, "\n")
}

# =============================================================================
# CALCULATE SUMMARY STATISTICS
# =============================================================================

cat("\n========================================\n")
cat("CALCULATING SUMMARY STATISTICS\n")
cat("========================================\n")

if(length(all_ud_data) > 0) {
  
  # Get all column names from the first dataset to build summary dynamically
  sample_data <- all_ud_data[[1]]
  numeric_cols <- names(sample_data)[sapply(sample_data, is.numeric)]
  numeric_cols <- numeric_cols[!numeric_cols %in% c("point_id")]
  
  # Calculate summary statistics for each polygon
  polygon_summaries <- final_all_uds %>%
    group_by(polygon) %>%
    summarise(
      across(all_of(numeric_cols), 
             list(mean = ~mean(.x, na.rm = TRUE),
                  sd = ~sd(.x, na.rm = TRUE),
                  median = ~median(.x, na.rm = TRUE),
                  min = ~min(.x, na.rm = TRUE),
                  max = ~max(.x, na.rm = TRUE)),
             .names = "{.col}_{.fn}"),
      n_points = n()
    )
  
  # Save summary statistics
  summary_file <- paste0(base_dir, "summary_statistics_UDs_", 
                         format(Sys.Date(), "%Y%m%d"), ".csv")
  write.csv(polygon_summaries, summary_file, row.names = FALSE)
  cat("Saved summary statistics:", summary_file, "\n")
  
  # Print a preview
  cat("\nSummary preview (first few rows):\n")
  print(head(polygon_summaries[, 1:min(8, ncol(polygon_summaries))]))
  
} else {
  cat("No data to summarize.\n")
}

