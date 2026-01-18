library(ctmm)
library(sf)
library(dplyr)

######-------------------------------
##Create AKDE
#####--------------------------------

# Step 1: Prepare your data for ctmm
# ctmm needs: individual.local.identifier, timestamp, location-long, location-lat

# Assuming your capture data has: individualID, x, y, timestamp
# You'll need to convert your coordinates to lat/long if they're not already

capture_ctmm <- joined_df %>%
  rename(
    `individual.local.identifier` = PITnum,
    timestamp = Date,  # Replace with your date column name
    `location-long` = laser_stake_x,
    `location-lat` = laser_stake_y
  ) %>%
  mutate(timestamp = as.POSIXct(timestamp))  # Ensure proper datetime format

# Step 2: Clean your data - remove or aggregate duplicate timestamps
capture_ctmm_clean <- capture_ctmm %>%
  group_by(`individual.local.identifier`, timestamp) %>%
  # Take the mean coordinates if there are duplicates at same time
  summarize(
    `location-long` = mean(`location-long`, na.rm = TRUE),
    `location-lat` = mean(`location-lat`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Sort by individual and time
  arrange(`individual.local.identifier`, timestamp)

# Check how many duplicates were removed
cat("Original rows:", nrow(capture_ctmm), "\n")
cat("After removing duplicates:", nrow(capture_ctmm_clean), "\n")
cat("Duplicates removed:", nrow(capture_ctmm) - nrow(capture_ctmm_clean), "\n")


# Step 3: Convert to telemetry object
telem <- as.telemetry(capture_ctmm_clean)

# Step 4: Check sample sizes per individual
sample_sizes <- sapply(telem, nrow)
cat("\nSample size summary:\n")
print(summary(sample_sizes))
cat("\nIndividuals with < 3 observations:", sum(sample_sizes < 3), "\n")

###Data:2–4 locations per individual
####Often same trap locations reused
####Long time gaps

# Step 5: Fit AKDE only for individuals with enough data
# The paper suggests AKDE needs ~5-9 observations minimum
min_obs <- 3
akde_results <- list()
akde_areas <- data.frame()
failed_individuals <- character()

for(i in 1:length(telem)) {
  individual_id <- names(telem)[i]
  n_obs <- nrow(telem[[i]])
  
  cat("Processing individual:", individual_id, "with", n_obs, "observations\n")
  
  # Skip if too few observations
  if(n_obs < min_obs) {
    cat("  Skipping - too few observations\n")
    failed_individuals <- c(failed_individuals, individual_id)
    next
  }
  
  tryCatch({
    # Fit variogram (movement model)
    # For trap data, use IID model
    GUESS <- ctmm.guess(telem[[i]], interactive = FALSE)
    
    # Fit the model
    FIT <- ctmm.fit(telem[[i]], GUESS)
    
    # Calculate AKDE
    akde <- akde(telem[[i]], FIT)
    
    # Store results
    akde_results[[individual_id]] <- akde
    
    # Extract 95% area (in m²)
    area_summary <- summary(akde)
    area_95 <- area_summary$CI[2, "est"]  # 95% area estimate
    area_95_low <- area_summary$CI[2, "low"]
    area_95_high <- area_summary$CI[2, "high"]
    
    akde_areas <- rbind(akde_areas, data.frame(
      individualID = individual_id,
      area_akde_95 = area_95,
      area_akde_95_low = area_95_low,
      area_akde_95_high = area_95_high,
      n_observations = n_obs
    ))
    
    cat("  Success! Area:", round(area_95, 2), "m²\n")
    
  }, error = function(e) {
    cat("  Error:", e$message, "\n")
    failed_individuals <<- c(failed_individuals, individual_id)
  })
}

# Print summary
cat("\n=== AKDE Summary ===\n")
cat("Successfully processed:", length(akde_results), "individuals\n")
cat("Failed:", length(failed_individuals), "individuals\n")
cat("Too few observations (<", min_obs, "):", sum(sample_sizes < min_obs), "\n")

# View results
print(akde_areas)

area_summary <- summary(akde)
area_95 <- area_summary$CI[2, "est"]

summary(akde)

str(summary(akde))


# Step 6: Compare AKDE to MCP for individuals with both
comparison <- akde_areas %>%
  left_join(mcp_geoms %>% 
              st_drop_geometry() %>%
              select(id, area) %>%
              rename(individualID = id, area_mcp = area),
            by = "individualID") %>%
  mutate(
    ratio_akde_mcp = area_akde_95 / area_mcp,
    difference_m2 = area_akde_95 - area_mcp
  )

print(comparison)

# Summary statistics
cat("\n=== AKDE vs MCP Comparison ===\n")
cat("Mean AKDE/MCP ratio:", round(mean(comparison$ratio_akde_mcp, na.rm = TRUE), 2), "\n")
cat("Median AKDE/MCP ratio:", round(median(comparison$ratio_akde_mcp, na.rm = TRUE), 2), "\n")
cat("AKDE typically larger:", sum(comparison$area_akde_95 > comparison$area_mcp, na.rm = TRUE), "\n")
cat("MCP typically larger:", sum(comparison$area_akde_95 < comparison$area_mcp, na.rm = TRUE), "\n")

# Step 7: Convert AKDE to sf polygons for mapping
akde_sf_list <- list()

for(i in 1:length(akde_results)) {
  individual_id <- names(akde_results)[i]
  
  tryCatch({
    # Extract 95% contour as SpatialPolygonsDataFrame
    contour <- SpatialPolygonsDataFrame.UD(akde_results[[i]], level.UD = 0.95)
    
    # Convert to sf
    akde_sf <- st_as_sf(contour)
    akde_sf$individualID <- individual_id
    akde_sf$area <- akde_areas$area_akde_95[akde_areas$individualID == individual_id]
    
    akde_sf_list[[i]] <- akde_sf
  }, error = function(e) {
    cat("Failed to convert", individual_id, "to sf:", e$message, "\n")
  })
}

# Combine all individuals
if(length(akde_sf_list) > 0) {
  akde_all_sf <- do.call(rbind, akde_sf_list)
  cat("\nSuccessfully created sf polygons for", nrow(akde_all_sf), "individuals\n")
}

# Step 8: Visualize one example
if(length(akde_results) > 0) {
  example_id <- names(akde_results)[1]
  
  par(mfrow = c(1, 1))
  plot(telem[[example_id]], 
       main = paste("AKDE vs MCP for", example_id))
  plot(akde_results[[example_id]], 
       level.UD = c(0.50, 0.95),  # 50% and 95% contours
       col.level = c("#FF000080", "#0000FF80"),
       add = TRUE)
  
  # Add MCP if exists
  mcp_example <- mcp_all_sf %>% filter(id == example_id)
  if(nrow(mcp_example) > 0) {
    plot(st_geometry(mcp_example), 
         border = "green", 
         lwd = 2, 
         add = TRUE)
  }
  
  legend("topright", 
         legend = c("AKDE 50%", "AKDE 95%", "Observations", "MCP"),
         col = c("red", "blue", "black", "green"),
         lty = c(1, 1, NA, 1),
         pch = c(NA, NA, 16, NA),
         lwd = 2)
}

###--------------------------------
####Trying with one individual first
##---------------------------------
library(adehabitatHR)
library(sp)
library(raster)
library(dplyr)

# Remove geometry column
library(sf)
joined_df_2 <- st_drop_geometry(joined_df)


# If you want to create a new column called "plot_pitnum"
joined_df_2 <- joined_df_2 %>%
  mutate(pitnum_rounded = round(PITnum, 2),
         plot_pitnum = paste(plot_id_clean, pitnum_rounded, sep = "_")
  )

coords <- joined_df_2[, c("laser_stake_x", "laser_stake_y")]

# Remove rows with NA coordinates
joined_df_clean <- joined_df_2 %>%
  filter(!is.na(laser_stake_x), !is.na(laser_stake_y))

# Check how many rows were removed
cat("Original rows:", nrow(joined_df_2), "\n")
cat("Clean rows:", nrow(joined_df_clean), "\n")
cat("Removed:", nrow(joined_df_2) - nrow(joined_df_clean), "rows with NA coordinates\n")

# Now create the SpatialPointsDataFrame
coords <- cbind(joined_df_clean$x, joined_df_clean$y)

spdf <- SpatialPointsDataFrame(
  coords = coords,
  data = joined_df_clean,
  proj4string = CRS("+proj=utm +zone=31 +ellps=WGS84")
)

# Verify it worked
summary(spdf)

spdf <- SpatialPointsDataFrame(
  coords = coords,
  data = joined_df_clean,
  proj4string = CRS("+proj=utm +zone=31 +ellps=WGS84")
)

kde_all <- kernelUD(spdf[, "plot_pitnum"], 
                    h = "href",  # Bandwidth selection method
                    grid = 500,
                    extent = 0.5)
