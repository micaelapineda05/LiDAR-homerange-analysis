library(dplyr)
library(lubridate)
library(tidyverse)
library(FNN)

# Create lookup with character PITnum
pit_plot_lookup_clean <- joined_df %>%
  st_drop_geometry() %>%
  distinct(PITnum, plot_id) %>%
  mutate(PITnum = as.character(PITnum))

# Add plot_id to each dataframe
for(pit_name in names(all_ud_data)) {
  
  plot_id <- pit_plot_lookup_clean$plot_id[pit_plot_lookup_clean$PITnum == pit_name]
  
  if(length(plot_id) > 0) {
    all_ud_data[[pit_name]]$PITnum <- pit_name
    all_ud_data[[pit_name]]$plot_id <- plot_id[1]
  } else {
    all_ud_data[[pit_name]]$PITnum <- pit_name
    all_ud_data[[pit_name]]$plot_id <- NA
  }
}

####RSF for March

####----------------------------------------------
### Creating RSF with KDE-weighted points
###-----------------------------------------------
library(dplyr)
library(sf)
library(FNN)

###----------------------------------------------
### Extract UD density values using ctmm telemetry objects
###Only do this once then use the output for July
###-----------------------------------------------

# Convert dat to telemetry objects
# First, convert dat.ok to telemetry objects
dat_telemetry <- lapply(dat.ok, function(x) {
  try(as.telemetry(x), silent = TRUE)
})

# Filter out errors
dat_telemetry_success <- dat_telemetry[!sapply(dat_telemetry, function(x) inherits(x, "try-error"))]

cat("Successfully created", length(dat_telemetry_success), "telemetry objects\n")
cat("Failed:", length(dat_telemetry) - length(dat_telemetry_success), "\n")

# Get only the successful UDs from res that match
res_ok <- res[!sapply(res, is, "try-error")]
res_ok <- res_ok[sapply(res_ok, function(x) inherits(x, "UD"))]

cat("UD objects available:", length(res_ok), "\n")

# Check what class res_ok contains
class(res_ok)
length(res_ok)

# Check the first UD object
class(res_ok[[1]])
class(res_ok[[1]]$PDF)

# Check the telemetry list
length(dat_telemetry_success)
names(dat_telemetry_success)

#####---------------------------------------------------------------
# Corrected function to extract UD density at telemetry locations
###-------------------------------------------------------------------
extract_ud_densities_from_telemetry <- function(pit_name, ud_object, telemetry_list) {
  
  cat("Processing:", pit_name, "\n")
  
  # Find the matching telemetry object
  matching_telemetry <- NULL
  
  for(i in seq_along(telemetry_list)) {
    tel_obj <- telemetry_list[[i]]
    if(!is.null(tel_obj@info$identity) && 
       as.character(tel_obj@info$identity) == as.character(pit_name)) {
      matching_telemetry <- tel_obj
      cat("  Found matching telemetry object\n")
      break
    }
  }
  
  if(is.null(matching_telemetry)) {
    cat("  WARNING: No telemetry object found for", pit_name, "\n")
    return(NULL)
  }
  
  cat("  Extracting density for", nrow(matching_telemetry), "locations\n")
  
  # Extract the PDF matrix and spatial information from UD
  pdf_matrix <- ud_object$PDF
  r_grid <- ud_object$r  # This contains the x and y grid coordinates
  
  # Get telemetry coordinates (in the projected space)
  tel_x <- matching_telemetry$x
  tel_y <- matching_telemetry$y
  
  # Use bilinear interpolation to get density values at telemetry locations
  # We'll use the fields package or a simple approach
  library(fields)
  
  # Create interpolation object
  # r_grid$x and r_grid$y contain the grid coordinates
  # pdf_matrix contains the density values
  density_values <- interp.surface(
    obj = list(
      x = r_grid$x,
      y = r_grid$y,
      z = pdf_matrix
    ),
    loc = cbind(tel_x, tel_y)
  )
  
  # Create dataframe with all info
  result <- data.frame(
    PITnum = pit_name,
    x = matching_telemetry$x,
    y = matching_telemetry$y,
    longitude = matching_telemetry$longitude,
    latitude = matching_telemetry$latitude,
    timestamp = matching_telemetry$timestamp,
    ud_density = density_values
  )
  
  cat("  Extracted", sum(!is.na(density_values)), "non-NA density values\n")
  
  return(result)
}

# Now run the extraction loop again
telemetry_with_density <- list()
for(i in seq_along(res_ok)) {
  
  if(!inherits(res_ok[[i]], "UD")) {
    cat("Skipping", i, "- not a UD object\n")
    next
  }
  
  pit_name <- as.character(res_ok[[i]]@info$identity)
  
  cat("\n=== Processing individual", i, ":", pit_name, "===\n")
  
  telemetry_with_density[[pit_name]] <- extract_ud_densities_from_telemetry(
    pit_name, 
    res_ok[[i]], 
    dat_telemetry_success
  )
}

# Check results
cat("\n=== Summary ===\n")
cat("Total individuals processed:", length(telemetry_with_density), "\n")
cat("Individuals with data:", sum(sapply(telemetry_with_density, function(x) !is.null(x))), "\n")

# Function to extract all cells within a UD contour with their densities
extract_ud_cells_with_weights <- function(ud_object, habitat_data, contour_level = 0.95) {
  
  # Get the PDF matrix and grid
  pdf_matrix <- ud_object$PDF
  r_grid <- ud_object$r
  
  # Create a data frame of all grid cells
  grid_df <- expand.grid(
    x = r_grid$x,
    y = r_grid$y
  )
  grid_df$ud_density <- as.vector(pdf_matrix)
  
  # Get the CDF (cumulative distribution) to identify contour levels
  cdf_matrix <- ud_object$CDF
  grid_df$ud_cdf <- as.vector(cdf_matrix)
  
  # Keep only cells within the specified contour (e.g., 95%)
  grid_df <- grid_df[grid_df$ud_cdf <= contour_level, ]
  
  # Remove NA densities
  grid_df <- grid_df[!is.na(grid_df$ud_density), ]
  
  # Match to nearest habitat cells
  nn_habitat <- get.knnx(
    data = habitat_data[, c("x", "y")],
    query = grid_df[, c("x", "y")],
    k = 1
  )
  
  # Get habitat values for these cells
  habitat_cells <- habitat_data[nn_habitat$nn.index[,1], ]
  habitat_cells$ud_density <- grid_df$ud_density
  habitat_cells$weight <- grid_df$ud_density
  
  return(habitat_cells)
}

####--------------------------
####Need extract_ud_cells_with_weights for July RSF
#####----------------------------

###########
##### start here for rsf
###########

# Set your base directory (adjust this path)
base_dir <- "C:/Users/PinedaMicaelaTonatsi/Documents/LiDAR Collab/New_2026_normalized/wetransfer_results_2026-02-06/March/"

rsf_data_weighted_cells_list <- list()

# Get plot names from all_ud_data
plot_names <- unique(sapply(all_ud_data, function(x) unique(x$plot_id)[1]))

for(plot_name in plot_names) {
  
  cat("Processing weighted cell RSF data for", plot_name, "\n")
  
  plot_num <- sub("^Plot_", "", plot_name)
  
  # Load habitat data
  cover <- read.csv(paste0(base_dir, "canopy cover csv/", plot_num, "_Subsampling_Remove Outliers_Normalize by Ground Points_Convert to ASCII.txt_xyCanopycover.csv"), 
                    sep = ";", dec = ",", header = TRUE,
                    col.names = c("x", "y", "canopy_cover"), fill = TRUE)
  cover$x <- as.numeric(gsub("[^0-9.-]", "", cover$x))
  cover$y <- as.numeric(gsub("[^0-9.-]", "", cover$y))
  
  roughness <- read.csv(paste0(base_dir, "roughness csv/", plot_num, "_Subsampling_Remove Outliers_Normalize by Ground Points_Convert to ASCII.txt_xyRauigkeiten.csv"), sep = ";", dec = ",")
  roughness$x <- as.numeric(gsub("[^0-9.-]", "", roughness$x))
  roughness$y <- as.numeric(gsub("[^0-9.-]", "", roughness$y))
  
  layering <- read.csv(paste0(base_dir, "layering csv/", plot_num, "_Subsampling_Remove Outliers_Normalize by Ground Points_Convert to ASCII.txt_xyLayering.csv"), sep = ";", dec = ",")
  layering$x <- as.numeric(gsub("[^0-9.-]", "", layering$x))
  layering$y <- as.numeric(gsub("[^0-9.-]", "", layering$y))
  
  filling_file <- paste0(
    base_dir,
    "filling csv/",
    plot_num,
    "_Subsampling_Remove Outliers_Normalize by Ground Points_Convert to ASCII.txt_xyFilling_layer1-10.csv"
  )
  
  if(file.exists(filling_file)) {
    
    filling <- read.csv(filling_file, sep = ",", dec = ".")
    
    filling$x <- as.numeric(gsub("[^0-9.-]", "", filling$x))
    filling$y <- as.numeric(gsub("[^0-9.-]", "", filling$y))
    
  } else {
    
    cat("WARNING: Filling file missing for", plot_name, "- skipping plot\n")
    next
  }
  
  
  filling2 <- read.csv(paste0(base_dir, "filling2 csv/", plot_num, "_Subsampling_Remove Outliers_Normalize by Ground Points_Convert to ASCII.txt_xyFilling_layer11-20.csv"))
                       
  if(file.exists(filling_file)) {
                         
                         filling2 <- read.csv(filling2_file, sep = ",", dec = ".")
                         
                         filling2$x <- as.numeric(gsub("[^0-9.-]", "", filling2$x))
                         filling2$y <- as.numeric(gsub("[^0-9.-]", "", filling2$y))
                         
                       } else {
                         
                         cat("WARNING: Filling2 file missing for", plot_name, "- skipping plot\n")
                         next   # skips to next plot
                       }
  
  # Merge habitat
  all_habitat <- cover %>%
    left_join(roughness, by = c("x", "y")) %>%
    left_join(layering, by = c("x", "y")) %>%
    left_join(filling, by = c("x", "y")) %>%
    left_join(filling2, by = c("x", "y"))
  
  # Get individuals belonging to this plot
  plot_inds <- names(all_ud_data)[
    sapply(all_ud_data, function(x) plot_name %in% x$plot_id)
  ]
  
  
  plot_rsf_data <- list()
  
  for(pit_num in plot_inds) {
    
    if(!pit_num %in% names(res_ok)) {
      cat("WARNING: No UD object for", pit_num, "\n")
      next
    }
    
    cat("Processing", pit_num, "\n")
    
    ud_obj <- res_ok[[pit_num]]
    
    used_cells <- extract_ud_cells_with_weights(
      ud_object = ud_obj,
      habitat_data = all_habitat,
      contour_level = 0.95
    )
    
    used_cells$weight <- used_cells$weight / mean(used_cells$weight, na.rm = TRUE)
    
    used_cells$used <- 1
    used_cells$individualID <- pit_num
    used_cells$polygon <- pit_num
    
    # Subsample
    max_cells <- 1000
    if(nrow(used_cells) > max_cells) {
      set.seed(123)
      sample_prob <- used_cells$weight / sum(used_cells$weight)
      keep_idx <- sample(nrow(used_cells), max_cells, prob = sample_prob)
      used_cells <- used_cells[keep_idx, ]
    }
    
    # Available cells
    n_available <- nrow(used_cells)
    set.seed(123)
    available_idx <- sample(nrow(all_habitat), n_available)
    available_cells <- all_habitat[available_idx, ]
    
    available_cells$used <- 0
    available_cells$individualID <- pit_num
    available_cells$polygon <- pit_num
    available_cells$weight <- 1
    
    common_cols <- intersect(names(used_cells), names(available_cells))
    
    plot_rsf_data[[pit_num]] <- rbind(
      used_cells[, common_cols],
      available_cells[, common_cols]
    )
  }
  
  if(length(plot_rsf_data) > 0) {
    rsf_data_weighted_cells_list[[plot_name]] <- do.call(rbind, plot_rsf_data)
  }
  
  cat("Completed", plot_name, "\n\n")
}
####------------------------------------------------
###Test roughness first
####--------------------------------------------------
library(lme4)

rsf_data_weighted <- dplyr::bind_rows(
  rsf_data_weighted_cells_list,
  .id = "plot"
)

rsf_data_weighted <- rsf_data_weighted %>%
  mutate(plot = sub("Plot_", "", plot))

rsf_roughness_only_march <- glmer(
  used ~ scale(roughness) +
    (1 | individualID),
  data = rsf_data_weighted,
  family = binomial,
  weights = weight,
  control = glmerControl(optimizer = "bobyqa")
)

summary(rsf_roughness_only_march)

rsf_data_weighted$pred <- predict(rsf_roughness_only_march, type = "response")
hist(rsf_data_weighted$pred)

library(ggplot2)
ggplot(rsf_data_weighted, aes(x = roughness, y = used)) +
  geom_jitter(height = 0.02, alpha = 0.2) +
  stat_smooth(method = "glm", method.args = list(family = "binomial"), color = "blue")


library(ggeffects)

# Get predictions WITH bias correction (recommended)
pred_rough_march <- ggpredict(
  rsf_roughness_only_march,
  terms = "roughness [all]",
  bias_correction = TRUE
)

# Create the plot
ggplot(pred_rough_march, aes(x = x, y = predicted)) +
  geom_ribbon(
    aes(ymin = conf.low, ymax = conf.high),
    fill = "grey80",
    alpha = 0.6
  ) +
  geom_line(
    linewidth = 1.2,
    color = "black"
  ) +
  labs(
    x = "Understory roughness (scaled)",
    y = "Predicted probability of selection"
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    panel.border = element_blank()
  )
