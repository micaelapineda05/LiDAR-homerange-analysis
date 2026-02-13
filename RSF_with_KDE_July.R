###########
##### start here for rsf
###########

# Create lookup with character PITnum
pit_plot_lookup_clean <- joined_df %>%
  st_drop_geometry() %>%
  distinct(PITnum, plot_id) %>%
  mutate(PITnum = as.character(PITnum))

# Add plot_id to each dataframe
for(pit_name in names(all_ud_data_july)) {
  
  plot_id <- pit_plot_lookup_clean$plot_id[pit_plot_lookup_clean$PITnum == pit_name]
  
  if(length(plot_id) > 0) {
    all_ud_data_july[[pit_name]]$PITnum <- pit_name
    all_ud_data_july[[pit_name]]$plot_id <- plot_id[1]
  } else {
    all_ud_data_july[[pit_name]]$PITnum <- pit_name
    all_ud_data_july[[pit_name]]$plot_id <- NA
  }
}

###--------------------------------
###Need res_ok for this to work!!
###---------------------------------

# Set your base directory (adjust this path)
base_dir <- "C:/Users/PinedaMicaelaTonatsi/Documents/LiDAR Collab/New_2026_normalized/wetransfer_results_2026-02-06/July/"


rsf_data_weighted_cells_list_july <- list()

# Get plot names from all_ud_data
plot_names <- unique(sapply(all_ud_data_july, function(x) unique(x$plot_id)[1]))

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
  plot_inds <- names(all_ud_data_july)[
    sapply(all_ud_data_july, function(x) plot_name %in% x$plot_id)
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
    rsf_data_weighted_cells_list_july[[plot_name]] <- do.call(rbind, plot_rsf_data)
  }
  
  cat("Completed", plot_name, "\n\n")
}
####------------------------------------------------
###Test roughness first
####--------------------------------------------------

library(lme4)
library(lubridate)
library(dplyr)

rsf_data_weighted_july <- rsf_data_weighted_cells_list_july %>%
  mutate(plot = sub("Plot_", "", plot))

rsf_roughness_only <- glmer(
  used ~ scale(roughness) +
    (1 | individualID),
  data = rsf_data_weighted_july,
  family = binomial,
  weights = weight,
  control = glmerControl(optimizer = "bobyqa")
)

summary(rsf_roughness_only)

rsf_data_weighted_july$pred <- predict(rsf_roughness_only, type = "response")
hist(rsf_data_weighted_july$pred)

library(ggplot2)
ggplot(rsf_data_weighted_july, aes(x = roughness, y = used)) +
  geom_jitter(height = 0.02, alpha = 0.2) +
  stat_smooth(method = "glm", method.args = list(family = "binomial"), color = "blue")

library(ggeffects)

# Get predictions WITH bias correction (recommended)
pred_rough <- ggpredict(
  rsf_roughness_only,
  terms = "roughness [all]",
  bias_correction = TRUE
)

# Create the plot
ggplot(pred_rough, aes(x = x, y = predicted)) +
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
