library(sf)
library(terra)
library(dplyr)
library(FNN)
####----------------------------------------------
###Creating rsf for March
###-----------------------------------------------
# Function to create bounding box from habitat data
create_study_area_bbox <- function(habitat_df) {
  xmin <- min(habitat_df$x, na.rm = TRUE)
  xmax <- max(habitat_df$x, na.rm = TRUE)
  ymin <- min(habitat_df$y, na.rm = TRUE)
  ymax <- max(habitat_df$y, na.rm = TRUE)
  
  # Create polygon from bounding box
  bbox_polygon <- st_polygon(list(matrix(
    c(xmin, ymin, 
      xmax, ymin, 
      xmax, ymax, 
      xmin, ymax, 
      xmin, ymin), 
    ncol = 2, byrow = TRUE
  )))
  
  return(st_sfc(bbox_polygon))
}

# Now use this in your RSF loop:
rsf_data_list <- list()

for(plot_name in names(all_plots_data)) {
  
  cat("Processing RSF data for", plot_name, "\n")
  
  plot_num <- sub("Plot_", "", plot_name)
  
  # Load habitat data
  cover <- read.csv(paste0(base_dir, "March canopy cover csv/", plot_num, ".txt_xyCanopycover.csv"), 
                    sep = ";", dec = ",", header = TRUE,
                    col.names = c("x", "y", "canopy_cover"), fill = TRUE)
  cover$x <- as.numeric(gsub("[^0-9.-]", "", cover$x))
  cover$y <- as.numeric(gsub("[^0-9.-]", "", cover$y))
  
  roughness <- read.csv(paste0(base_dir, "March roughness csv/", plot_num, ".txt_xyRauigkeiten.csv"), sep = ",")
  roughness$x <- as.numeric(gsub("[^0-9.-]", "", roughness$x))
  roughness$y <- as.numeric(gsub("[^0-9.-]", "", roughness$y))
  
  layering <- read.csv(paste0(base_dir, "March layering csv/", plot_num, ".txt_xyLayering.csv"), sep = ",")
  layering$x <- as.numeric(gsub("[^0-9.-]", "", layering$x))
  layering$y <- as.numeric(gsub("[^0-9.-]", "", layering$y))
  
  filling <- read.csv(paste0(base_dir, "March filling csv/", plot_num, ".txt_xyFilling_layer1-10.csv"), sep = ",")
  filling$x <- as.numeric(gsub("[^0-9.-]", "", filling$x))
  filling$y <- as.numeric(gsub("[^0-9.-]", "", filling$y))
  
  filling2 <- read.csv(paste0(base_dir, "March filling2 csv/", plot_num, ".txt_xyFilling_layer11-20.csv"), sep = ",")
  filling2$x <- as.numeric(gsub("[^0-9.-]", "", filling2$x))
  filling2$y <- as.numeric(gsub("[^0-9.-]", "", filling2$y))
  
  # Create study area
  study_area <- create_study_area_bbox(cover)
  st_crs(study_area) <- st_crs(plot_lists[[plot_name]][[1]])
  
  unique_polygons <- unique(all_plots_data[[plot_name]]$polygon)
  
  plot_rsf_data <- list()
  
  for(poly_name in unique_polygons) {
    
    # Get ALL used points for this individual
    used_pts_full <- all_plots_data[[plot_name]][all_plots_data[[plot_name]]$polygon == poly_name, ]
    
    # *** SUBSAMPLE USED POINTS ***
    # Set a maximum number of points per individual (e.g., 200-500)
    max_points_per_individual <- 500
    
    if(nrow(used_pts_full) > max_points_per_individual) {
      # Randomly sample
      set.seed(123)  # For reproducibility
      used_pts <- used_pts_full[sample(nrow(used_pts_full), max_points_per_individual), ]
    } else {
      used_pts <- used_pts_full
    }
    
    used_pts$used <- 1
    used_pts$individualID <- poly_name
    
    # Generate EQUAL number of available points (1:1 ratio)
    n_available <- nrow(used_pts)
    
    cat("  ", poly_name, ": ", nrow(used_pts), " used, ", n_available, " available\n")
    
    available_pts_geom <- st_sample(st_sf(geometry = study_area), size = n_available)
    available_coords <- st_coordinates(available_pts_geom)
    
    available_habitat <- data.frame(
      x = available_coords[,1],
      y = available_coords[,2]
    )
    
    # Extract habitat values
    nn_cover <- get.knnx(cover[, c("x", "y")], available_habitat[, c("x", "y")], k = 1)
    available_habitat$canopy_cover <- cover$canopy_cover[nn_cover$nn.index[,1]]
    
    nn_rough <- get.knnx(roughness[, c("x", "y")], available_habitat[, c("x", "y")], k = 1)
    available_habitat$roughness <- roughness$roughness[nn_rough$nn.index[,1]]
    
    nn_layer <- get.knnx(layering[, c("x", "y")], available_habitat[, c("x", "y")], k = 1)
    available_habitat$ENL0D <- layering$ENL0D[nn_layer$nn.index[,1]]
    available_habitat$ENL1 <- layering$ENL1[nn_layer$nn.index[,1]]
    available_habitat$ENL2D <- layering$ENL2D[nn_layer$nn.index[,1]]
    
    nn_fill <- get.knnx(filling[, c("x", "y")], available_habitat[, c("x", "y")], k = 1)
    available_habitat$X0.25m <- filling$X0.25m[nn_fill$nn.index[,1]]
    available_habitat$X0.5m <- filling$X0.5m[nn_fill$nn.index[,1]]
    available_habitat$X0.75m <- filling$X0.75m[nn_fill$nn.index[,1]]
    available_habitat$X1m <- filling$X1m[nn_fill$nn.index[,1]]
    available_habitat$X1.25m <- filling$X1.25m[nn_fill$nn.index[,1]]
    available_habitat$X1.5m <- filling$X1.5m[nn_fill$nn.index[,1]]
    available_habitat$X1.75m <- filling$X1.75m[nn_fill$nn.index[,1]]
    available_habitat$X2m <- filling$X2m[nn_fill$nn.index[,1]]
    available_habitat$X2.25m <- filling$X2.25m[nn_fill$nn.index[,1]]
    available_habitat$X2.5m <- filling$X2.5m[nn_fill$nn.index[,1]]
    
    nn_fill2 <- get.knnx(filling2[, c("x", "y")], available_habitat[, c("x", "y")], k = 1)
    available_habitat$X2.75m <- filling2$X2.75m[nn_fill2$nn.index[,1]]
    available_habitat$X3m <- filling2$X3m[nn_fill2$nn.index[,1]]
    available_habitat$X3.25m <- filling2$X3.25m[nn_fill2$nn.index[,1]]
    available_habitat$X3.5m <- filling2$X3.5m[nn_fill2$nn.index[,1]]
    available_habitat$X3.75m <- filling2$X3.75m[nn_fill2$nn.index[,1]]
    available_habitat$X4m <- filling2$X4m[nn_fill2$nn.index[,1]]
    available_habitat$X4.25m <- filling2$X4.25m[nn_fill2$nn.index[,1]]
    available_habitat$X4.5m <- filling2$X4.5m[nn_fill2$nn.index[,1]]
    available_habitat$X4.75m <- filling2$X4.75m[nn_fill2$nn.index[,1]]
    available_habitat$X5m <- filling2$X5m[nn_fill2$nn.index[,1]]
    
    available_habitat$polygon <- poly_name
    available_habitat$individualID <- poly_name
    available_habitat$used <- 0
    
    # Combine
    common_cols <- intersect(names(used_pts), names(available_habitat))
    
    plot_rsf_data[[poly_name]] <- rbind(
      used_pts[, common_cols],
      available_habitat[, common_cols]
    )
  }
  
  rsf_data_list[[plot_name]] <- do.call(rbind, plot_rsf_data)
  cat("Completed", plot_name, "\n\n")
}

rsf_data_final_march <- do.call(rbind, rsf_data_list)
rsf_data_final_march$plot <- sub("^(Plot_[0-9]+\\.[0-9]+).*", "\\1", rsf_data_final_march$polygon)

# Check the new size
cat("Final dataset size:", nrow(rsf_data_final_march), "rows\n")
cat("Used vs Available:\n")
print(table(rsf_data_final_march$used))

# Check distribution per individual
rsf_data_final_march %>%
  group_by(individualID, used) %>%
  summarise(n = n()) %>%
  head(20)

# Add plot identifier
rsf_data_final_march$plot <- sub("^(Plot_[0-9]+\\.[0-9]+).*", "\\1", rsf_data_final_march$polygon)

####----------------------------------------------
###Creating rsf for July
###-----------------------------------------------

# Function to create bounding box from habitat data
create_study_area_bbox <- function(habitat_df) {
  xmin <- min(habitat_df$x, na.rm = TRUE)
  xmax <- max(habitat_df$x, na.rm = TRUE)
  ymin <- min(habitat_df$y, na.rm = TRUE)
  ymax <- max(habitat_df$y, na.rm = TRUE)
  
  # Create polygon from bounding box
  bbox_polygon <- st_polygon(list(matrix(
    c(xmin, ymin, 
      xmax, ymin, 
      xmax, ymax, 
      xmin, ymax, 
      xmin, ymin), 
    ncol = 2, byrow = TRUE
  )))
  
  return(st_sfc(bbox_polygon))
}

# Now use this in your RSF loop:
rsf_data_list <- list()

for(plot_name in names(all_plots_data)) {
  
  cat("Processing RSF data for", plot_name, "\n")
  
  plot_num <- sub("Plot_", "", plot_name)
  
  # Load habitat data
  cover <- read.csv(paste0(base_dir, "July canopy cover csv/", plot_num, ".txt_xyCanopycover.csv"), 
                    sep = ";", dec = ",", header = TRUE,
                    col.names = c("x", "y", "canopy_cover"), fill = TRUE)
  cover$x <- as.numeric(gsub("[^0-9.-]", "", cover$x))
  cover$y <- as.numeric(gsub("[^0-9.-]", "", cover$y))
  
  roughness <- read.csv(paste0(base_dir, "July roughness csv/", plot_num, ".txt_xyRauigkeiten.csv"), sep = ",")
  roughness$x <- as.numeric(gsub("[^0-9.-]", "", roughness$x))
  roughness$y <- as.numeric(gsub("[^0-9.-]", "", roughness$y))
  
  layering <- read.csv(paste0(base_dir, "July layering csv/", plot_num, ".txt_xyLayering.csv"), sep = ",")
  layering$x <- as.numeric(gsub("[^0-9.-]", "", layering$x))
  layering$y <- as.numeric(gsub("[^0-9.-]", "", layering$y))
  
  filling <- read.csv(paste0(base_dir, "July filling csv/", plot_num, ".txt_xyFilling_layer1-10.csv"), sep = ",")
  filling$x <- as.numeric(gsub("[^0-9.-]", "", filling$x))
  filling$y <- as.numeric(gsub("[^0-9.-]", "", filling$y))
  
  filling2 <- read.csv(paste0(base_dir, "July filling2 csv/", plot_num, ".txt_xyFilling_layer11-20.csv"), sep = ",")
  filling2$x <- as.numeric(gsub("[^0-9.-]", "", filling2$x))
  filling2$y <- as.numeric(gsub("[^0-9.-]", "", filling2$y))
  
  # Create study area
  study_area <- create_study_area_bbox(cover)
  st_crs(study_area) <- st_crs(plot_lists[[plot_name]][[1]])
  
  unique_polygons <- unique(all_plots_data[[plot_name]]$polygon)
  
  plot_rsf_data <- list()
  
  for(poly_name in unique_polygons) {
    
    # Get ALL used points for this individual
    used_pts_full <- all_plots_data[[plot_name]][all_plots_data[[plot_name]]$polygon == poly_name, ]
    
    # *** SUBSAMPLE USED POINTS ***
    # Set a maximum number of points per individual (e.g., 200-500)
    max_points_per_individual <- 500
    
    if(nrow(used_pts_full) > max_points_per_individual) {
      # Randomly sample
      set.seed(123)  # For reproducibility
      used_pts <- used_pts_full[sample(nrow(used_pts_full), max_points_per_individual), ]
    } else {
      used_pts <- used_pts_full
    }
    
    used_pts$used <- 1
    used_pts$individualID <- poly_name
    
    # Generate EQUAL number of available points (1:1 ratio)
    n_available <- nrow(used_pts)
    
    cat("  ", poly_name, ": ", nrow(used_pts), " used, ", n_available, " available\n")
    
    available_pts_geom <- st_sample(st_sf(geometry = study_area), size = n_available)
    available_coords <- st_coordinates(available_pts_geom)
    
    available_habitat <- data.frame(
      x = available_coords[,1],
      y = available_coords[,2]
    )
    
    # Extract habitat values
    nn_cover <- get.knnx(cover[, c("x", "y")], available_habitat[, c("x", "y")], k = 1)
    available_habitat$canopy_cover <- cover$canopy_cover[nn_cover$nn.index[,1]]
    
    nn_rough <- get.knnx(roughness[, c("x", "y")], available_habitat[, c("x", "y")], k = 1)
    available_habitat$roughness <- roughness$roughness[nn_rough$nn.index[,1]]
    
    nn_layer <- get.knnx(layering[, c("x", "y")], available_habitat[, c("x", "y")], k = 1)
    available_habitat$ENL0D <- layering$ENL0D[nn_layer$nn.index[,1]]
    available_habitat$ENL1 <- layering$ENL1[nn_layer$nn.index[,1]]
    available_habitat$ENL2D <- layering$ENL2D[nn_layer$nn.index[,1]]
    
    nn_fill <- get.knnx(filling[, c("x", "y")], available_habitat[, c("x", "y")], k = 1)
    available_habitat$X0.25m <- filling$X0.25m[nn_fill$nn.index[,1]]
    available_habitat$X0.5m <- filling$X0.5m[nn_fill$nn.index[,1]]
    available_habitat$X0.75m <- filling$X0.75m[nn_fill$nn.index[,1]]
    available_habitat$X1m <- filling$X1m[nn_fill$nn.index[,1]]
    available_habitat$X1.25m <- filling$X1.25m[nn_fill$nn.index[,1]]
    available_habitat$X1.5m <- filling$X1.5m[nn_fill$nn.index[,1]]
    available_habitat$X1.75m <- filling$X1.75m[nn_fill$nn.index[,1]]
    available_habitat$X2m <- filling$X2m[nn_fill$nn.index[,1]]
    available_habitat$X2.25m <- filling$X2.25m[nn_fill$nn.index[,1]]
    available_habitat$X2.5m <- filling$X2.5m[nn_fill$nn.index[,1]]
    
    nn_fill2 <- get.knnx(filling2[, c("x", "y")], available_habitat[, c("x", "y")], k = 1)
    available_habitat$X2.75m <- filling2$X2.75m[nn_fill2$nn.index[,1]]
    available_habitat$X3m <- filling2$X3m[nn_fill2$nn.index[,1]]
    available_habitat$X3.25m <- filling2$X3.25m[nn_fill2$nn.index[,1]]
    available_habitat$X3.5m <- filling2$X3.5m[nn_fill2$nn.index[,1]]
    available_habitat$X3.75m <- filling2$X3.75m[nn_fill2$nn.index[,1]]
    available_habitat$X4m <- filling2$X4m[nn_fill2$nn.index[,1]]
    available_habitat$X4.25m <- filling2$X4.25m[nn_fill2$nn.index[,1]]
    available_habitat$X4.5m <- filling2$X4.5m[nn_fill2$nn.index[,1]]
    available_habitat$X4.75m <- filling2$X4.75m[nn_fill2$nn.index[,1]]
    available_habitat$X5m <- filling2$X5m[nn_fill2$nn.index[,1]]
    
    available_habitat$polygon <- poly_name
    available_habitat$individualID <- poly_name
    available_habitat$used <- 0
    
    # Combine
    common_cols <- intersect(names(used_pts), names(available_habitat))
    
    plot_rsf_data[[poly_name]] <- rbind(
      used_pts[, common_cols],
      available_habitat[, common_cols]
    )
  }
  
  rsf_data_list[[plot_name]] <- do.call(rbind, plot_rsf_data)
  cat("Completed", plot_name, "\n\n")
}

rsf_data_final <- do.call(rbind, rsf_data_list)
rsf_data_final$plot <- sub("^(Plot_[0-9]+\\.[0-9]+).*", "\\1", rsf_data_final$polygon)

# Check the new size
cat("Final dataset size:", nrow(rsf_data_final), "rows\n")
cat("Used vs Available:\n")
print(table(rsf_data_final$used))

# Check distribution per individual
rsf_data_final %>%
  group_by(individualID, used) %>%
  summarise(n = n()) %>%
  head(20)

# Add plot identifier
rsf_data_final$plot <- sub("^(Plot_[0-9]+\\.[0-9]+).*", "\\1", rsf_data_final$polygon)

###--------------------------------
###Combine Seasons
###--------------------------------

# Check the first few rows
head(rsf_data_final_march)

# Check the class of the columns
str(rsf_data_final_march)

# If the first row contains column names as character data, remove it:
rsf_data_final_march <- rsf_data_final_march[-1, ]

# If columns are characters instead of numeric, convert them:
rsf_data_final_march <- rsf_data_final_march %>%
  mutate(
    canopy_cover = as.numeric(canopy_cover),
    roughness = as.numeric(roughness),
    ENL0D = as.numeric(ENL0D),
    ENL1 = as.numeric(ENL1),
    ENL2D = as.numeric(ENL2D),
    X0.25m = as.numeric(X0.25m),
    X0.5m = as.numeric(X0.5m),
    X1m = as.numeric(X1m),
    X2m = as.numeric(X2m),
    X3m = as.numeric(X3m),
    X4m = as.numeric(X4m),
    X5m = as.numeric(X5m)
    # Add other numeric columns as needed
  )

# Check if it's fixed
head(rsf_data_final_march)
str(rsf_data_final_march)

# Reset row names
rownames(rsf_data_final_march) <- NULL

# Or completely regenerate it to avoid the issue

rsf_data_final_march$season <- "March"
rsf_data_final$season <- "July"

rsf_both_seasons <- rbind(rsf_data_final, rsf_data_final_march)

##Try a simple model, one variable at a time
##--------------------------------
####Just looking at roughness
##--------------------------------

rsf_roughness_only <- glmer(
  used ~ scale(roughness) +
    (1 | individualID) +
    (1 | plot),
  data = rsf_both_seasons,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)

summary(rsf_roughness_only)

library(ggeffects)

pred_rough <- ggpredict(rsf_roughness_only, terms = "roughness [all]")

plot(pred_rough) +
  labs(
    x = "Understory roughness (scaled)",
    y = "Predicted probability of use",
    title = "Selection for understory roughness"
  ) +
  theme_classic()

pred_rough <- ggpredict(
  rsf_roughness_only,
  terms = "roughness [all]",
  bias_correction = TRUE
)

plot(pred_rough) +
  labs(
    x = "Understory roughness (scaled)",
    y = "Predicted probability of use"
  ) +
  theme_classic()

### “Predicted probabilities from mixed-effects logistic regression models were bias-corrected to account for back-transformation from the logit scale.”
### scale(roughness) = +0.176
### p < 2e-16

library(ggplot2)

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
    x = "Understory roughness",
    y = "Relative probability of selection"
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    panel.border = element_blank()
  )


# Model 2: Combined model with season interactions
rsf_seasonal <- glmer(used ~ (scale(canopy_cover) + scale(roughness) + 
                                scale(ENL1) + scale(understory) + 
                                scale(midstory) + scale(overstory)) * season +
                        (1|individualID) + (1|plot),
                      data = rsf_both_seasons,
                      family = binomial,
                      control = glmerControl(optimizer = "bobyqa"))
summary(rsf_seasonal)

library(ggeffects)
library(ggplot2)
library(patchwork)

# List of variables you want to visualize
vars <- c("canopy_cover", "understory", "midstory", 
          "roughness", "overstory", "ENL1")

vars <- c("canopy_cover", "roughness", "ENL1",
          "understory", "midstory", "overstory")

plot_list <- lapply(vars, function(v) {
  ggpredict(rsf_seasonal, terms = c(v, "season")) |>
    plot() +
    labs(title = paste("Effect of", v, "by Season"),
         x = v, y = "Predicted Probability of Use") +
    theme_bw()
})

# Combine plots into a grid
wrap_plots(plotlist = plot_list, ncol = 2)`


library(ggplot2)
library(dplyr)

# Extract coefficients for plotting
coef_july <- c(
  Canopy = 0.407212,
  Roughness = 0.072027,
  ENL1 = -0.048203,
  Understory = 0.186435,
  Midstory = 0.083671,
  Overstory = 0.028284
)

coef_march <- c(
  Canopy = 0.407212 - 0.027327,
  Roughness = 0.072027 - 0.004647,
  ENL1 = -0.048203 - 0.001547,
  Understory = 0.186435 - 0.028933,
  Midstory = 0.083671 - 0.019827,
  Overstory = 0.028284 - 0.005386
)

# Create dataframe for plotting
coef_df <- data.frame(
  Variable = rep(names(coef_july), 2),
  Coefficient = c(coef_july, coef_march),
  Season = rep(c("July", "March"), each = 6)
)

###----------------------------------
# Publication-ready plot
###----------------------------------

# Ensure correct ordering of variables
coef_df$Variable <- factor(
  coef_df$Variable,
  levels = c("Roughness", "Canopy", "Understory",
             "Midstory", "Overstory", "ENL1"), 
  labels = c("Roughness", "Canopy Cover", "Understory",
             "Midstory", "Overstory", "ENL1"))

ggplot(coef_df, aes(x = Variable, y = Coefficient, fill = Season)) +
  geom_col(
    position = position_dodge(width = 0.7),
    width = 0.6,
    color = "black",
    linewidth = 0.2
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4) +
  scale_fill_manual(
    values = c(
      "March" = "#66C2A5",  # spring green
      "July"  = "#FC8D62"   # warm summer orange
    ),
    name = "Season"
  ) +
  labs(
    title = "Seasonal Habitat Selection",
    subtitle = "Comparison of standardized RSF coefficients",
    y = "Selection Coefficient (β)",
    x = "Habitat Variable"
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 10)
  )

###-------------------------------
####To make a table
###-------------------------------
library(sjPlot)

# Run separate seasonal models for cleaner comparison
rsf_july <- glmer(used ~ scale(canopy_cover) + scale(roughness) + 
                    scale(ENL1) + scale(understory) + 
                    scale(midstory) + scale(overstory) +
                    (1|individualID) + (1|plot),
                  data = rsf_both_seasons[rsf_both_seasons$season == "July", ],
                  family = binomial,
                  control = glmerControl(optimizer = "bobyqa"))

rsf_march <- glmer(used ~ scale(canopy_cover) + scale(roughness) + 
                     scale(ENL1) + scale(understory) + 
                     scale(midstory) + scale(overstory) +
                     (1|individualID) + (1|plot),
                   data = rsf_both_seasons[rsf_both_seasons$season == "March", ],
                   family = binomial,
                   control = glmerControl(optimizer = "bobyqa"))

# Create comparison table
tab_model(rsf_july, rsf_march, 
          dv.labels = c("July", "March"),
          show.ci = TRUE,
          title = "Habitat Selection by Season")
####--------------------------------------------
###Adding in an area column to the model
###--------------------------------------------
# Replace only the second underscore with a dot
rsf_both_seasons <- rsf_both_seasons %>%
  left_join(mcp_sf %>% select(id, area), 
            by = c("polygon" = "id"))

rsf_seasonal_area <- glmer(used ~ (scale(canopy_cover) + scale(roughness) + 
                                     scale(ENL1) + scale(understory) + 
                                     scale(midstory) + scale(overstory)) * season +
                             scale(area) +  # Main effect only
                             (1|individualID) + (1|plot),
                           data = rsf_both_seasons,
                           family = binomial,
                           control = glmerControl(optimizer = "bobyqa"))

# Check the convergence more carefully
relgrad <- with(rsf_seasonal_area@optinfo$derivs, solve(Hessian, gradient))
max(abs(relgrad))

# If this is < 0.001, your model is fine despite the warning

summary(rsf_seasonal_area)


# Is the model with area better than without?
anova(rsf_seasonal, rsf_seasonal_area)
AIC(rsf_seasonal, rsf_seasonal_area)

# Does area change how strongly animals select for canopy cover or roughness?
rsf_area_habitat_full <- glmer(used ~ (scale(canopy_cover) + scale(roughness) + 
                                         scale(ENL1) + scale(understory) + 
                                         scale(midstory) + scale(overstory)) * season +
                                 scale(area) * (scale(canopy_cover) + scale(roughness)) +
                                 (1|individualID) + (1|plot),
                               data = rsf_both_seasons,
                               family = binomial,
                               control = glmerControl(optimizer = "bobyqa"))

summary(rsf_area_habitat_full)

######Biological Interpretation:
#Results are consistent across both datasets - individuals with larger home ranges:
#Are more selective about canopy cover (stronger preference)
#Are less influenced by terrain roughness
#This suggests a scale-dependent habitat selection strategy:
#Small mammals with more space can afford to be pickier about canopy
#Roughness matters less when you have a larger area to work with

relgrad <- with(rsf_area_habitat_full@optinfo$derivs, solve(Hessian, gradient))
max(abs(relgrad))

library(ggplot2)
library(dplyr)

# Create binary categories (median split)
rsf_both_seasons <- rsf_both_seasons %>%
  mutate(area_category = ifelse(area > median(area, na.rm = TRUE), 
                                "Large HR", "Small HR"))

# Plot canopy cover selection by home range size
rsf_both_seasons %>%
  filter(used == 1) %>%
  ggplot(aes(x = canopy_cover, fill = area_category)) +
  geom_density(alpha = 0.6) +
  facet_wrap(~season) +
  labs(title = "Canopy Cover Selection by Home Range Size",
       x = "Canopy Cover (%)", 
       y = "Density",
       fill = "Home Range Size") +
  theme_minimal()

# Roughness by home range size
rsf_both_seasons %>%
  filter(used == 1) %>%
  ggplot(aes(x = roughness, fill = area_category)) +
  geom_density(alpha = 0.6) +
  facet_wrap(~season) +
  labs(title = "Roughness Selection by Home Range Size",
       x = "Roughness", 
       y = "Density",
       fill = "Home Range Size") +
  theme_minimal()

library(ggplot2)
library(dplyr)

# Custom theme for publication
theme_pub <- theme_classic() +
  theme(
    axis.line = element_line(color = "black", linewidth = 0.5),
    axis.text = element_text(size = 11, color = "black"),
    axis.title = element_text(size = 12, face = "bold"),
    strip.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
    strip.text = element_text(size = 11, face = "bold"),
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    legend.position = "top",
    panel.spacing = unit(1, "lines")
  )

# Canopy cover plot with more transparency
p1 <- rsf_both_seasons %>%
  filter(used == 1) %>%
  ggplot(aes(x = canopy_cover, fill = area_category)) +
  geom_density(alpha = 0.4, linewidth = 0.7) +  # Increased transparency
  facet_wrap(~season) +
  scale_fill_manual(values = c("Small HR" = "#2166ac", "Large HR" = "#b2182b")) +
  labs(x = "Canopy Cover (%)", 
       y = "Density",
       fill = "Home Range Size") +
  theme_pub

# Roughness plot with more transparency
p2 <- rsf_both_seasons %>%
  filter(used == 1) %>%
  ggplot(aes(x = roughness, fill = area_category)) +
  geom_density(alpha = 0.4, linewidth = 0.7) +  # Increased transparency
  facet_wrap(~season) +
  scale_fill_manual(values = c("Small HR" = "#2166ac", "Large HR" = "#b2182b")) +
  labs(x = "Terrain Roughness", 
       y = "Density",
       fill = "Home Range Size") +
  theme_pub

# Display
print(p1)
print(p2)

# Save
ggsave("canopy_cover_by_homerange.png", p1, width = 8, height = 4, dpi = 300)
ggsave("roughness_by_homerange.png", p2, width = 8, height = 4, dpi = 300)

# Option 1: Use just outlines (no fill)
p1_lines <- rsf_both_seasons %>%
  filter(used == 1) %>%
  ggplot(aes(x = canopy_cover, color = area_category)) +
  geom_density(linewidth = 1) +
  facet_wrap(~season) +
  scale_color_manual(values = c("Small HR" = "#2166ac", "Large HR" = "#b2182b")) +
  labs(x = "Canopy Cover (%)", 
       y = "Density",
       color = "Home Range Size") +
  theme_pub

# Option 2: More transparency (less overlap color)
p1_transparent <- rsf_both_seasons %>%
  filter(used == 1) %>%
  ggplot(aes(x = canopy_cover, fill = area_category)) +
  geom_density(alpha = 0.2, linewidth = 0.7) +  # Changed from 0.4 to 0.2
  facet_wrap(~season) +
  scale_fill_manual(values = c("Small HR" = "#2166ac", "Large HR" = "#b2182b")) +
  labs(x = "Canopy Cover (%)", 
       y = "Density",
       fill = "Home Range Size") +
  theme_pub

# Option 3: Filled with colored outlines (clearest)
p1_both <- rsf_both_seasons %>%
  filter(used == 1) %>%
  ggplot(aes(x = canopy_cover, fill = area_category, color = area_category)) +
  geom_density(alpha = 0.3, linewidth = 1) +
  facet_wrap(~season) +
  scale_fill_manual(values = c("Small HR" = "#2166ac", "Large HR" = "#b2182b")) +
  scale_color_manual(values = c("Small HR" = "#2166ac", "Large HR" = "#b2182b")) +
  labs(x = "Canopy Cover (%)", 
       y = "Density",
       fill = "Home Range Size",
       color = "Home Range Size") +
  theme_pub

p2_both <- rsf_both_seasons %>%
  filter(used == 1) %>%
  ggplot(aes(x = roughness, fill = area_category, color = area_category)) +
  geom_density(alpha = 0.3, linewidth = 1) +
  facet_wrap(~season) +
  scale_fill_manual(values = c("Small HR" = "#2166ac", "Large HR" = "#b2182b")) +
  scale_color_manual(values = c("Small HR" = "#2166ac", "Large HR" = "#b2182b")) +
  labs(x = "Roughness", 
       y = "Density",
       fill = "Home Range Size",
       color = "Home Range Size") +
  theme_pub

# View them
p1_lines
p1_transparent
p1_both
p2_both
