### Small Mammal Data Import / Prep / Output Captures per 100 TN ###

library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(tibble)
library(readxl)
library(vegan)
library(lubridate)
library(sf)
##### Import / Combine Data #####
### Load Smam Capture Data from 2020
SMAMDATA <- read_excel("C:/Users/PinedaMicaelaTonatsi/Documents/Small_mammal_capture data_Scott/RTG 2300 SP7 - Capture Data - 2020 July-September.xlsx") ## Load July-August-September 2020 Capture Data

------------------------------------------------------------
  ###Add xy columns to capture data for coordinate locations
------------------------------------------------------------
  # Read in the trap coordinates
trap_coords <- read.table("C:/Users/PinedaMicaelaTonatsi/Documents/Small_mammal_capture data_Scott/RTG 2300 SP7 - Basic Trap Layout 64 points.txt", 
                            header = TRUE, 
                            sep = "\t")
  
# Join the coordinates to your capture data
SMAMDATA_2 <- SMAMDATA %>%
    left_join(trap_coords, by = c("TrapNum" = "trapID")) 
 
##write.csv(SMAMATA_3, file = "./LiDAR Collab/smma_loc.csv")
  
table(SMAMDATA$PITnum)
  
filtered_data <- SMAMDATA_2 %>%
    group_by(PITnum) %>%
    filter(n() > 1) %>%
    ungroup()

cleaned_data <- filtered_data %>%
    drop_na(PITnum)

unique(cleaned_data$PITnum)
  
-------------------------------------------
    ##Separate plots
-------------------------------------------
  
  # Create all site objects
for(i in 1:8) {
    assign(paste0("site_", i), 
           cleaned_data %>% filter(Site == as.character(i)))
  }

# Create all plot objects
for(site_num in 1:8) {
  for(plot_num in 1:5) {
    assign(paste0("Plot_", site_num, ".", plot_num),
           cleaned_data %>% filter(Site == as.character(site_num), 
                                   Plot == as.character(plot_num)))
  }
}

# Print summary statistics for all sites
for(i in 1:8) {
  cat("\n=== Site", i, "===\n")
  site_data <- get(paste0("site_", i))
  print(table(site_data$PITnum))
  print(table(site_data$SpeciesID))
  print(unique(site_data$Plot))
}  

# Step 1: Get all plot data frame names
df_names <- ls(pattern = "^Plot_\\d+\\.\\d+$")

# Step 2: Create a list to store results
valid_pits_list <- list()

# Step 3: Loop through each data frame
for (df_name in df_names) {
  df <- get(df_name)  # fetch the data frame by name
  
  # Make sure "pit" column exists
  if ("PITnum" %in% names(df)) {
    # Count PIT occurrences
    pit_counts <- table(df$PITnum)
    
    # Filter PITs with at least 3 observations
    valid_pits <- names(pit_counts[pit_counts >= 3])
    
    # Store in list
    valid_pits_list[[df_name]] <- valid_pits
  } else {
    warning(paste("No 'pit' column in", df_name))
  }
}

# Now valid_pits_list is a named list with data frame names as keys
# and vectors of valid PITs as values
------------------------------
###Inspecting the pit_list
------------------------------
  # View all PITs for Plot_1.1
valid_pits_list["Plot_1.5"]

# Check all plots that had at least one qualifying PIT
names(valid_pits_list)[sapply(valid_pits_list, length) > 0]

##---------------------------
##Creating a joined list with only pit tags 3 or more observations
##---------------------------
# Create a new list to store filtered dataframes
filtered_plots_list <- list()

# Loop over each data frame name
for (df_name in df_names) {
  # Get the full data frame
  df <- get(df_name)
  
  # Get the valid PITs for that data frame
  valid_pits <- valid_pits_list[[df_name]]
  
  # Subset the data frame for valid PITs only
  filtered_df <- df[df$PITnum %in% valid_pits, ]
  
  # Store it in the list
  filtered_plots_list[[df_name]] <- filtered_df
  
  # Optionally: assign it back to the global environment with _filtered suffix
  assign(paste0(df_name, "_filtered"), filtered_df)
}

###Before creating MCPs add in the xy coords from the matching values for the raters!
# Create a data frame from the filtered plots list
nw_df <- do.call(rbind, lapply(names(filtered_plots_list), function(name) {
  df <- filtered_plots_list[[name]]
  df$plot_id <- name   # add the plot name as an ID
  df
}))

# Check the result
table(nw_df$plot_id)  # Should show all plots, not just Plot_8.5

##write.csv(nw_df, "All_filtered_pits.csv", row.names = FALSE)

####Join with the coords from the grid with matching xy coords from the raster data
# Load an RData file
load("C:/Users/PinedaMicaelaTonatsi/Documents/LiDAR Collab/laser_grid_stakes.rda")

laser_grid_coords <- laser_grid_stakes %>%
  select(-geometry)

nw_df <- nw_df %>%
  mutate(plot_id_clean = gsub("Plot_", "", plot_id))

laser_grid_stakes <- laser_grid_stakes %>%
  rename(plot_id_clean = id_plot)

joined_df <- nw_df %>%
  left_join(
    laser_grid_stakes,
    by = c("plot_id_clean", "TrapNum" = "stake")
  )