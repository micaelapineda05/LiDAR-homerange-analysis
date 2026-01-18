library(sf)
library(dplyr)
library(ggplot2)
library(terra)
#####THIS IS THE CORRECT ONE FOR CREATING THE MCP LIST WITH THE COORDS REFERNCEING THE RASTERS!!!!!!!!
mcp_polygons <- list()

groups <- split(joined_df, list(joined_df$plot_id, joined_df$PITnum), drop = TRUE)

for (group_name in names(groups)) {
  indiv_df <- groups[[group_name]]
  
  if (nrow(indiv_df) < 3) next
  
  coords <- indiv_df[, c("laser_stake_x", "laser_stake_y")]
  coords <- coords[complete.cases(coords), ]
  if (nrow(coords) < 3) next
  
  hull_idx <- chull(coords)
  hull_coords <- coords[c(hull_idx, hull_idx[1]), ]
  
  poly <- st_polygon(list(as.matrix(hull_coords)))
  
  # name will automatically be "plotID.PITtag"
  mcp_polygons[[group_name]] <- st_sfc(poly)
}


# Plot by index (e.g., the first one)
###Did not create polygons with reoccurring trapping ponits, so add in buffer

plot(mcp_polygons[[2]], col = 'lightblue', border = 'darkblue', main = names(mcp_polygons)[2])

###
#####Start here for creating buffer!!!
###

# Start with your MCP list
mcp_geoms <- list()

groups <- split(joined_df, list(joined_df$plot_id, joined_df$PITnum), drop = TRUE)

for (group_name in names(groups)) {
  indiv_df <- groups[[group_name]]
  
  coords <- indiv_df[, c("laser_stake_x", "laser_stake_y")]
  coords <- coords[complete.cases(coords), ]
  coords <- unique(coords)
  
  # no coordinates → skip
  if (nrow(coords) == 0) next
  
  # --- CASE 1: Only 1 unique point → POINT ---
  if (nrow(coords) == 1) {
    geom <- st_point(as.numeric(coords[1, ]))
    sfc <- st_sfc(geom, crs = 3857)
    buffered <- st_buffer(sfc, dist = 5)
    mcp_geoms[[group_name]] <- buffered
    next
  }
  
  # --- CASE 2: Exactly 2 unique points → LINESTRING ---
  if (nrow(coords) == 2) {
    geom <- st_linestring(as.matrix(coords))
    sfc <- st_sfc(geom, crs = 3857)
    buffered <- st_buffer(sfc, dist = 5)
    mcp_geoms[[group_name]] <- buffered
    next
  }
  
  # --- CASE 3: 3 or more points → POLYGON (convex hull) ---
  hull_idx <- chull(coords)
  hull_coords <- coords[c(hull_idx, hull_idx[1]), ]
  
  poly <- st_polygon(list(as.matrix(hull_coords)))
  sfc <- st_sfc(poly, crs = 3857)
  buffered <- st_buffer(sfc, dist = 5)
  
  mcp_geoms[[group_name]] <- buffered
}

# Combine into sf
mcp_sf <- do.call(rbind, lapply(names(mcp_geoms), function(nm) {
  st_sf(
    id = nm,
    geometry = mcp_geoms[[nm]]
  )
}))

# Compute area in grid units² (e.g., m² if your x/y are meters)
mcp_sf$area <- st_area(mcp_sf)


####Looking at them individually
unique(mcp_sf$id)[5]

test_id <- unique(mcp_sf$id)[5]

plot(mcp_sf[mcp_sf$id == test_id, "geometry"],
     col = "lightblue", border = "black", main = test_id)

###Here is how I plotted them with the MCPs on top:
##read in csv 
cover <- read.csv("C:/Users/PinedaMicaelaTonatsi/Documents/LiDAR Collab/New Data Winter & Summer/July/July canopy cover csv/1.2.txt_xyCanopycover.csv", sep= ";",
                  dec = ",",
                  header = TRUE,
                  col.names = c("x", "y", "canopy_cover"),
                  fill = TRUE)  
###--------------------------------------------------------
###This is the code for plotting the mcps on the raster!
###--------------------------------------------------------
###Turning the csvs into rasters
head(cover)

cover$x <- as.numeric(gsub("[^0-9.-]", "", cover$x))
cover$y <- as.numeric(gsub("[^0-9.-]", "", cover$y))
r <- rast(cover, type="xyz")

terra::plot(r)

####ploting the mcps on the rasters
plot(st_geometry(mcp_sf[6, ]), 
     col = "lightblue", 
     border = "darkblue", 
     add = TRUE)

#####--------------------------------------------
###
#####--------------------------------------------
# Create the grouping variable from names
plot_base <- sub("^(Plot_[0-9]+\\.[0-9]+).*", "\\1", names(mcp_geoms))

# Split the list
plot_lists <- split(mcp_geoms, plot_base)

# View the structure
str(plot_lists, max.level = 1)
