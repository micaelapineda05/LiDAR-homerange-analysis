library(ctmm)
library(sf)
library(tidyverse)

# Load data
load("creating_mcps.RData")
##Or use step_1.RData, you just need joined_df

xy <- joined_df$geometry |> st_transform(4326) |> st_coordinates()
names(joined_df)
joined_df$Check

ctmm_dat <- data.frame(
  individual.local.identifier = joined_df$PITnum, 
  timestamp = ymd_hm(paste0(as.character(joined_df$Date), " ", 
                           ifelse(joined_df$Check == "AM", "6:00", "18:00"))), 
  lon = xy[, 1], 
  lat = xy[, 2] 
)

dat <- split(ctmm_dat, ctmm_dat$individual.local.identifier)
sapply(dat, nrow)


res <- lapply(dat, function(x) 
  try({
    x <- as.telemetry(x)
    GUESS <- ctmm.guess(x, interactive = FALSE)
    FIT   <- ctmm.fit(x, GUESS)
    akde(x, FIT)
  })
)
    
# how did we do?

length(res) # 388 attemps

# how many failed?
sum(sapply(res, is, "try-error")) # 40 failed

# how many areas did we get?
areas <- lapply(res, function(x) try(summary(x)$CI))
sum(!sapply(areas, is, "try-error")) # 201

# Where did it work?
dat.ok <- dat[which(!sapply(areas, is, "try-error"))]

sapply(dat.ok, nrow) |> table() # they all had at least 3 data points

sapply(dat.ok, function(x) length(unique(x$lon))) |> table() # with at least 3 distinct data points
####----------------------------------
####Testing to plot just one
####---------------------------------------
plot(res[[2]])

ud <- res[["900200000718829"]]

plot(ud, level = c(0.5, 0.95))


library(sf)

ud50 <- as.sf(ud, level = 0.50)
ud95 <- as.sf(ud, level = 0.95)

plot(ud95["level"])
plot(ud50["level"], add = TRUE)

plot(ud, level = 0.95)

plot(ud, level = c(0.5, 0.95))

####--------------------------------------
#### Removing individuals in multiple plots
####--------------------------------------
library(dplyr)

# Create lookup table: one plot per PIT tag (most points wins)
pit_plot_lookup_clean <- joined_df %>%
  st_drop_geometry() %>%
  count(PITnum, plot_id) %>%
  group_by(PITnum) %>%
  slice_max(n, with_ties = FALSE) %>%
  ungroup() %>%
  select(PITnum, plot_id)

# Verify each PIT tag appears only once
stopifnot(all(table(pit_plot_lookup_clean$PITnum) == 1))

# Add plot_id to each UD object's info slot
for (id in names(res)) {
  if (!inherits(res[[id]], "UD")) next
  
  plot_id <- pit_plot_lookup_clean$plot_id[pit_plot_lookup_clean$PITnum == id]
  res[[id]]@info$plot_id <- ifelse(length(plot_id) == 0, NA, plot_id[1])
}

# Quick check - view all plot_ids
for (id in names(res)) {
  if (inherits(res[[id]], "UD")) {
    cat(id, "->", res[[id]]@info$plot_id, "\n")
  }
}

# Build file lookup table (remove "Plot_" prefix for CSV filenames)
ud_file_lookup <- data.frame(
  ud_name = names(res)[sapply(res, inherits, "UD")],
  stringsAsFactors = FALSE
) %>%
  mutate(
    plot_id = sapply(ud_name, function(id) res[[id]]@info$plot_id),
    file_id = gsub("^Plot_", "", plot_id)
  ) %>%
  filter(!is.na(file_id)) %>%
  select(ud_name, file_id)

# View the lookup
print(ud_file_lookup)

# Now use this in the script extract_lidar_ctmm_UDs with use_lookup = TRUE
