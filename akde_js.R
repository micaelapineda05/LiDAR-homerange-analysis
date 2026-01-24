library(ctmm)
library(sf)
library(tidyverse)

# Load data
load("creating_mcps.RData")

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


