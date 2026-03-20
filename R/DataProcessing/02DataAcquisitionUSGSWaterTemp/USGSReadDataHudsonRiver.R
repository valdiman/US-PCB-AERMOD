# -------------------------------------------------------------------
# Code use to retrieve eater temperature from a USGS station

# Install packages
{
  install.packages("dataRetrieval")
  install.packages("dplyr")
  install.packages('sf')
  install.packages("zoo")
  install.packages("lubridate")
}

# Load libraries
{
  library(dataRetrieval) # read data from USGS
  library(dplyr)         # data manipulation
  library(sf)            # for handling sf objects (used by read_waterdata_daily)
  library(zoo)           # for interpolation
  library(lubridate)
  library(geosphere)
}

# Read data ---------------------------------------------------------------
# From Data Folder, Hudson River.csv
hur <- read.csv("Data/HudsonRiverAlbany/HudsonRiver.csv")

# Convert from "m/d/YYYY" to Date
hur$SampleDate <- as.Date(hur$SampleDate)

# Seek stations
sites <- read_waterdata_monitoring_location(
  bbox = c(-74.3, 42.4, -73.5, 42.9))

# replace with your target point (lon, lat)
target_lon <- -73.7562
target_lat <- 42.6526

# extract coords from sf geometry
coords <- st_coordinates(sites)
sites_near_5km <- sites %>%
  st_drop_geometry() %>%
  mutate(
    lon = coords[,1],
    lat = coords[,2],
    dist_km = distHaversine(
      cbind(lon, lat),
      cbind(target_lon, target_lat)
    ) / 1000
  ) %>%
  filter(dist_km <= 5) %>%
  arrange(dist_km) %>%
  select(monitoring_location_id, monitoring_location_name, lat, lon, dist_km)

# Define USGS site and parameter ------------------------------------------
site <- gsub("USGS-", "", sites_near_5km$monitoring_location_id[6]) # 6th station has data
paramtemp <- "00010"     # water temperature, °C

# Fetch daily water temperature -------------------------------------------
temp <- read_waterdata_daily(
  monitoring_location_id = paste("USGS", site, sep = "-"),
  parameter_code = paramtemp,
  statistic_id = "00003",  # mean daily values
  time = c(as.character(min(hur$SampleDate)), as.character(max(hur$SampleDate)))
)

# Clean USGS data
temp_values <- temp %>%
  st_drop_geometry() %>%
  mutate(time = as.Date(time)) %>%
  select(time, value)

# Merge USGS temperature into fx
hur <- hur %>%
  left_join(temp_values, by = c("SampleDate" = "time")) %>%
  mutate(temp = value) %>%
  select(-value)

# Predict missing values --------------------------------------------------
# Define seasonal smoothing function with year-end wrapping
fill_seasonal_wrap <- function(temp_vec, doy_vec, window = 7) {
  temp_smoothed <- temp_vec
  n_days <- 366  # max possible day-of-year including leap year
  
  for (i in seq_along(temp_vec)) {
    # Compute wrapped distance for neighbors
    distances <- abs(doy_vec - doy_vec[i])
    distances <- pmin(distances, n_days - distances)  # wrap-around
    neighbors <- which(distances <= window & !is.na(temp_vec))
    
    if (length(neighbors) > 0) {
      temp_smoothed[i] <- mean(temp_vec[neighbors], na.rm = TRUE)
    }
  }
  return(temp_smoothed)
}

# Apply seasonal smoothing with wrap and fill edge NAs
hur <- hur %>%
  arrange(SampleDate) %>%
  mutate(doy = yday(SampleDate)) %>%
  mutate(
    temp_pred_seasonal = fill_seasonal_wrap(temp, doy, window = 7),
    temp_pred_seasonal = zoo::na.locf(temp_pred_seasonal, na.rm = FALSE),
    temp_pred_seasonal = zoo::na.locf(temp_pred_seasonal, fromLast = TRUE)
  ) %>%
  select(-doy)

# Create final temperature column
hur <- hur %>%
  mutate(temp_final = coalesce(temp, temp_pred_seasonal)) %>%
  select(-temp, -temp_pred_seasonal)

# save
write.csv(hur, "Data/HudsonRiverAlbany/AlbanyRiver_temp.csv", row.names = FALSE)

