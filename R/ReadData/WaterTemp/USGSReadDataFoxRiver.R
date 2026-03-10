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
# From Data Folder, Fox River.csv
fxr <- read.csv("Data/FoxRiver/FoxRiver_env.csv")

# Convert from "m/d/YYYY" to Date
fxr$SampleDate <- as.Date(fxr$SampleDate)

# Seek stations
sites <- read_waterdata_monitoring_location(
  bbox = c(-89, 43.5, -87, 45.5)) # need to be changed per locations

# replace with your target point (lon, lat)
target_lon <- -88.01
target_lat <- 44.52861

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
# FOX RIVER AT OIL TANK DEPOT AT GREEN BAY, WI
site <- gsub("USGS-", "", sites_near_5km$monitoring_location_id[1])
paramtemp <- "00010"     # water temperature, °C

# Fetch daily water temperature -------------------------------------------
temp <- read_waterdata_daily(
  monitoring_location_id = paste("USGS", site, sep = "-"),
  parameter_code = paramtemp,
  statistic_id = "00003",  # mean daily values
  time = c(as.character(min(fxr$SampleDate)), as.character(max(fxr$SampleDate)))
)

# Clean USGS data
temp_values <- temp %>%
  st_drop_geometry() %>%
  mutate(time = as.Date(time)) %>%
  select(time, value)

# Merge USGS temperature into fxr
fxr <- fxr %>%
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
fxr <- fxr %>%
  arrange(SampleDate) %>%
  mutate(doy = yday(SampleDate)) %>%
  mutate(
    temp_pred_seasonal = fill_seasonal_wrap(temp, doy, window = 7),
    temp_pred_seasonal = zoo::na.locf(temp_pred_seasonal, na.rm = FALSE),
    temp_pred_seasonal = zoo::na.locf(temp_pred_seasonal, fromLast = TRUE)
  ) %>%
  select(-doy)

# Create final temperature column
fxr <- fxr %>%
  mutate(temp_final = coalesce(temp, temp_pred_seasonal)) %>%
  select(-temp, -temp_pred_seasonal)

# Modify incorrectly predicted values
fxr$temp_final[c(21,22)] <- 20

# save
write.csv(fxr, "Data/FoxRiver/FoxRiver_temp.csv", row.names = FALSE)

