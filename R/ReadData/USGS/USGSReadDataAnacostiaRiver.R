
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
# From Data Folder, AnacostiaRiver.csv
anr <- read.csv("Data/AnacostiaRiver/AnacostiaRiver.csv")

# Convert from "m/d/YYYY" to Date
anr$SampleDate <- as.Date(anr$SampleDate)

bbox <- c(-77.15, 38.75, -76.85, 39.05)

## 2) get monitoring locations
sites <- read_waterdata_monitoring_location(bbox = bbox)

## quick checks
nrow(sites)
head(sites)

site_ids <- sites$monitoring_location_number
# sometimes the API expects "USGS-" prefix; check a sample:
head(site_ids)

# summary of characteristics (doesn't download full result rows)
summary_resp <- readWQPdata(
  siteNumbers = site_ids[1],
  querySummary = TRUE
)
summary_resp

res_qw <- readWQPqw(
  siteNumbers = site_ids[1],
  parameterCd  = "00010",
  startDate    = "2000-01-01",
  endDate      = as.character(Sys.Date())
)
nrow(res_qw); head(res_qw)


# Seek stations
sites <- read_waterdata_monitoring_location(
  bbox = c(-77.12, 38.80, -76.88, 39.02)) # need to be changed per locations

# replace with your target point (lon, lat)
target_lon <- -76.98
target_lat <- 38.90

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
site1 <- "01651827" # Anacostia River nr Buzzard Point at Washington, DC
site2 <- "01651760"
site3 <- "01647600"
paramtemp <- "00010"     # water temperature, °C

# Fetch daily water temperature -------------------------------------------
temp <- read_waterdata_daily(
  monitoring_location_id = paste("USGS", site2, sep = "-"),
  parameter_code = paramtemp,
  statistic_id = "00003",  # mean daily values
  time = c(as.character(min(anr$SampleDate)), as.character(max(anr$SampleDate)))
)

# Clean USGS data
temp_values <- temp %>%
  st_drop_geometry() %>%
  mutate(time = as.Date(time)) %>%   # <-- THIS is likely missing
  select(time, value)

# Merge USGS temperature into fx
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

