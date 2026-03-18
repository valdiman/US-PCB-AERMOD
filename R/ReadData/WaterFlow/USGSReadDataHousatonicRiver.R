# -------------------------------------------------------------------
# Code use to retrieve water flow (Q) from a USGS station

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
# From Data Folder, Housatonic River.csv
hor <- read.csv("Data/HousatonicRiver/HousatonicRiverMeteoWaterTemp.csv")

# Convert from "m/d/YYYY" to Date
hor$SampleDate <- as.Date(hor$SampleDate)

# Seek stations
sites <- read_waterdata_monitoring_location(
  bbox = c(-73.45, 42.25, -73.05, 42.65))

# replace with your target point (lon, lat)
target_lon <- -73.2454
target_lat <- 42.4501

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
site <- gsub("USGS-", "", sites_near_5km$monitoring_location_id[58])
paramflow <- "00060"     # water flow, cfs
# East Branch Housatonic River at Coltsville, MA - USGS-01197000
# Fetch daily water temperature -------------------------------------------
flow <- read_waterdata_daily(
  monitoring_location_id = paste("USGS", site, sep = "-"),
  parameter_code = paramflow,
  time = c(as.character(min(hor$SampleDate)), as.character(max(hor$SampleDate)))
)

# Clean USGS data
flow_values <- flow %>%
  st_drop_geometry() %>%
  mutate(
    time = as.Date(time),
    Q_cm3s = value * 28316.8,   # cfs → cm³/s
    width_cm = 1000,           # 10 m
    depth_cm = 120,            # 1.2 m
    area_cm2 = width_cm * depth_cm,
    velocity_cms = Q_cm3s / area_cm2) %>%
  select(time, Q_cm3s, velocity_cms)

# Merge the water velocity
hor <- hor %>%
  left_join(flow_values %>% select(time, velocity_cms),
            by = c("SampleDate" = "time"))

# save
write.csv(hor, "Data/HousatonicRiver/HousatonicRiverMeteoWaterTempFlow.csv", row.names = FALSE)

