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
# From Data Folder, Spokane River.csv
spr <- read.csv("Data/Spokane/SpokaneRiver_env.csv")

# Convert from "m/d/YYYY" to Date
spr$SampleDate <- as.Date(spr$SampleDate)

# Seek stations
sites <- read_waterdata_monitoring_location(
  bbox = c(-117.63, 47.46, -117.23, 47.86))

# replace with your target point (lon, lat)
target_lon <- -117.4260
target_lat <- 47.6588

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
site <- gsub("USGS-", "", sites_near_5km$monitoring_location_id[31])
paramflow <- "00060"     # water flow, cfs
# Spokane River at Spokane, WA - USGS-12422500 
# Fetch daily water flow -------------------------------------------
flow <- read_waterdata_daily(
  monitoring_location_id = paste("USGS", site, sep = "-"),
  parameter_code = paramflow,
  time = c(as.character(min(spr$SampleDate)), as.character(max(spr$SampleDate)))
)

# Clean USGS data
flow_values <- flow %>%
  st_drop_geometry() %>%
  mutate(
    time = as.Date(time),
    Q_cm3s = value * 28316.8,   # cfs → cm³/s
    width_cm = 6000,           # 60 m
    depth_cm = 200,            # 2 m
    area_cm2 = width_cm * depth_cm,
    velocity_cms = Q_cm3s / area_cm2) %>%
  select(time, Q_cm3s, velocity_cms)

# Merge the water velocity
spr <- spr %>%
  left_join(flow_values %>% select(time, velocity_cms),
            by = c("SampleDate" = "time"))

# save
write.csv(spr, "Data/Spokane/SpokaneRiver_env.csv", row.names = FALSE)

