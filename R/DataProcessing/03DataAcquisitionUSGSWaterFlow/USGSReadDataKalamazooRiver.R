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
# From Data Folder, Kalamazoo River.csv
kar <- read.csv("Data/KalamazooRiver/KalamazooRiverMeteoWaterTemp.csv")

# Convert from "m/d/YYYY" to Date
kar$SampleDate <- as.Date(kar$SampleDate)

# Seek stations
sites <- read_waterdata_monitoring_location(
  bbox = c(-85.79, 42.09, -85.39, 42.49))

# replace with your target point (lon, lat)
target_lon <- -85.5872
target_lat <- 42.2917

# extract coords from sf geometry
coords <- st_coordinates(sites)
sites_near_10km <- sites %>%
  st_drop_geometry() %>%
  mutate(
    lon = coords[,1],
    lat = coords[,2],
    dist_km = distHaversine(
      cbind(lon, lat),
      cbind(target_lon, target_lat)
    ) / 1000
  ) %>%
  filter(dist_km <= 10) %>%
  arrange(dist_km) %>%
  select(monitoring_location_id, monitoring_location_name, lat, lon, dist_km)

# Define USGS site and parameter ------------------------------------------
site <- gsub("USGS-", "", sites_near_10km$monitoring_location_id[81])
paramflow <- "00060"     # water flow, cfs
# Kalamazoo River at Comstock, MI - USGS-04106000
# Fetch daily water temperature -------------------------------------------
flow <- read_waterdata_daily(
  monitoring_location_id = paste("USGS", site, sep = "-"),
  parameter_code = paramflow,
  time = c(as.character(min(kar$SampleDate)), as.character(max(kar$SampleDate)))
)

# Clean USGS data
flow_values <- flow %>%
  st_drop_geometry() %>%
  mutate(
    time = as.Date(time),
    Q_cm3s = value * 28316.8 * 0.05,   # cfs → cm³/s, 0.05 is to get an estimate for Portage Creek at Kalamazoo, MI - USGS-04106500
    width_cm = 1200,           # 12 m
    depth_cm = 120,            # 1.2 m
    area_cm2 = width_cm * depth_cm,
    velocity_cms = Q_cm3s / area_cm2) %>%
  select(time, Q_cm3s, velocity_cms)

# Merge the water velocity
kar <- kar %>%
  left_join(flow_values %>% select(time, velocity_cms),
            by = c("SampleDate" = "time"))

# save
write.csv(kar, "Data/KalamazooRiver/KalamazooRiverMeteoWaterTempFlow.csv", row.names = FALSE)

