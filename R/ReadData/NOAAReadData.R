# Code to read NOAA meteorological data
# to be used for flux calculations
# https://docs.ropensci.org/rnoaa/articles/rnoaa.html
# Documentation:
# https://www.ncei.noaa.gov/data/global-hourly/doc/isd-format-document.pdf

# Packages and libraries needed --------------------------------------------
# Install packages
install.packages("rnoaa") # For future use, noaaweather package will need to be installed it.
install.packages("leaflet")
install.packages("dplyr")
install.packages("stringr")
install.packages("geosphere")

# Libraries
{
  library(rnoaa)
  library(geosphere)
  library(dplyr)
  library(lubridate)
  library(zoo)
}

# The rnoaa package will soon be retired and archived because the underlying
# APIs have changed dramatically. The package currently works but does not pull
# the most recent data in all cases. A noaaWeather package is planned as a
# replacement but the functions will not be interchangeable.

# Fox River coordinates
site_lat <- 44.52861
site_lon <- -88.01

# Load NOAA ISD station list
stations <- isd_stations()

# Find nearby stations (within 50 km)
stations_nearby <- stations %>%
  mutate(distance_km = distHaversine(cbind(site_lon, site_lat), cbind(lon, lat)) / 1000) %>%
  filter(distance_km <= 50)

# Select the nearest major station (Austin Straubel Int. Airport)
fx.station <- stations[stations$usaf == "726450", ]

# Read For River Data
fx <- read.csv("Data/FoxRiver/FoxRiver.csv")
fx$SampleDate <- as.Date(fx$SampleDate, format = "%m/%d/%Y")

# Determine years to download
years_to_download <- unique(year(fx$SampleDate))

# NOAA station IDs
usaf <- as.character(fx.station$usaf[1])
wban <- as.character(fx.station$wban[1])

# Download yearly ISD data
weather_list <- list()

for (y in years_to_download) {
  cat("Downloading year:", y, "\n")
  dat <- isd(usaf = usaf, wban = wban, year = y)
  weather_list[[as.character(y)]] <- dat
}

weather_all <- bind_rows(weather_list)

weather_daily <- weather_all %>%
  mutate(
    date = as.Date(date, format = "%Y%m%d"),
    
    air_temp = as.numeric(temperature),
    wind_speed = as.numeric(wind_speed),
    air_pressure = as.numeric(air_pressure),
    
    # # Replace impossible codes with NA
    air_temp = ifelse(air_temp %in% c(9999, 99999), NA, air_temp / 10),       # °C
    wind_speed = ifelse(wind_speed %in% c(9999, 99999), NA, wind_speed / 10), # m/s
    air_pressure = ifelse(air_pressure %in% c(99999, 999999), NA, air_pressure / 10) # hPa (tenths)
  ) %>%
  filter(!is.na(date)) %>%
  group_by(date) %>%
  summarise(
    air_temp = mean(air_temp, na.rm = TRUE),
    wind_speed = mean(wind_speed, na.rm = TRUE),
    air_pressure = mean(air_pressure, na.rm = TRUE)
  )

fx <- fx %>%
  left_join(weather_daily, by = c("SampleDate" = "date"))

# Save
write.csv(fx, "Data/FoxRiver/FoxRiver_env.csv", row.names = FALSE)

