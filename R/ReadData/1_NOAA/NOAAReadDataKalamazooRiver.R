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

# Kalamazoo River coordinates (Kalamazoo, MI)
site_lat <- 42.2917
site_lon <- -85.5872

# Load NOAA ISD station list
stations <- isd_stations()

# Find nearby stations (within 50 km)
stations_nearby <- stations %>%
  mutate(distance_km = distHaversine(cbind(site_lon, site_lat), cbind(lon, lat)) / 1000) %>%
  filter(distance_km <= 50)

# Select the two nearest major stations (KLMAZO/BTL CREEK INTL ARPT & W K KELLOGG AIRPORT)
primary_station <- stations %>% filter(usaf == "726357")
backup_station  <- stations %>% filter(usaf == "725396")

# Read Kalamazoo River Data
kar <- read.csv("Data/Kalamazoo/KalamazooRiver.csv")
kar$SampleDate <- as.Date(kar$SampleDate)

# years needed from your river data
years_needed <- sort(unique(as.integer(format(kar$SampleDate, "%Y"))))

# station start years
primary_start_year <- as.integer(substr(as.character(primary_station$begin[1]), 1, 4))
backup_start_year  <- as.integer(substr(as.character(backup_station$begin[1]), 1, 4))

# only request years the station actually exists for
years_primary <- years_needed[years_needed >= primary_start_year]
years_backup  <- years_needed[years_needed >= backup_start_year]

get_daily_weather <- function(usaf, wban, years_vec) {
  weather_list <- list()
  
  for (y in years_vec) {
    cat("Downloading", usaf, "year:", y, "\n")
    dat <- tryCatch(
      isd(usaf = usaf, wban = wban, year = y),
      error = function(e) NULL
    )
    if (!is.null(dat)) weather_list[[as.character(y)]] <- dat
  }
  
  bind_rows(weather_list) %>%
    mutate(
      date = as.Date(date, format = "%Y%m%d"),
      air_temp = as.numeric(temperature),
      wind_speed = as.numeric(wind_speed),
      air_pressure = as.numeric(air_pressure),
      air_temp = ifelse(air_temp %in% c(9999, 99999), NA, air_temp / 10),
      wind_speed = ifelse(wind_speed %in% c(9999, 99999), NA, wind_speed / 10),
      air_pressure = ifelse(air_pressure %in% c(99999, 999999), NA, air_pressure / 10)
    ) %>%
    filter(!is.na(date)) %>%
    group_by(date) %>%
    summarise(
      air_temp = mean(air_temp, na.rm = TRUE),
      wind_speed = mean(wind_speed, na.rm = TRUE),
      air_pressure = mean(air_pressure, na.rm = TRUE),
      .groups = "drop"
    )
}

weather_primary <- get_daily_weather(
  primary_station$usaf[1],
  primary_station$wban[1],
  years_primary
)

weather_backup <- get_daily_weather(
  backup_station$usaf[1],
  backup_station$wban[1],
  years_backup
)

weather_daily <- full_join(weather_primary, weather_backup, by = "date",
                           suffix = c("_primary", "_backup")) %>%
  mutate(
    air_temp = coalesce(air_temp_primary, air_temp_backup),
    wind_speed = coalesce(wind_speed_primary, wind_speed_backup),
    air_pressure = coalesce(air_pressure_primary, air_pressure_backup),
    station_used = ifelse(!is.na(air_temp_primary) |
                            !is.na(wind_speed_primary) |
                            !is.na(air_pressure_primary),
                          "726357", "725396")
  ) %>%
  select(date, air_temp, wind_speed, air_pressure, station_used)

# Check values
summary(weather_daily$air_temp)
summary(weather_daily$wind_speed)
summary(weather_daily$air_pressure)

kar <- kar %>%
  left_join(
    weather_daily %>% select(-station_used),
    by = c("SampleDate" = "date"))

# Save
write.csv(kar, "Data/Kalamazoo/KalamazooRiverMeteo.csv", row.names = FALSE)

