
# Install packages
install.packages("dataRetrieval")

# Load libraries
{
  library(dataRetrieval) # read data from USGS
  library(dplyr)          # data manipulation
  library(sf)             # for handling sf objects (used by read_waterdata_daily)
  library(zoo) # for interpolation
  library(lubridate)
}

# Read data ---------------------------------------------------------------
# From Data Folder, FoxRiver.csv
fx <- read.csv("Data/FoxRiver/FoxRiver.csv")

# Convert from "m/d/YYYY" to Date
fx$SampleDate <- as.Date(fx$SampleDate, format = "%m/%d/%Y")

# Define USGS site and parameter ------------------------------------------
sitefoxN2 <- "040851385" # FOX RIVER AT OIL TANK DEPOT AT GREEN BAY, WI
paramtemp <- "00010"     # water temperature, °C

# Fetch daily water temperature -------------------------------------------
temp <- read_waterdata_daily(
  monitoring_location_id = paste("USGS", sitefoxN2, sep = "-"),
  parameter_code = paramtemp,
  statistic_id = "00003",  # mean daily values
  time = c(as.character(min(fx$SampleDate)), as.character(max(fx$SampleDate)))
)

# Clean USGS data
temp_values <- temp %>%
  st_drop_geometry() %>%
  select(time, value)

# Merge USGS temperature into fx
fx <- fx %>%
  left_join(temp_values, by = c("SampleDate" = "time")) %>%
  rename(temp = value)  # final column for water temperature

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
fx <- fx %>%
  arrange(SampleDate) %>%
  mutate(doy = yday(SampleDate)) %>%
  mutate(
    temp_pred_seasonal = fill_seasonal_wrap(temp, doy, window = 7),
    temp_pred_seasonal = zoo::na.locf(temp_pred_seasonal, na.rm = FALSE),
    temp_pred_seasonal = zoo::na.locf(temp_pred_seasonal, fromLast = TRUE)
  ) %>%
  select(-doy)

# Create final temperature column
fx <- fx %>%
  mutate(temp_final = coalesce(temp, temp_pred_seasonal)) %>%
  select(-temp, -temp_pred_seasonal)

# save
write.csv(fx, "Data/FoxRiver/FoxRiver_temp.csv", row.names = FALSE)

