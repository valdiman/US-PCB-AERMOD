# -------------------------------------------------------------------
# Hybrid Air-Temperature–Based Water Temperature Model
#
# Predicted water temperature (°C) was estimated using a hybrid model
# that combines long-term climatological air temperature with daily
# air temperature anomalies from Daymet.
#
# Step 1: Compute daily mean air temperature:
#   T_air = (Tmax + Tmin) / 2
#
# Step 2: Compute long-term climatological mean air temperature
# (1980–2019) for each day-of-year (DOY):
#   T_clim(DOY) = mean(T_air for that DOY across 1980–2019)
#
# Step 3: Decompose daily air temperature into:
#   Baseline component  = β × T_clim(DOY)
#   Anomaly component   = γ × (T_air − T_clim(DOY))
#
# Step 4: Predict water temperature:
#   T_water_pred = β·T_clim + γ·(T_air − T_clim)
#
# Model parameters:
#   β = 0.8  (scales seasonal baseline signal)
#   γ = 0.6  (scales short-term air temperature anomalies)
#
# Air temperature data source:
#   Daymet v4 daily gridded meteorological data
#   https://daymet.ornl.gov/
#
# Note:
# This is a simplified empirical model assuming water temperature
# responds to both seasonal air temperature patterns and short-term
# atmospheric variability.

# Water temperature was predicted using a hybrid air-temperature model:
# T_water = β·T_clim(DOY) + γ·(T_air − T_clim(DOY)),
# where T_air = (Tmax + Tmin)/2 from Daymet,
# T_clim is the 1980–2019 daily climatology,
# β = 0.8 and γ = 0.6.
# -------------------------------------------------------------------
# Install packages
{
  install.packages("dplyr")
  install.packages("lubridate")
  install.packages("daymetr")
  install.packages("tidyr")
  install.packages("readr")
}

# Load libraries
{
  library(daymetr)
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(readr)
}

# Helper: find a single column name matching a pattern (or throw informative error)
find_single_col <- function(df, pattern, what = "column") {
  matches <- grep(pattern, names(df), ignore.case = TRUE, value = TRUE)
  if (length(matches) == 0) {
    stop(sprintf("No %s found matching '%s' in names: %s", what, pattern, pattern, paste(names(df), collapse = ", ")), call. = FALSE)
  }
  if (length(matches) > 1) {
    stop(sprintf("Multiple %ss found matching '%s': %s. Please inspect names(df).", what, pattern, paste(matches, collapse = ", ")), call. = FALSE)
  }
  matches
}

# ---- 1. Read data ----
fxr <- read.csv("Data/FoxRiver/FoxRiver.csv", stringsAsFactors = FALSE)
fxr$SampleDate <- as.Date(fxr$SampleDate)   # ensure Date

# Keep only 2000+
fxr <- fxr %>% filter(SampleDate >= as.Date("2000-01-01"))

# Coordinates FOX RIVER AT OIL TANK DEPOT AT GREEN BAY, WI
lat <- 44.52861
lon <- -88.01

# ---- 2. Download Daymet climatology (1980–2019) ----
dm_clim <- download_daymet("clim", lat, lon, 1980, 2019, internal = TRUE, simplify = TRUE)

# If download_daymet returned a list with a 'data' element, extract it
if (is.list(dm_clim) && "data" %in% names(dm_clim)) {
  dm_clim <- dm_clim$data
}

# pivot to wide and compute tair robustly
wide_clim <- dm_clim %>%
  pivot_wider(names_from = measurement, values_from = value)

# detect tmax/tmin column names (case-insensitive)
tmax_col_clim <- find_single_col(wide_clim, "tmax", "tmax column")
tmin_col_clim <- find_single_col(wide_clim, "tmin", "tmin column")

clim <- wide_clim %>%
  mutate(
    date = as.Date(paste0(year, "-01-01")) + yday - 1,
    doy  = yday(date),
    tair = (.data[[tmax_col_clim]] + .data[[tmin_col_clim]]) / 2
  ) %>%
  group_by(doy) %>%
  summarise(tair_clim = mean(tair, na.rm = TRUE), .groups = "drop")

# ---- 3. Download actual Daymet for sample years ----
yr1 <- year(min(fxr$SampleDate))
yr2 <- year(max(fxr$SampleDate))

dm_act <- download_daymet("act", lat, lon, yr1, yr2, internal = TRUE, simplify = TRUE)

if (is.list(dm_act) && "data" %in% names(dm_act)) {
  dm_act <- dm_act$data
}

wide_act <- dm_act %>%
  pivot_wider(names_from = measurement, values_from = value)

# detect tmax/tmin for actual period
tmax_col_act <- find_single_col(wide_act, "tmax", "tmax column")
tmin_col_act <- find_single_col(wide_act, "tmin", "tmin column")

air <- wide_act %>%
  mutate(
    date = as.Date(paste0(year, "-01-01")) + yday - 1,
    doy  = yday(date),
    tair = (.data[[tmax_col_act]] + .data[[tmin_col_act]]) / 2
  ) %>%
  select(date, doy, tair)

# ---- 4. Hybrid water temperature model ----
beta  <- 0.8
gamma <- 0.6

# Ensure SampleDate is Date and create doy before joins
fxr2 <- fxr %>%
  mutate(
    SampleDate = as.Date(SampleDate),
    doy = yday(SampleDate)
  )

out <- fxr2 %>%
  left_join(select(air, -doy), by = c("SampleDate" = "date")) %>%  # don't import air$doy
  left_join(clim, by = "doy") %>%                                  # now 'doy' from hor2 is present
  mutate(
    baseline    = beta * tair_clim,
    anomaly     = tair - tair_clim,
    pred_temp_C = baseline + gamma * anomaly
  )

# keep original hor columns + predicted temperature
out_pred <- out %>%
  select(all_of(names(fxr)), pred_temp_C)

# ---- 5. Save result ----
write.csv(out_pred, "Data/FoxRiver/FoxRiver_temp2.csv", row.names = FALSE)
