
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
startDate <- min(anr$SampleDate)
endDate <- max(anr$SampleDate)
param = '00010'
usgs = c('01651003')

df <- dataRetrieval::readNWISuv(usgs,parameterCd = param,
                                startDate = startDate,
                                endDate = endDate)


startDate = '2020-04-11'
df <- dataRetrieval::readNWISuv(usgs, parameterCd = param,
                                startDate = startDate)


usgs <- c('01651827')

startDate <- '2024-04-01'

res_qw <- readWQPqw(
  siteNumber = station,
  parameterCd  = "00010",
  startDate    = startDate,
  endDate      = as.character(Sys.Date())
)

param <- '0010'
df = dataRetrieval::readNWISuv(usgs, parameterCd = param, startDate = startDate)

nrow(res_qw); head(res_qw)

