
# Install packages
install.packages("ggplot2")
install.packages("scales")
install.packages("tidyr")
install.packages("dplyr")
install.packages("tibble")

# Load libraries
{
  library(ggplot2)
  library(scales)
  library(tidyr)
  library(dplyr)
  library(tibble)
}

# Read flux data ----------------------------------------------------------
# Flux generated for each location

anr <- read.csv("Output/Data/AnacostiaRiver/FluxAnacostiaRiver2016_2017_2018.csv",
                check.names = FALSE)
anr.tpcb <- anr[c("SampleDate", "tPCB")]
anr.tpcb$Site <- "Anacostia River"

fxr <- read.csv("Output/Data/FoxRiver/tPCBFluxFoxRiver2014_2018.csv",
                check.names = FALSE)
fxr.tpcb <- fxr[c("SampleDate", "tPCB")]
fxr.tpcb$Site <- "Fox River"

hor <- read.csv("Output/Data/HousatonicRiver/tPCBFluxHousatonicRiver2006_2007.csv",
                check.names = FALSE)
hor.tpcb <- hor[c("SampleDate", "tPCB")]
hor.tpcb$Site <- "Housatonic River"

hur <- read.csv("Output/Data/HudsonRiverAlbany/FluxHudsonRiverAlbany2017.csv",
                check.names = FALSE)
hur.tpcb <- hur[c("SampleDate", "tPCB")]
hur.tpcb$Site <- "Hudson River"

kar <- read.csv("Output/Data/Kalamazoo/tPCBFluxKalamazooRiver2000_2001.csv",
                check.names = FALSE)
kar.tpcb <- kar[c("SampleDate", "tPCB")]
kar.tpcb$Site <- "Kalamazoo River"

par <- read.csv("Output/Data/PassaicRiver/tPCBFluxPassaicRiver2018_2019.csv",
                check.names = FALSE)
par.tpcb <- par[c("SampleDate", "tPCB")]
par.tpcb$Site <- "Passaic River"

hur <- read.csv("Output/Data/Spokane/FluxHudsonRiverAlbany2017.csv",
                check.names = FALSE)
hur.tpcb <- hur[c("SampleDate", "tPCB")]
hur.tpcb$Site <- "Hudson River"



