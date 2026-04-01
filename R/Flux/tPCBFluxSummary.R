# Code summarizes the flux calcualtion from all the sites
# for statistical description and visualization

# Packages and libraries --------------------------------------------------
# Install packages
install.packages("ggplot2")
install.packages("scales")
install.packages("dplyr")

# Load libraries
{
  library(ggplot2)
  library(scales)
  library(dplyr)
}
  

# Read flux data ----------------------------------------------------------
# Flux generated for each location
# Anacostia River
anr <- read.csv("Output/Data/AnacostiaRiver/fluxPCBAnacostiaRiver2016_2017_2018.csv",
                check.names = FALSE)
anr.tpcb <- anr[c("SampleDate", "tPCB")]
anr.tpcb$Site <- "Anacostia River"
# Fox River
fxr <- read.csv("Output/Data/FoxRiver/fluxPCBFoxRiver2014_2018.csv",
                check.names = FALSE)
fxr.tpcb <- fxr[c("SampleDate", "tPCB")]
fxr.tpcb$Site <- "Fox River"
# Housatonic River
hor <- read.csv("Output/Data/HousatonicRiver/fluxPCBHousatonicRiver2006_2007.csv",
                check.names = FALSE)
hor.tpcb <- hor[c("SampleDate", "tPCB")]
hor.tpcb$Site <- "Housatonic River"
#Hudson River
hur <- read.csv("Output/Data/HudsonRiver/fluxPCBHudsonRiver2017.csv",
                check.names = FALSE)
hur.tpcb <- hur[c("SampleDate", "tPCB")]
hur.tpcb$Site <- "Hudson River"
# Kalamazoo River
kar <- read.csv("Output/Data/KalamazooRiver/fluxPCBKalamazooRiver2000_2001.csv",
                check.names = FALSE)
kar.tpcb <- kar[c("SampleDate", "tPCB")]
kar.tpcb$Site <- "Kalamazoo River"
# Passaic River
par <- read.csv("Output/Data/PassaicRiver/fluxPCBPassaicRiver2018_2019.csv",
                check.names = FALSE)
par.tpcb <- par[c("SampleDate", "tPCB")]
par.tpcb$Site <- "Passaic River"
# Spokane River
spr <- read.csv("Output/Data/SpokaneRiver/fluxPCBSpokaneRiver2014_2016.csv",
                check.names = FALSE)
spr.tpcb <- spr[c("SampleDate", "tPCB")]
spr.tpcb$Site <- "Spokane River"

# Combine data ------------------------------------------------------------
flux.tpcb <- rbind(anr.tpcb, fxr.tpcb, hor.tpcb, hur.tpcb, kar.tpcb, par.tpcb, spr.tpcb)

flux.tpcb <- flux.tpcb %>%
  rename(tPCBFlux = tPCB)

# Add fluxes already calculated from IHSC (2017), New Bedford Harbor (2015) and
# New Bedford Harbor (2015)
new_rows <- data.frame(
  SampleDate = as.Date(c(
    "2015-08-10", "2015-08-11", "2015-08-12",
    "2015-08-10", "2015-08-11", "2015-08-12",
    "2015-08-10", "2015-08-11", "2015-08-12",
    "2015-08-12", "2015-08-10", "2015-08-11",
    "2015-08-12")),
  tPCBFlux = c(
    608404.8397, 474304.9784, 241960.3095,
    241838.8647, 271093.9919, 217076.9577,
    433252.0160, 509458.3228, 462649.2342,
    1115874.8830, 196906.9311, 152143.7367,
    187540.6617),
  Site = rep("New Bedford Harbor", 13))

flux.tpcb <- bind_rows(flux.tpcb, new_rows)

# IHSC (2017)
# Date corresponds to the midpoint date (passive sampler)
new_rows_ihsc <- data.frame(
  SampleDate = as.Date(c(
    "2016-12-24", "2017-02-08", "2017-03-12",
    "2017-04-13", "2017-05-10", "2017-06-10",
    "2017-07-13", "2017-08-22", "2017-09-17",
    "2017-10-08")),
  tPCBFlux = c(
    1420, 1636.046832, 1635.398458,
    2511.386208, 2255.578066, 2200.093666,
    2923.362157, 2214.981746, 2273.364835,
    3026.390723),
  Site = rep("IHSC", 10))

flux.tpcb <- bind_rows(flux.tpcb, new_rows_ihsc)

# Portland Harbor (2018)



# Descriptive stats
summary(flux.tpcb$tPCBFlux)
min(flux.tpcb$tPCBFlux)

# Calculate geometric mean per site
# Create function
geo_mean <- function(x, na.rm = TRUE) {
  if (na.rm) x <- x[!is.na(x)]
  x <- x[x > 0]
  exp(mean(log(x)))
}
# Calculate gm using function
gm.flux.tpcb <- flux.tpcb %>%
  group_by(Site) %>%
  summarise(GM = geo_mean(tPCBFlux)) %>%
  mutate(units = "ng/m2/d")

# Save data ---------------------------------------------------------------
write.csv(gm.flux.tpcb, "Output/Data/gmFluxtPCBAllSites.csv",
          row.names = FALSE)

# Plot
# Time series
# Change SampleDate to date format
flux.tpcb$SampleDate <- as.Date(flux.tpcb$SampleDate)

plot.tflux <- ggplot(flux.tpcb, aes(x = SampleDate, y = tPCBFlux, color = Site)) +
  geom_point(shape = 21, fill = NA, size = 2.5, stroke = 1.2) +
  labs(x = NULL, y = expression(Sigma*"PCB Flux (ng/"*m^2*"/d)")) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b-%Y") +
  scale_y_log10(limits = c(0.001, 10^6),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw(base_size = 12) +
  theme(aspect.ratio = 8/16,
        axis.text.x = element_text(angle = 45, hjust = 1))

# See plot
plot.tflux

# Save plot in folder
ggsave("Output/Plot/fluxtPCBAllSites.png",
       plot = plot.tflux, width = 16, height = 8, dpi = 500)

box.plot.tflux <- ggplot(flux.tpcb, aes(x = Site, y = tPCB, color = Site)) +
  geom_boxplot(outlier.shape = NA) +
  stat_summary(aes(shape = "GM"), fun = geo_mean, geom = "point",
    size = 4, color = "black") +
  scale_shape_manual(name = "", values = c(GM = 18)) +
  scale_y_log10(limits = c(0.001, 10^6),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  labs(x = NULL, y = expression(Sigma*"PCB Flux (ng/"*m^2*"/d)"),
    color = "Site") +
  theme_bw(base_size = 14)

box.plot.tflux

# Save plot in folder
ggsave("Output/Plot/boxplotfluxtPCBAllSites.png",
       plot = box.plot.tflux, width = 16, height = 8, dpi = 500)
