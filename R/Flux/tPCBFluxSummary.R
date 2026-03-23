#


# Packages and libraries --------------------------------------------------
# Install packages
install.packages("ggplot2")

# Load libraries
library(ggplot2)
library(scales)
library(dplyr)

# Read flux data ----------------------------------------------------------
# Flux generated for each location

anr <- read.csv("Output/Data/AnacostiaRiver/fluxPCBAnacostiaRiver2016_2017_2018.csv",
                check.names = FALSE)
anr.tpcb <- anr[c("SampleDate", "tPCB")]
anr.tpcb$Site <- "Anacostia River"

fxr <- read.csv("Output/Data/FoxRiver/fluxPCBFoxRiver2014_2018.csv",
                check.names = FALSE)
fxr.tpcb <- fxr[c("SampleDate", "tPCB")]
fxr.tpcb$Site <- "Fox River"

hor <- read.csv("Output/Data/HousatonicRiver/fluxPCBHousatonicRiver2006_2007.csv",
                check.names = FALSE)
hor.tpcb <- hor[c("SampleDate", "tPCB")]
hor.tpcb$Site <- "Housatonic River"

hur <- read.csv("Output/Data/HudsonRiver/fluxPCBHudsonRiver2017.csv",
                check.names = FALSE)
hur.tpcb <- hur[c("SampleDate", "tPCB")]
hur.tpcb$Site <- "Hudson River"

kar <- read.csv("Output/Data/KalamazooRiver/fluxPCBKalamazooRiver2000_2001.csv",
                check.names = FALSE)
kar.tpcb <- kar[c("SampleDate", "tPCB")]
kar.tpcb$Site <- "Kalamazoo River"

par <- read.csv("Output/Data/PassaicRiver/fluxPCBPassaicRiver2018_2019.csv",
                check.names = FALSE)
par.tpcb <- par[c("SampleDate", "tPCB")]
par.tpcb$Site <- "Passaic River"

spr <- read.csv("Output/Data/SpokaneRiver/fluxPCBSpokaneRiver2014_2016.csv",
                check.names = FALSE)
spr.tpcb <- spr[c("SampleDate", "tPCB")]
spr.tpcb$Site <- "Spokane River"

# Combine data ------------------------------------------------------------
flux.tpcb <- rbind(anr.tpcb, fxr.tpcb, hor.tpcb, hur.tpcb, kar.tpcb, par.tpcb, spr.tpcb)

# Descriptive stats
summary(flux.tpcb$tPCB)

# Calculate geometric mean per site
geo_mean <- function(x, na.rm = TRUE) {
  if (na.rm) x <- x[!is.na(x)]
  x <- x[x > 0]
  exp(mean(log(x)))
}

gm.flux.tpcb <- flux.tpcb %>%
  group_by(Site) %>%
  summarise(GM = geo_mean(tPCB)) %>%
  mutate(units = "ng/m2/d")

# Save data ---------------------------------------------------------------
write.csv(gm.flux.tpcb, "Output/Data/gmFluxtPCBAllSites.csv",
          row.names = FALSE)

# Plot
# Time series
# Change SampleDate to date format
flux.tpcb$SampleDate <- as.Date(flux.tpcb$SampleDate)

plot.tflux <- ggplot(flux.tpcb, aes(x = SampleDate, y = tPCB, color = Site)) +
  geom_point(shape = 21, fill = NA, size = 4, stroke = 1.2) +
  labs(x = NULL, y = expression(Sigma*"PCB Flux (ng/"*m^2*"/d)")) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b-%Y") +
  scale_y_log10(limits = c(0.001, 10^6),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw(base_size = 14) +
  theme(aspect.ratio = 8/16,
        axis.text.x = element_text(angle = 45, hjust = 1))

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
