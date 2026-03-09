# Evaluate KALson River water PCB data

# Packages and libraries needed --------------------------------------------
# Install packages
{
  install.packages("ggplot2")
  install.packages("dplyr")
}

# Libraries
{
  library(ggplot2)
  library(dplyr)
}

# Read data ---------------------------------------------------------------
# Read water concentrations
kar <- read.csv("Data/Kalamazoo/KalamazooRiver_env.csv")

# Select Site Name near Albany
kar.site <- kar[kar$SiteID %in% c(
  "WCPCB-KAL002",
  "WCPCB-KAL013",
  "WCPCB-KAL020",
  "WCPCB-KAL025"), ]

# Select  locations, sampling date, lat, long and tPCB
tpcb <- data.frame(
  SiteName   = kar.site$SiteName,
  SampleDate = kar.site$SampleDate,
  Latitude   = kar.site$Latitude,
  Longitude  = kar.site$Longitude,
  tPCB       = kar.site$tPCB)

# Change to numeric
tpcb[, 3:5] <- lapply(tpcb[, 3:5], as.numeric)
# Change date format
tpcb$SampleDate <- as.Date(tpcb$SampleDate)

# Descriptive stats
summary(tpcb$tPCB)

# Histogram
ggplot(tpcb, aes(x = tPCB)) +
  geom_histogram(aes(y = ..density..),
                 bins = 10,
                 fill = "grey70",
                 color = "black",
                 alpha = 0.7) +
  geom_density(color = "blue", linewidth = 1) +
  theme_bw() +
  labs(x = expression(bold(Sigma*"PCB (pg/L)")),
       y = "Density")

ggplot(tpcb, aes(x = log10(tPCB))) +
  geom_histogram(aes(y = ..density..),
                 bins = 10,
                 fill = "grey70",
                 color = "black",
                 alpha = 0.7) +
  geom_density(color = "blue", linewidth = 1) +
  theme_bw() +
  labs(x = expression(bold(Sigma*"PCB (pg/L)")),
       y = "Density")

# basic quantities
mu_log  <- mean(log(tpcb$tPCB), na.rm = TRUE)      # mean of log
sd_log  <- sd(log(tpcb$tPCB), na.rm = TRUE)        # sd of log

# Geometric values
geo_mean <- exp(mu_log)
geo_gsd  <- exp(sd_log)

# Check max value
tpcb.max <- tpcb %>%
  filter(tPCB == max(tPCB, na.rm = TRUE))

# Box plot
ggplot(tpcb, aes(x = "", y = tPCB)) +
  geom_boxplot(width = 0.7, outlier.shape = NA, alpha = 0.7) +
  geom_point(shape = 21, color = "black",
             position = position_jitter(0.3),
             size = 1.5, alpha = 0.6) +
  geom_text(data = tpcb.max,
            aes(label = SiteName),
            vjust = -0.5,
            fontface = "bold") +
  xlab(expression(bold(" "))) +
  labs(y = expression(bold("Water Concentration " * Sigma * "PCB (pg/L)"))) +
  theme_bw() +
  theme(aspect.ratio = 4/1)

# remove higher value
tpcb.2 <- tpcb %>%
  filter(tPCB < max(tPCB, na.rm = TRUE))

# basic quantities (to be used in the Monte Carlo simulation)
mu_log  <- mean(log(tpcb.2$tPCB), na.rm = TRUE)      # mean of log
sd_log  <- sd(log(tpcb.2$tPCB), na.rm = TRUE)        # sd of log

# Geometric values
geo_mean <- exp(mu_log)
geo_gsd  <- exp(sd_log)

# Histogram
ggplot(tpcb.2, aes(x = tPCB)) +
  geom_histogram(aes(y = ..density..),
                 bins = 10,
                 fill = "grey70",
                 color = "black",
                 alpha = 0.7) +
  geom_density(color = "blue", linewidth = 1) +
  theme_bw() +
  labs(x = expression(bold(Sigma*"PCB (pg/L)")),
       y = "Density")

ggplot(tpcb.2, aes(x = log10(tPCB))) +
  geom_histogram(aes(y = ..density..),
                 bins = 10,
                 fill = "grey70",
                 color = "black",
                 alpha = 0.7) +
  geom_density(color = "blue", linewidth = 1) +
  theme_bw() +
  labs(x = expression(bold(Sigma*"PCB (pg/L)")),
       y = "Density")

# Spatial plot
ggplot(tpcb.2, aes(x = factor(SiteName), y = tPCB)) + 
  geom_point() +
  theme_bw() +
  xlab(expression("")) +
  theme(aspect.ratio = 10/18) +
  ylab(expression(bold(Sigma*"PCB (pg/L)"))) +
  theme(axis.text.y = element_text(face = "bold", size = 9),
        axis.title.y = element_text(face = "bold", size = 9)) +
  theme(axis.text.x = element_text(face = "bold", size = 8,
                                   angle = 60, hjust = 1),
        axis.title.x = element_text(face = "bold", size = 8)) +
  theme(axis.ticks = element_line(linewidth = 0.8, color = "black"), 
        axis.ticks.length = unit(0.2, "cm"))

ggplot(tpcb.2, aes(x = SiteName, y = tPCB, group = SampleDate)) + 
  geom_point(aes(color = SampleDate), shape = 1, size  = 2) +
  labs(color = "Date") +
  theme_bw() +
  xlab(expression("")) +
  theme(aspect.ratio = 10/18) +
  ylab(expression(bold(Sigma*"PCB (pg/L)"))) +
  theme(axis.text.y = element_text(face = "bold", size = 9),
        axis.title.y = element_text(face = "bold", size = 9)) +
  theme(axis.text.x = element_text(face = "bold", size = 8,
                                   angle = 60, hjust = 1),
        axis.title.x = element_text(face = "bold", size = 8)) +
  theme(axis.ticks = element_line(linewidth = 0.8, color = "black"), 
        axis.ticks.length = unit(0.2, "cm"))

ggplot(tpcb.2, aes(x = format(SampleDate), y = tPCB)) +
  geom_point() +
  xlab("") +
  theme_bw() +
  theme(aspect.ratio = 10/20) +
  ylab(expression(bold(Sigma*"PCB (pg/L)"))) +
  theme(axis.text.y = element_text(face = "bold", size = 9),
        axis.title.y = element_text(face = "bold", size = 9)) +
  theme(axis.text.x = element_text(face = "bold", size = 7,
                                   angle = 60, hjust = 1),
        axis.title.x = element_text(face = "bold", size = 7)) +
  theme(axis.ticks = element_line(linewidth = 0.8, color = "black"), 
        axis.ticks.length = unit(0.2, "cm"))





