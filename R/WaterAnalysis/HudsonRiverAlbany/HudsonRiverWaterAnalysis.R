# Evaluate Hudson River water PCB data

# Packages and libraries needed --------------------------------------------
# Install packages
{
  install.packages("ggplot2")
}

# Libraries
{
  library(ggplot2)
}

# Read data ---------------------------------------------------------------
# Read water concentrations
hur <- read.csv("Data/HudsonRiverAlbany/HudsonRiver_env.csv")

# Select Site Name near Albany
hur.site <- hur[hur$SiteID %in% c(
  "WCPCB-HUD004",
  "WCPCB-HUD016",
  "WCPCB-HUD005",
  "WCPCB-HUD001",
  "WCPCB-HUD002"), ]

# Select  locations, sampling date, lat, long and tPCB
tpcb <- data.frame(cbind(hur.site$SiteName, hur.site$SampleDate,
                         hur.site$Latitude, hur.site$Longitude,
                         hur.site$tPCB))

# Name the columns
colnames(tpcb) <- c("Site", "SampleDate", "Latitude", "Longitude",
                      "tPCB")

# Change to numeric
tpcb[, 3:5] <- lapply(tpcb[, 3:5], as.numeric)
# Change date format
tpcb$SampleDate <- as.Date(tpcb$SampleDate)

# Descriptive stats
summary(tpcb$tPCB)
tpcb.max <- max(tpcb$tPCB)

# remove higher value
tpcb.2 <- tpcb %>%
  filter(tPCB < max(tPCB, na.rm = TRUE))

tpcb.3 <- tpcb.2 %>%
  filter(tPCB < max(tPCB, na.rm = TRUE))

# Histogram
hist(tpcb.3$tPCB)
hist(log10(tpcb$tPCB))

# Box plot
ggplot(tpcb.3, aes(x = "", y = tPCB)) +
  geom_boxplot(lwd = 0.8, width = 0.7,
               outlier.shape = NA, alpha = 0.7) +
  geom_point(shape = 21, color = "black", position = position_jitter(0.3),
             size = 1.5, alpha = 0.6) +
  xlab(expression(bold(" "))) +
  labs(y = expression(bold("Water Concentration " *Sigma*"PCB (pg/L)"))) +
  theme_bw() +
  theme(aspect.ratio = 5/1,
        axis.text.y = element_text(face = "bold", size = 9),
        axis.title.y = element_text(face = "bold", size = 11),
        axis.title.x = element_text(face = "bold", size = 11,
                                    angle = 60, hjust = 0, vjust = 0.3),
        axis.ticks = element_line(linewidth = 0.8, color = "black"), 
        axis.ticks.length = unit(0.2, "cm"))

# Spatial plot
ggplot(tpcb, aes(x = factor(Site), y = tPCB)) + 
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

ggplot(tpcb, aes(x = Site, y = tPCB, group = SampleDate)) + 
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

ggplot(tpcb, aes(x = format(SampleDate,'%Y%m%d'), y = tPCB)) +
  geom_point() +
  xlab("") +
  theme_bw() +
  theme(aspect.ratio = 10/20) +
  ylab(expression(bold(Sigma*"PCB (pg/L)"))) +
  theme(axis.text.y = element_text(face = "bold", size = 9),
        axis.title.y = element_text(face = "bold", size = 9)) +
  theme(axis.text.x = element_text(face = "bold", size = 8,
                                   angle = 60, hjust = 1),
        axis.title.x = element_text(face = "bold", size = 8)) +
  theme(axis.ticks = element_line(linewidth = 0.8, color = "black"), 
        axis.ticks.length = unit(0.2, "cm"))





