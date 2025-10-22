# Results from the flux calculations

# Install packages
{
  install.packages("ggplot2")
  install.packages("scales")
  install.packages("reshape")
}

# Libraries
{
  library(ggplot2)
  library(scales)
  library(grid) 
}

# Read data from Output/Data/FoxRiver folder
{
  as <- read.csv("Output/Data/FoxRiver/FluxFoxRiverAlexanderSt.csv")
  lw <- read.csv("Output/Data/FoxRiver/FluxFoxRiverLakeWinnebago.csv")
  rm <- read.csv("Output/Data/FoxRiver/FluxFoxRiverFoxRiverMouth.csv")
  o1 <- read.csv("Output/Data/FoxRiver/FluxFoxRiverOperableUnit1.csv")
  oa <- read.csv("Output/Data/FoxRiver/FluxFoxRiverOperableUnit2A.csv")
  ob <- read.csv("Output/Data/FoxRiver/FluxFoxRiverOperableUnit2B.csv")
  oc <- read.csv("Output/Data/FoxRiver/FluxFoxRiverOperableUnit2C.csv")
  o3 <- read.csv("Output/Data/FoxRiver/FluxFoxRiverOperableUnit3.csv")
  # Combine dates frames
  flux_fr <- rbind(as, lw, rm, o1, oa, ob, oc, o3)
}

# General plot
# Convert SampleDate to Date format
flux_fr$SampleDate <- as.Date(flux_fr$SampleDate, format = "%Y-%m-%d")

# Select symbol shapes
shape_values <- c(21, 22, 23, 24, 25, 0, 1, 2) 

# Select colors
okabe_ito <- c(
  "#E69F00", # orange
  "#56B4E9", # sky blue
  "#009E73", # bluish green
  "#F0E442", # yellow
  "#0072B2", # blue
  "#D55E00", # vermillion
  "#CC79A7", # reddish purple
  "#999999"  # gray
)

# plot
p.flux.tpcb <- ggplot(flux_fr, aes(x = SampleDate, y = tPCB, color = SampleSite, fill = SampleSite,
                    shape = SampleSite)) +
  geom_point(size = 3.5, alpha = 0.9, stroke = 1) +
  scale_shape_manual(values = shape_values) +
  scale_color_manual(values = okabe_ito) +
  scale_fill_manual(values = okabe_ito) +
  scale_y_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x)),
    limits = c(0.5, 10^7.5)) +
  scale_x_date(
    date_breaks = "6 months", date_labels = "%b-%Y", expand = c(0.02, 0.02)) +
  xlab(expression(bold(""))) +
  labs(y = expression(bold(Sigma*"PCB Flux (pg/m"^2*"/d)")), color = "Sample Site",
       shape = "Sample Site", fill = "Sample Site") +
  theme_bw() +
  theme(
    aspect.ratio = 1/3,
    axis.text.y = element_text(face = "bold", size = 9),
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 9),
    axis.title.y = element_text(face = "bold", size = 11),
    axis.title.x = element_text(face = "bold", size = 11),
    axis.ticks = element_line(linewidth = 0.8, color = "black"), 
    axis.ticks.length = unit(0.2, "cm"),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    legend.key.size = unit(0.6, "cm"),
    legend.text = element_text(size = 9),
    legend.title = element_text(face = "bold", size = 10)
  )

# See plot
p.flux.tpcb

# Save plot
ggsave("Output/Plot/FoxRiver/FluxtPCB.jpg", plot = p.flux.tpcb,
       width = 10, height = 4, dpi = 500)
