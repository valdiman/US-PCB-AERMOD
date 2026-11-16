

# Install Packages
{
  install.packages("tidyverse")
  install.packages("ggplot2")
}

# Load Libraries
library(tidyverse)
library(ggplot2)

# Read data from Data Folder
# Concentration in pg/L [pg/m3]
transec.dat <- read.csv("Data/Aermod/ShoreNormalization.csv",
                        stringsAsFactors = FALSE)

T1 <- transec.dat %>%
  transmute(
    StudySite,
    Transect = "T1",
    `0` = 100,
    `250` = T1_250 / T1_0 * 100,
    `500` = T1_500 / T1_0 * 100,
    `750` = T1_750 / T1_0 * 100,
    `1000` = T1_1000 / T1_0 * 100
  )

T2 <- transec.dat %>%
  transmute(
    StudySite,
    Transect = "T2",
    `0` = 100,
    `250` = T2_250 / T2_0 * 100,
    `500` = T2_500 / T2_0 * 100,
    `750` = T2_750 / T2_0 * 100,
    `1000` = T2_1000 / T2_0 * 100
  )

profiles <- bind_rows(T1, T2) %>%
  pivot_longer(
    cols = c(`0`, `250`, `500`, `750`, `1000`),
    names_to = "Distance",
    values_to = "C_relative"
  ) %>%
  mutate(Distance = as.numeric(Distance))

profiles <- profiles %>%
  mutate(
    Distance = if_else(
      Transect == "T1",
      -abs(Distance),
      abs(Distance)
    )
  )

p <- ggplot(
  profiles,
  aes(
    x = abs(Distance),
    y = C_relative,
    fill = StudySite
  )
) +
  # Colored observations at 250–1000 m
  # Thin lines connecting receptor values
  geom_line(
    aes(color = StudySite, group = StudySite),
    linewidth = 0.45,
    alpha = 0.7,
    show.legend = FALSE
  ) +
  
  # Colored circles with black borders
  geom_point(
    data = subset(profiles, abs(Distance) > 0),
    shape = 21,
    size = 2.8,
    stroke = 0.3,
    color = "black"
  ) +
  
  # Common shoreline reference
  geom_point(
    data = data.frame(
      Transect = c("T1", "T2"),
      Distance = c(0, 0),
      C_relative = c(100, 100)
    ),
    aes(x = Distance, y = C_relative),
    inherit.aes = FALSE,
    shape = 23,
    size = 3.5,
    fill = "black"
  ) +
  guides(
    color = "none",
    fill = guide_legend(ncol = 3, byrow = TRUE)
  ) +
  facet_wrap(
    ~Transect,
    nrow = 1,
    ncol = 2,
    labeller = as_labeller(c(
      T1 = "(a) Transect 1",
      T2 = "(b) Transect 2"
    ))
  ) +
  scale_x_continuous(
    breaks = c(0, 250, 500, 750, 1000)
  ) +
  scale_y_log10(
    breaks = c(1, 2, 5, 10, 20, 50, 100)
  ) +
  scale_fill_discrete(
    labels = c(
      "AnacostiaRiver"   = "Anacostia River",
      "FoxRiver2014"     = "Fox River 2014",
      "FoxRiver2018"     = "Fox River 2018",
      "HousatonicRiver"  = "Housatonic River",
      "HudsonRiver"      = "Hudson River",
      "IndianaHarbor"    = "Indiana Harbor",
      "KalamazooRiver"   = "Kalamazoo River",
      "NewBedfordHarbor" = "New Bedford Harbor",
      "PassaicRiver"     = "Passaic River",
      "PortlandHarbor"   = "Portland Harbor",
      "SpokaneRiver2014" = "Spokane River 2014",
      "SpokaneRiver2016" = "Spokane River 2016"
    )
  ) +
  labs(
    x = "Distance from shoreline (m)",
    y = "Normalized PCB concentration (% of shoreline)",
    fill = NULL
  ) +
  theme_bw(base_size = 18) +
  theme(
    # Axis titles
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    
    # Axis numbers
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    
    # Panel titles
    strip.text = element_text(
      size = 20,
      face = "bold"
    ),
    
    # Legend
    legend.position = "bottom",
    legend.text = element_text(size = 14),
    legend.key.size = unit(0.5, "cm"),
    legend.spacing.x = unit(0.2, "cm"),
    
    # Grid
    panel.grid.minor = element_blank()
  )

p

ggsave("Output/Plot/Aermod/PCB_Normalized_Transects.png", plot = p,
       width = 8, height = 10, units = "in", dpi = 600)


