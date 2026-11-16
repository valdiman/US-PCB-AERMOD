

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
fox.dat <- read.csv("Data/Aermod/FoxRiver3Sources.csv",
                        stringsAsFactors = FALSE)

# Prepare data
fox.dat <- fox.dat %>%
  mutate(
    date = as.Date(date, format = "%m/%d/%Y"),
    date_label = format(date, "%b %d")
  ) %>%
  arrange(date)

# Create bar plot
p_fox <- ggplot(
  fox.dat,
  aes(
    x = factor(date_label, levels = date_label),
    y = PCB_concentration
  )
) +
  geom_col(
    fill = "#E69F00",
    color = "black",
    linewidth = 0.5,
    width = 0.7
  ) +
  scale_y_continuous(
    breaks = seq(0, 0.05, 0.01),
    limits = c(0, 0.05),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    x = " ",
    y = expression("PCB concentration (pg/m"^3*")")
  ) +
  theme_bw(base_size = 18) +
  theme(
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 16),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

p_fox

ggsave("Output/Plot/Aermod/FoxRiver3Sources.png", plot = p_fox,
       width = 8, height = 10, units = "in", dpi = 600)
