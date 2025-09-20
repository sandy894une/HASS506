library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)
library(readr)
library(dplyr)
library(tidyr)

# Load data
nodes <- read_csv("nodes.csv")

# Convert yes/no to binary if needed (example below assumes 1/0 already)
 nodes <- nodes %>% mutate(across(starts_with("Ware"), ~ ifelse(. == "yes", 1, 0)))

# Get map data
world <- ne_countries(scale = "medium", returnclass = "sf")
iran <- world[world$name == "Iran", ]

# Convert data to long format for easier plotting of Ware columns
nodes_long <- nodes %>%
  pivot_longer(cols = starts_with("Ware"), names_to = "WareType", values_to = "Present") %>%
  filter(Present == 1)  # Keep only sites where ware is present

# Plot map with Ware presence by shape and color
ggplot(data = iran) +
  geom_sf(fill = "antiquewhite", color = "black") +
  geom_point(
    data = nodes_long,
    aes(x = Long, y = Lat, shape = WareType, color = WareType),
    size = 4,
    position = position_jitter(width = 0.03, height = 0.03)
  ) +
  geom_text_repel(
    data = nodes,
    aes(x = Long, y = Lat, label = Site_Code),
    size = 3,
    max.overlaps = Inf
  ) +
  coord_sf(xlim = c(50, 56), ylim = c(28, 32), expand = FALSE) +
  labs(
    title = "Site Locations in Iran by Ware Presence",
    x = "Longitude",
    y = "Latitude",
    color = "Ware Type",
    shape = "Ware Type"
  ) +
  theme_minimal()
