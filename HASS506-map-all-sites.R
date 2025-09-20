

library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)
library(readr)
nodes <- read_csv("nodes.csv")
world <- ne_countries(scale = "medium", returnclass = "sf")
iran <- world[world$name == "Iran", ]
# Add new column for legend
nodes_long <- nodes %>%
  mutate(site_display = paste(Site_Code, Site_Name, sep = " - "))

ggplot(data = iran) +
  geom_sf(fill = "whitesmoke", color = "black") +
  geom_point(
    data = nodes_long,
    aes(x = Long, y = Lat, color = site_display),
    size = 2,
    position = position_jitter(width = 0.05, height = 0.05)
  ) +
  geom_text_repel(
    data = nodes_long,
    aes(x = Long, y = Lat, label = Site_Code),
    size = 3,
    nudge_x = 0.1,
    nudge_y = 0.1,
    box.padding = 0.5,
    point.padding = 0.5,
    segment.size = 0.5,
    segment.color = "grey50",
    min.segment.length = 0,
    max.overlaps = Inf
  )+
  #  coord_sf(
  #    xlim = c(51, 54),  # longitude limits to zoom map
  #   ylim = c(29, 31),  # latitude limits to zoom map
  #    expand = FALSE
  #  ) +
  labs(
    title = "Site Locations in Iran",
    color = "Site Code - Site Name"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )


