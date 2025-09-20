library(raster)
library(sf)
library(ggplot2)
library(ggrepel)
library(readr)
library(dplyr)
library(rnaturalearth)

# Load relief raster (assumes file is in working directory)
relief <- raster("NE2_HR_LC_SR_W.tif")

# Load world and extract Iran polygon
world <- st_read(system.file("shape/nc.shp", package="sf"), quiet = TRUE)  # example; replace with rnaturalearth

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
iran <- world %>% filter(name == "Iran")

# Crop relief to Iran bounding box plus some buffer
relief_cropped <- crop(relief, extent(iran) + 1)
#zoom_extent <- extent(51.3, 53.7, 29, 31)
#relief_cropped <- crop(relief, zoom_extent)


# Convert raster to dataframe for ggplot
relief_df <- as.data.frame(rasterToPoints(relief_cropped))
colnames(relief_df) <- c("x", "y", "value")

# Load your nodes data and prepare site_display
nodes <- read_csv("nodes.csv") %>% 
  mutate(site_display = paste(Site_Code, Site_Name, sep = " - "))

# Plot
ggplot() + 
  geom_raster(data = relief_df, aes(x = x, y = y, fill = value), alpha = 0.4)+
  scale_fill_gradient(low = "gray90", high = "gray50", guide = "none")+
  geom_sf(data = iran, fill = NA, color = "black", size = 0.5) +

  geom_point(
    data = nodes,
    aes(x = Long, y = Lat, color = site_display),
    size = 2,
    position = position_jitter(width = 0.01, height = 0.01)
  ) +
  geom_text_repel(
    data = nodes,
    aes(x = Long, y = Lat, label = Site_Code),
    size = 3,
    nudge_x = 0,
    nudge_y = 0.05,
    box.padding = 0.5,
    point.padding = 0.5,
    segment.size = 0.5,
    segment.color = "grey50",
    min.segment.length = 0,
    max.overlaps = Inf,
   # segment.curvature = 0.2,       # increase curvature (default is 0)
  #  segment.ncp = 10,              # number of control points for curvature
  #  segment.angle = 20             # angle of curvature in degrees
  )+
  coord_sf(
    xlim = c(51.3, 53.7),  # longitude limits to zoom map
    ylim = c(29, 31),  # latitude limits to zoom map
    expand = FALSE
  ) +
  labs(color = "Site Code - Site Name") +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )


