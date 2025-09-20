library(raster)
library(sf)
library(ggplot2)
library(ggrepel)
library(readr)
library(dplyr)
library(rnaturalearth)

# Load relief raster
relief <- raster("NE2_HR_LC_SR_W.tif")

# Load world and extract Iran polygon
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
iran <- world %>% filter(name == "Iran")

# Crop relief raster to Iran extent + buffer
relief_cropped <- crop(relief, extent(iran) + 1)
relief_df <- as.data.frame(rasterToPoints(relief_cropped))
colnames(relief_df) <- c("x", "y", "value")

# Load nodes data
nodes <- read_csv("ca-test.csv")

# Reshape Ware presence columns to long format and filter to present only
nodes_long <- nodes %>%
  pivot_longer(cols = starts_with("Ware"),
               names_to = "WareType",
               values_to = "Present") %>%
  filter(Present == 1)

# Make WareType factor with correct order
ware_levels <- unique(names(nodes)[grepl("^Ware", names(nodes))])
nodes_long$WareType <- factor(nodes_long$WareType, levels = ware_levels)

# Add combined site display for legend
nodes_long <- nodes_long %>%
  mutate(site_display = paste(Site_Code, Site_Name, sep = " - "))

# Split data by WareType for individual plots
node_splits <- split(nodes_long, nodes_long$WareType)

# Function to create individual plot per WareType
create_ware_plot <- function(df, ware_type){
  ggplot() +
    geom_raster(data = relief_df, aes(x = x, y = y, fill = value), alpha = 0.4) +
    scale_fill_gradient(low = "gray90", high = "gray50", guide = "none") +
    geom_sf(data = iran, fill = NA, color = "black", size = 0.5) +
    geom_point(data = df, aes(x = Long, y = Lat, color = site_display), size = 2,
               position = position_jitter(width = 0.05, height = 0.05)) +
 #   geom_text_repel(data = df, aes(x = Long, y = Lat, label = Site_Code),
#                    size = 3, box.padding = 0.5, point.padding = 0.5,
#                   segment.size = 0.5, segment.color = "grey50", min.segment.length = 0,
#                  max.overlaps = Inf, nudge_y = 0.05) +
#    coord_sf(xlim = c(51.3, 53.7), ylim = c(29, 31), expand = FALSE) +
    labs(title = paste("Distribution of ", ware_type),
         color = "Site Code - Site Name") +
    theme_minimal() +
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank())
}

# Create plots for all WareTypes
plot_list <- lapply(names(node_splits), function(wt){
  create_ware_plot(node_splits[[wt]], wt)
})

# Optionally print first plot to screen
print(plot_list[[1]])
print(plot_list[[2]])
print(plot_list[[3]])
