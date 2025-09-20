
Here is the complete standalone R script, updated to use "Type" instead of "WareType", handling multiple "Type" columns, and titling each plot as "Distribution of <Type>":
  
  r
library(raster)
library(sf)
library(ggplot2)
library(ggrepel)
library(readr)
library(dplyr)
library(tidyr)
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
nodes <- read_csv("nodes.csv")
# Specify all Type columns you want to include, including new ones
					

type_cols <- c("Ware 2a", "Ware 2b", "Ware 3", "Clay Tokens", "Shell", "Obsidian", "Copper")  
# Replace above with your actual column names containing yes/no

# Convert yes/no to 1/0 for all those Type columns
nodes <- nodes %>%
  mutate(across(all_of(type_cols), ~ ifelse(tolower(.) == "yes", 1, 0)))

# Reshape Type columns to long format and filter only present (1)
nodes_long <- nodes %>%
  pivot_longer(cols = all_of(type_cols),
               names_to = "Type",
               values_to = "Present") %>%
  filter(Present == 1)

# Set Type factor with levels to preserve order
nodes_long$Type <- factor(nodes_long$Type, levels = type_cols)

# Combined label for legend
nodes_long <- nodes_long %>%
  mutate(site_display = paste(Site_Code, Site_Name, sep = " - "))

# Split data by Type for individual plots
node_splits <- split(nodes_long, nodes_long$Type)

# Function to create plot per Type with updated title
create_type_plot <- function(df, type){
  ggplot() +
    geom_raster(data = relief_df, aes(x = x, y = y, fill = value), alpha = 0.4) +
    scale_fill_gradient(low = "gray90", high = "gray50", guide = "none") +
    geom_sf(data = iran, fill = NA, color = "black", size = 0.5) +
    geom_point(data = df, aes(x = Long, y = Lat, color = site_display), size = 2,
               position = position_jitter(width = 0.05, height = 0.05)) +
    geom_text_repel(data = df, aes(x = Long, y = Lat, label = Site_Code),
                    size = 3, box.padding = 0.5, point.padding = 0.5,
                    segment.size = 0.5, segment.color = "grey50", min.segment.length = 0,
                    max.overlaps = Inf, nudge_y = 0.05) +
    coord_sf(xlim = c(51.3, 53.7), ylim = c(29, 31), expand = FALSE) +
    labs(title = paste("Distribution of", type),
         color = "Site Code - Site Name") +
    theme_minimal() +
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank())
}

# Generate plots list
plot_list <- lapply(names(node_splits), function(tp){
  create_type_plot(node_splits[[tp]], tp)
})

# Print or save plots
for(p in plot_list){
  print(p)
}
