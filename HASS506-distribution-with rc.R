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

# Load archaeological nodes data
nodes <- read_csv("nodes.csv")

# Specify all Type columns 

type_cols <- c("Ware 2a", "Ware 2b", "Ware 3", "Clay Tokens", "Shell", "Obsidian", "Copper")  


# Convert yes/no in Type columns to 1/0
nodes <- nodes %>%
  mutate(across(all_of(type_cols), ~ ifelse(tolower(.) == "yes", 1, 0)))

# Reshape Type columns to long format and filter to present
nodes_long <- nodes %>%
  pivot_longer(cols = all_of(type_cols),
               names_to = "Type",
               values_to = "Present") %>%
  filter(Present == 1)

nodes_long$Type <- factor(nodes_long$Type, levels = type_cols)

nodes_long <- nodes_long %>%
  mutate(site_display = paste(Site_Code, Site_Name, sep = " - "))

# Load radiocarbon data
rc_data <- read_csv("Radiocarbon-dates.csv") %>% 
  filter(!is.na(Long), !is.na(Lat)) %>%
  mutate(RC_Age = as.numeric(`BP Date`))

# Split data by Type for plotting
node_splits <- split(nodes_long, nodes_long$Type)

# Plotting function with sites in single color and radiocarbon points colored by age
create_type_plot <- function(df, type, rc_data = NULL) {
  p <- ggplot() +
    geom_raster(data = relief_df, aes(x = x, y = y, fill = value), alpha = 0.4) +
    scale_fill_gradient(low = "gray90", high = "gray50", guide = "none") +
    geom_sf(data = iran, fill = NA, color = "black", size = 0.5) +
    geom_point(data = df, aes(x = Long, y = Lat), 
               color = "blue", size = 2, position = position_jitter(width = 0.05, height = 0.05)) +
    geom_text_repel(data = df, aes(x = Long, y = Lat, label = Site_Code),
                    size = 3, box.padding = 0.5, point.padding = 0.5,
                    segment.size = 0.5, segment.color = "grey50",
                    min.segment.length = 0, max.overlaps = Inf, nudge_y = 0.05)
  
  if (!is.null(rc_data)) {
    p <- p +
      geom_point(data = rc_data, aes(x = Long, y = Lat, color = RC_Age),
                 shape = 21, fill = "red", size = 3, alpha = 0.7) +
      scale_color_viridis_c(name = "Radiocarbon Age (BP)", option = "plasma", na.value = "grey50")
  }
  
  p +
    coord_sf(xlim = c(51.3, 53.7), ylim = c(29, 31), expand = FALSE) +
    labs(title = paste("Distribution of", type), color = "Radiocarbon Age (BP)") +
    theme_minimal() +
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank())
}

# Generate all plots with radiocarbon data overlay
plot_list <- lapply(names(node_splits), function(tp) {
  create_type_plot(node_splits[[tp]], tp, rc_data)
})

# Print plots
for (plot in plot_list) {
  print(plot)
}

# Optional: save plots to files
# for (i in seq_along(plot_list)) {
#   ggsave(filename = paste0("Distribution_of_", names(plot_list)[i], ".png"), plot = plot_list[[i]], width = 8, height = 6)
# }
