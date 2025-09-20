
library(raster)
library(sf)
library(ggplot2)
library(ggrepel)
library(readr)
library(dplyr)
library(rnaturalearth)


# Split the data frame by WareType (list of data frames)
node_splits <- split(nodes_long, nodes_long$WareType)

# Function to create plot for each WareType subset
plot_list <- lapply(names(node_splits), function(type){
  df_subset <- node_splits[[type]]
  
  ggplot() + 
    geom_raster(data = relief_df, aes(x = x, y = y, fill = value), alpha = 0.4) +
    scale_fill_gradient(low = "gray90", high = "gray50", guide = "none") +
    geom_sf(data = iran, fill = NA, color = "black", size = 0.5) +
    geom_point(data = df_subset, aes(x = Long, y = Lat, color = site_display), size = 2) +
    geom_text_repel(
      data = df_subset,
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
    #coord_sf(xlim = c(51.3, 53.7), ylim = c(29, 31), expand = FALSE) +
    labs(
      title = paste("Distribution of ", type),
      color = "Site Code - Site Name"
    ) +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    )
})

# Display plots separately
print(plot_list[[1]])  # first WareType
print(plot_list[[2]])  # second WareType
print(plot_list[[3]])  # third WareType
