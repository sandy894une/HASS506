# title: HASS506-ware-cluster-analysis.R
# description: 
# author: 'Sandy Pullen'
# date: '2025-09-10'

library(readr)
library(dplyr)
library(vegan)        # for Jaccard distance
library(cluster)      # for hclust
library(ggplot2)
library(ggrepel)
library(raster)
library(sf)
library(rnaturalearth)
library(igraph)

# Load and prepare site data 
nodes <- read_csv("ca-test.csv")

# Identify motif (ware) columns
motif_cols <- grep("^Ware", names(nodes), value = TRUE)

# Convert motif data to numeric binary (assuming already 1/0)
#binary_matrix <- nodes %>%
#  select(all_of(motif_cols)) %>%
#  mutate(across(everything(), as.numeric))

binary_matrix <- dplyr::select(nodes, all_of(motif_cols)) %>%
  dplyr::mutate(across(everything(), as.numeric))

# Assign rownames as Site_Code for clustering
rownames(binary_matrix) <- nodes$Site_Code

# Calculate Jaccard distance for binary data
jaccard_dist <- vegdist(binary_matrix, method = "jaccard")

# Hierarchical clustering using Ward's method
hc <- hclust(jaccard_dist, method = "average")

# Plot dendrogram with site codes as labels
plot(hc, labels = nodes$Site_Code, main = "Hierarchical Clustering of Sites by Motif Presence",
     xlab = "Sites", sub = "", cex = 0.8)

  # Number of clusters to cut tree into
  k <- 4
  # Add rectangles highlighting clusters
  rect.hclust(hc, k = k, border = "grey80")

# Cut tree into k clusters
cluster_assignments <- cutree(hc, k = k)

  # Define cluster levels and colors (adjust colors as needed)
  cluster_levels <- c("1", "2", "3", "4", "5", "6")


cluster_colors <- c(
  "2" = "#E41A1C",
  "5" = "#377EB8",
  "3" = "#4DAF4A",
  "4" = "#984EA3",
  "1" = "#21409A",
  "6" = "#FFD33F"
)


# Cut dendrogram into k clusters and assign cluster memberships
cluster_groups <- cutree(hc, k = k)

# Print cluster assignment per site
print(cluster_groups)

# Create cluster dataframe and merge with nodes
cluster_df <- data.frame(Site_Code = names(cluster_assignments),
                         Cluster = factor(cluster_assignments))

nodes_clustered <- nodes %>%
  left_join(cluster_df, by = "Site_Code")


#-----------------------------------------------------------------


#Network diagram

# Convert distance to similarity
jaccard_sim <- 1 - as.matrix(jaccard_dist)

# Build edge list with threshold (e.g. similarity > 0.5)
edges <- data.frame(
  from = rep(rownames(jaccard_sim), times = nrow(jaccard_sim)),
  to = rep(colnames(jaccard_sim), each = nrow(jaccard_sim)),
  similarity = as.vector(jaccard_sim)
) %>%
  filter(from != to & similarity > 0.5)

# Create network graph
g <- graph_from_data_frame(edges, directed = FALSE)

# Plot
plot(g, vertex.label = V(g)$name, edge.width = E(g)$similarity*5)

#---------------------------


library(ggplot2)
library(ggrepel)
library(patchwork)
library(raster)
library(rnaturalearth)

# Load relief raster and Iran polygon 
relief <- raster("NE2_HR_LC_SR_W.tif")
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
iran <- world %>% filter(name == "Iran")
relief_cropped <- crop(relief, extent(iran) + 1)
relief_df <- as.data.frame(rasterToPoints(relief_cropped))
colnames(relief_df) <- c("x", "y", "value")




# Ensure cluster factor levels match
nodes_clustered$Cluster <- factor(nodes_clustered$Cluster, levels = cluster_levels)

# Prepare edges with cluster info on 'from' and 'to' nodes

#edges_plot <- edges %>%
#  left_join(nodes_clustered %>% select(Site_Code, Long, Lat, Cluster), by = c("from" = "Site_Code")) %>%
#  rename(Long_from = Long, Lat_from = Lat, Cluster_from = Cluster) %>%
#  left_join(nodes_clustered %>% select(Site_Code, Long, Lat, Cluster), by = c("to" = "Site_Code")) %>%
#  rename(Long_to = Long, Lat_to = Lat, Cluster_to = Cluster)

#explicitly call dplyr to avoid masking conflicts in code above
edges_plot <- edges %>%
  dplyr::left_join(
    dplyr::select(nodes_clustered, Site_Code, Long, Lat, Cluster),
    by = c("from" = "Site_Code")
  ) %>%
  dplyr::rename(Long_from = Long, Lat_from = Lat, Cluster_from = Cluster) %>%
  dplyr::left_join(
    dplyr::select(nodes_clustered, Site_Code, Long, Lat, Cluster),
    by = c("to" = "Site_Code")
  ) %>%
  dplyr::rename(Long_to = Long, Lat_to = Lat, Cluster_to = Cluster)


edges_plot$Cluster_from <- factor(edges_plot$Cluster_from, levels = cluster_levels)
edges_plot$Cluster_to <- factor(edges_plot$Cluster_to, levels = cluster_levels)

# Create edge type to separate intra- and inter-cluster edges
edges_plot <- edges_plot %>%
  mutate(edge_type = ifelse(Cluster_from == Cluster_to, "intra", "inter"))

# Plot 1: Geographic sites colored by cluster on relief map
p1 <- ggplot() + 
  geom_raster(data = relief_df, aes(x = x, y = y, fill = value), alpha = 0.4) +
  scale_fill_gradient(low = "gray90", high = "gray50", guide = "none") +
  geom_sf(data = iran, fill = NA, color = "black", size = 0.5) +
  geom_point(
    data = nodes_clustered,
    aes(x = Long, y = Lat, color = Cluster),
    size = 3,
    position = position_jitter(width = 0.01, height = 0.01)
  ) +
  geom_text_repel(
    data = nodes_clustered,
    aes(x = Long, y = Lat, label = Site_Code),
    size = 3,
    nudge_y = 0.05,
    box.padding = 0.5,
    point.padding = 0.5,
    segment.size = 0.5,
    segment.color = "grey50",
    min.segment.length = 0,
    max.overlaps = Inf
  ) +
  coord_sf(xlim = c(51.3, 53.7), ylim = c(29, 31), expand = FALSE) +
  scale_color_manual(values = cluster_colors, drop = FALSE) +
  labs(color = "Cluster") +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

# Plot 2: Network edges and nodes with intra/inter cluster edge distinction
p2 <- ggplot() +
  geom_segment(
    data = edges_plot %>% filter(edge_type == "inter"),
    aes(x = Long_from, y = Lat_from, xend = Long_to, yend = Lat_to),
    color = "grey70",
    alpha = 0.5
  ) +
  geom_segment(
    data = edges_plot %>% filter(edge_type == "intra"),
    aes(x = Long_from, y = Lat_from, xend = Long_to, yend = Lat_to, color = Cluster_from),
    alpha = 0.7
  ) +
  geom_point(
    data = nodes_clustered,
    aes(x = Long, y = Lat, color = Cluster),
    size = 4
  ) +
  geom_label_repel(
    data = nodes_clustered,
    aes(x = Long, y = Lat, label = Site_Code, color = Cluster),
    fill = "white",
    box.padding = 0.4,
    segment.color = 'grey50',
    size = 3
  ) +
  scale_color_manual(values = cluster_colors, drop = FALSE) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

# Combine the two plots side by side
combined_plot <- p1 + p2 + plot_layout(ncol = 2)

# Print combined plot
print(combined_plot)


#-----------------------------------------------------------------
 #Network stats
  
library(dplyr)
library(igraph)

# Group sites by Cluster and list site codes per cluster
sites_by_cluster <- nodes_clustered %>%
  group_by(Cluster) %>%
  summarize(Sites = paste(Site_Code, collapse = ", ")) %>%
  arrange(Cluster)

print(sites_by_cluster)

# Prepare edge list for igraph (add weight if available)
#edges_igraph <- edges %>%
#  select(from, to)  

#explicitly call dplyr to avoid masking conflicts in code above
  
  edges_igraph <- edges %>%
  dplyr::select(from, to)  
  
# Create igraph object, undirected network
g <- graph_from_data_frame(d = edges_igraph, vertices = nodes_clustered, directed = FALSE)

# Compute node-level metrics
degree_cent <- degree(g)
betweenness_cent <- betweenness(g, normalized = TRUE)
closeness_cent <- closeness(g)

# Compute network-level metrics
density_val <- edge_density(g)
avg_path_len <- average.path.length(g)
deg_dist <- degree_distribution(g)

# Optional: community detection (Louvain method)
comm <- cluster_louvain(g)
mod_score <- modularity(comm)
membership_vec <- membership(comm)

# Create summary dataframe with key stats per site (node)
node_stats <- data.frame(
  Site_Code = V(g)$name,
  Degree = degree_cent,
  Betweenness = betweenness_cent,
  Closeness = closeness_cent,
  Cluster_Louvain = as.vector(membership_vec)  # convert membership to vector
)

node_stats <- node_stats %>%
  arrange(desc(Degree), desc(Betweenness))

# Print overall network stats
cat("Network density:", density_val, "\n")
cat("Average path length:", avg_path_len, "\n")
cat("Modularity (Louvain):", mod_score, "\n")

# Show node-level statistics
print(node_stats)



#-----------------------------------------------------------------
# inter cluster analysis


# Identify inter-cluster edges
inter_cluster_edges <- edges_plot %>% filter(Cluster_from != Cluster_to)

# Summary 1: Count of inter-cluster edges by cluster pair
inter_edge_summary <- inter_cluster_edges %>%
  count(Cluster_from, Cluster_to, name = "EdgeCount") %>%
  arrange(desc(EdgeCount))

# Summary 2: Nodes acting as 'bridges' (sites with high inter-cluster degree)
bridge_from <- inter_cluster_edges %>% count(from, name = 'From_Bridge_Degree')
bridge_to <- inter_cluster_edges %>% count(to, name = 'To_Bridge_Degree')

bridge_summary <- full_join(bridge_from, bridge_to, by = c('from' = 'to')) %>%
  mutate(Bridge_Degree = coalesce(From_Bridge_Degree, 0) + coalesce(To_Bridge_Degree, 0)) %>%
  rename(Site_Code = from) %>%
  arrange(desc(Bridge_Degree))

# Optional: Export for Word/Excel
write.csv(inter_edge_summary, "inter_edge_summary.csv", row.names = FALSE)
write.csv(bridge_summary, "inter_cluster_bridge_nodes.csv", row.names = FALSE)

# Print results for interpretation
print(inter_edge_summary)
print(bridge_summary)

