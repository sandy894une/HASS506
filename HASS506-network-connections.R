# title: HASS506-network-connections.R
# description: 
# author: 'Sandy Pullen'
# date: '2025-09-10'

library(tidygraph)
library(ggraph)
library(dplyr)
library(readr)
library(ggrepel)

# Load full nodes.csv
nodes_full <- read_csv("nodes.csv")

# Select only the first 4 columns for network nodes

nodes <- nodes_full %>% select(1:4)

# Example edges dataframe for testing
edges <- data.frame(
  Source = c("TRH", "TRH", "TJB", "TJB", "TBS", "TNB", "TNB", "KHZ", "TJA", "TGR", "TGR"),
  Target = c("TMS", "TMS", "TMS", "TBS", "HMG", "TKA", "TKA", "TGR", "TJB", "TRH", "TJB"),
  Ware_Type = c("1", "2a", "1", "3", "3", "2b", "3", "1", "3", "3", "1"),
  stringsAsFactors = FALSE
) %>% rename(from = Source, to = Target)

# Replace the example dataframe with a CSV import
# edges <- read_csv("edges.csv") %>% 
#  rename(from = Source, to = Target)


# Create tidygraph graph
g_tbl <- tbl_graph(nodes = nodes, edges = edges, node_key = "Site_Code", directed = FALSE)

# Activate nodes for plotting
g_nodes <- activate(g_tbl, nodes)

# plot network with nodes sized by degree and edges colored by Ware_Type
ggraph(g_nodes, layout = "fr") +
  geom_edge_fan(aes(color = Ware_Type), width = 1, alpha = 0.8) +   
  geom_node_point(aes(size = centrality_degree()), color = "steelblue") +
  geom_node_text(aes(label = Site_Code), repel = TRUE, size = 3) +
  scale_edge_colour_manual(
    values = c("1" = "#1b9e77", "2a" = "#d95f02", "2b" = "#7570b3", "3" = "#e7298a"),
    name = "Edge Ware Type",
    aesthetics = "edge_colour",
    guide = guide_legend()
  ) +
  labs(
    title = "Archaeological Site Network",
    subtitle = "Nodes sized by degree, edges colored by Ware Type"
  ) +
  theme_minimal()
