library(vegan)
library(stats)
library(ggplot2)
library(ggdendro)

# Assuming binary_matrix is your binary site-feature matrix

# Calculate Jaccard distance matrix
jaccard_dist <- vegdist(binary_matrix, method = "jaccard")

# Hierarchical clustering with different methods
hc_complete <- hclust(jaccard_dist, method = "complete")
hc_average <- hclust(jaccard_dist, method = "average")
hc_ward <- hclust(jaccard_dist, method = "ward.D2")

# Calculate cophenetic correlations
cc_complete <- cor(jaccard_dist, cophenetic(hc_complete))
cc_average <- cor(jaccard_dist, cophenetic(hc_average))
cc_ward <- cor(jaccard_dist, cophenetic(hc_ward))

cat("Cophenetic correlation (Complete):", cc_complete, "\n")
cat("Cophenetic correlation (Average):", cc_average, "\n")
cat("Cophenetic correlation (Ward):", cc_ward, "\n")

# Optional: Plot dendrograms side by side for visual inspection
dend_data_complete <- ggdendrogram(hc_complete, rotate = FALSE)
dend_data_average <- ggdendrogram(hc_average, rotate = FALSE)
dend_data_ward <- ggdendrogram(hc_ward, rotate = FALSE)

# Plot function for ggplot2 dendrogram
plot_dendrogram <- function(hc, title) {
  ggdend <- ggdendrogram(hc)
  ggdend + ggtitle(title) + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
}

# Print dendrograms (adjust plotting layout as needed)
plot_dendrogram(hc_complete, "Complete Linkage")
plot_dendrogram(hc_average, "Average Linkage")
plot_dendrogram(hc_ward, "Ward's Method")
