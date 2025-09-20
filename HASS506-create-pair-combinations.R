library(dplyr)

sites <- c("TRH","TMS","TNB","TJA","TBS","HMG","TKA","KHZ")
edge_pairs <- combn(sites, 2)


edge_list <- as.data.frame(t(edge_pairs), col.names = c("Source", "Target"))


print(edge_list)