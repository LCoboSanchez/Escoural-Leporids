#Alternating carnivore and Neanderthal activities at Escoural Cave:
#Insights from the taphonomic and machine learning analysis of leporid remains

# Cobo-Sánchez, L.; Rufà, A.; Cascalheira, J.

# 7. Taphonomy HCPC and kmeans cluster analysis

# Load necessary libraries
library(FactoMineR)
library(factoextra)
library(missForest)
library(ggrepel)
library(cluster)
library(fpc)

# Load data
data <- read.csv("analysis/data/raw_data/7_HCPC_Kmeans_tapho.txt", sep = "\t", na.strings = "na")

# HCPC
# Impute missing values
set.seed(123)
data_imputed <- missForest(data[, -1])$ximp  # Exclude the first column if it's not numeric

# PCA
pca_result <- PCA(data_imputed, graph = TRUE)

# Perform HCPC
hcpc_result <- HCPC(pca_result, graph = FALSE)
hcpc_result <- HCPC(pca_result, graph = TRUE)

# Plot HCPC dendrogram
fviz_dend(hcpc_result, cex = 0.7, palette = "futurama", rect = TRUE, rect_fill = TRUE, rect_border = "jco",
          labels_track_height = 0.8)

# Create the factor map plot
factor_map <- fviz_cluster(hcpc_result,
                           geom = "point",
                           ellipse.type = "convex",
                           palette = "futurama",
                           ggtheme = theme_minimal(),
                           shape = 19
)

# Add sample names to the plot
factor_map <- factor_map +
  geom_text_repel(aes(label = data$Assemblage),
                  vjust = -1,
                  hjust = 0.5,
                  size = 2.5) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

# Save the plot with 300 dpi resolution
ggsave("Figure10.png", plot = factor_map, width = 10, height = 8, dpi = 300)


# Convert clustering labels to numeric
clustering_labels <- as.numeric(hcpc_result$data.clust$clust)

# Calculate silhouette scores
silhouette_score <- silhouette(clustering_labels, dist(data_imputed))

# Plot silhouette scores
fviz_silhouette(silhouette_score)


# K- means clustering analysis

# Perform k-means clustering with a chosen number of clusters
set.seed(123)
kmeans_result <- kmeans(data_imputed, centers = 6)

# Print cluster centers
print(kmeans_result$centers)

# Plot the clusters
fviz_cluster(kmeans_result, data = data_imputed)

factor_map <- fviz_cluster(kmeans_result, data=data_imputed, geom = "point", ellipse.type = "convex", palette = "futurama", ggtheme = theme_minimal(),shape=19)
factor_map <- factor_map + geom_text_repel(aes(label = data$Assemblage), vjust = -1, hjust = 0.5, size = 2.5)
factor_map <- factor_map + theme(
  axis.text.x = element_text(angle = 45, hjust = 1),
  panel.background = element_rect(fill = "white", color = NA),
  plot.background = element_rect(fill = "white", color = NA)
)

plot(factor_map)
# Save the plot with 300 dpi resolution
ggsave("Figure11.png", plot = factor_map, width = 10, height = 8, dpi = 300)

# Convert clustering labels to numeric
clustering_labels <- as.numeric(kmeans_result$data.clust$clust)

# Calculate silhouette scores
silhouette_scores <- silhouette(kmeans_result$cluster, dist(data_imputed))

# Plot silhouette scores
fviz_silhouette(silhouette_scores)

write.table(silhouette_scores,"silhouette_scores_kmeans.csv",sep=";")


