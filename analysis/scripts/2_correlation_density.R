#Alternating carnivore and Neanderthal activities at Escoural Cave:
#Insights from the taphonomic and machine learning analysis of leporid remains

# Cobo-Sánchez, L.; Rufà, A.; Cascalheira, J.


# 2. Correlation of skeletal part abundances with bone density values

# Load data and library
library(ggplot2)
data <- read.csv("analysis/data/raw_data/2_density.csv",header=T,sep=";")
head(data)

# Calculate Spearman correlation
correlation <- cor(data$RA, data$density, method = "spearman")

# Calculate the p-value for the Spearman correlation
test_result <- cor.test(data$RA, data$density, method = "spearman")

# Print the correlation and p-value
cat("Spearman correlation: ", correlation, "\n")
cat("p-value: ", test_result$p.value, "\n")

# Generate scatterplot
plot<-ggplot(data, aes(x = RA, y = density)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Scatterplot of RA% vs. Density Values",
       subtitle = paste("Spearman correlation: ", round(correlation, 2)),
       x = "RA%",
       y = "Density Value (VDSA)") +
  theme_bw() +  # Use theme_bw for a white background
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

# Save the plot with 300 dpi resolution
#ggsave("Figure3.png", plot = plot, width = 10, height = 8, dpi = 300)
