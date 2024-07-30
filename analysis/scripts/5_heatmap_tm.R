#Alternating carnivore and Neanderthal activities at Escoural Cave:
#Insights from the taphonomic and machine learning analysis of leporid remains

# Cobo-Sánchez, L.; Rufà, A.; Cascalheira, J.

# 5. Bone surface modifications. Heatmap for tooth marks

library(ggplot2)
library(reshape2)

# Load the data
tm <- read.csv("analysis/data/raw_data/5_tm.csv", sep=";", header=TRUE)

# Print the first few rows of the data to check
head(tm)

# Ensure the 'Element' column is treated as a factor with levels in the desired order
tm$Element <- factor(tm$Element, levels = tm$Element)

# Convert data to long format
data_long <- melt(tm, id.vars = "Element", variable.name = "Damage_Type", value.name = "Percentage")

# Print the first few rows of the long-format data to check
head(data_long)

# Create the heatmap
plot<-ggplot(data_long, aes(x = Element, y = Damage_Type, fill = Percentage)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "darkred") +
  theme_minimal()+
  labs(title = "", #Heatmap of Damage Types Across Skeletal Elements
       x = "Skeletal Element",
       y = "Type of Damage") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

# Save the plot with 300 dpi resolution
#ggsave("Figure7.png", plot = plot, width = 10, height = 8, dpi = 300)
