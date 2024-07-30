#Alternating carnivore and Neanderthal activities at Escoural Cave:
#Insights from the taphonomic and machine learning analysis of leporid remains

# Cobo-Sánchez, L.; Rufà, A.; Cascalheira, J.


# 1. Anatomical representation: Relative abundance (RA%) barplot

# Load the necessary libraries
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(readr)

# Read the CSV file into a data frame
data <- read.csv("analysis/data/raw_data/1_RA_MAU.csv",sep=";")

# Check the structure of the data
str(data)

# Melt the data for ggplot2
melted_data <- melt(data, id.vars = "Element")

# Preserve order of elements
data$Element <- factor(data$Element, levels = data$Element)

# Choose color
chosen_color <- brewer.pal(12, "Set3")[6]

# Create bar plot for %MAU
ggplot(data, aes(x = Element, y = MAU_percent)) +
  geom_bar(stat = "identity", fill = chosen_color) +
  labs(title = " ", x = "Element", y = "%MAU") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Create bar plot for RA%
plot<- ggplot(data, aes(x = Element, y = RA_percent)) +
  geom_bar(stat = "identity", fill = chosen_color) +
  labs(title = " ", x = "Element", y = "RA%") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )
# Save the plot with 300 dpi resolution
#ggsave("Figure2.png", plot = plot, width = 10, height = 6, dpi = 300)


