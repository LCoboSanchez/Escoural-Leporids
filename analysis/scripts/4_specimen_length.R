#Alternating carnivore and Neanderthal activities at Escoural Cave:
#Insights from the taphonomic and machine learning analysis of leporid remains

# Cobo-Sánchez, L.; Rufà, A.; Cascalheira, J.

# 4. Breakage: Barplot for specimen length
# Load necessary library and load data
library(ggplot2)
data <- read.csv("analysis/data/raw_data/4_lengths.csv")

# Create intervals for specimen lengths with whole numbers and 10 intervals
# Using pretty to get nice round numbers for breaks
breaks <- pretty(data$SPECIMENSIZE, n = 10)

data$interval <- cut(data$SPECIMENSIZE, breaks = breaks, include.lowest = TRUE)

# Count the number of specimens in each interval
interval_counts <- as.data.frame(table(data$interval))
colnames(interval_counts) <- c("Interval", "Count")

# Create the barplot Distribution of leporid specimen lengths.
plot<-ggplot(interval_counts, aes(x = Interval, y = Count)) +
  geom_bar(stat = "identity",fill="skyblue") +
  theme_minimal()+
  labs(title = "",
       x = "Specimen Length Intervals (mm)",
       y = "Count of Specimens")+
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

# Save the plot with 300 dpi resolution
#ggsave("Figure5.png", plot = plot, width = 10, height = 8, dpi = 300)
