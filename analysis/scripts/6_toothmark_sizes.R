#Alternating carnivore and Neanderthal activities at Escoural Cave:
#Insights from the taphonomic and machine learning analysis of leporid remains

# Cobo-Sánchez, L.; Rufà, A.; Cascalheira, J.

# Bone surface modifications. Mean and CI plot for pits and puncture breadth

library(DescTools)
library(ggplot2)
data<-read.csv("analysis/data/raw_data/6_pits_punct_Esc.csv", sep=";", header=TRUE)
data <- as.numeric(unlist(data))

# Calculate the mean and 95% confidence interval
result <- t.test(data, conf.level = 0.95)

# Extract the mean and confidence interval
mean_value <- mean(data)
ci_lower <- result$conf.int[1]
ci_upper <- result$conf.int[2]

# Print the results
mean_value
ci_lower
ci_upper

data_all<-read.csv("6_pits_punct_all.csv",sep=";",header=TRUE)

# Create the plot
plot<-ggplot(data_all, aes(x = Group, y = Mean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), width = 0.2) +
  theme_minimal() +
  labs(title = "",
       x = "Group",
       y = "Mean") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
  panel.background = element_rect(fill = "white", color = NA),
  plot.background = element_rect(fill = "white", color = NA)
)

# Save the plot with 300 dpi resolution
#ggsave("Figure9.png", plot = plot, width = 10, height = 8, dpi = 300)
