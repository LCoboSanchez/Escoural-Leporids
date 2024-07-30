#Alternating carnivore and Neanderthal activities at Escoural Cave:
#Insights from the taphonomic and machine learning analysis of leporid remains

# Cobo-Sánchez, L.; Rufà, A.; Cascalheira, J.


# Machine learning analysis of rabbit skeletal representation data

# Dataset includes relative abundance data (RA%) of 52 actualistic and archaeological rabbit assemblages
# produced by different predator agents, including terrestrial carnivores, diurnal and nocturnal raptors, and humans.

# Install and load necessary libraries
library(tidyverse)
library(caret)
library(randomForest)
library(e1071)
library(missForest)
library(ggplot2)
library(factoextra)
library(ranger)
library(corrplot)

# Load the data
data <- read.table("analysis/data/raw_data/3_anatom_rep.txt", header = TRUE, sep = "\t", na.strings = "na")

# Impute missing values
set.seed(123)
data_imputed <- missForest(data[,-c(1:3)])$ximp

# Convert imputed data to data frame
data_imputed <- as.data.frame(data_imputed)

# Normalize data
data_normalized <- scale(data_imputed)
data_normalized <- data.frame(data_normalized)

# PCA
pca <- prcomp(data_normalized[,-ncol(data_normalized)], center = TRUE, scale. = FALSE)
data_pca <- as.data.frame(pca$x)

# Add the 'Key2' column back to the PCA data
data_pca$Key2 <- data$Key2
data_pca$Sample <- data$Sample
data_normalized$Key2 <- as.factor(data$Key2)
data_normalized$Sample <- as.factor(data$Sample)

# Check levels of 'Key2'
levels(data_normalized$Key2)
levels(data_normalized$Sample)

# Plot results (with labels)
shape_values <- c(16, 17, 18, 19, 15)
pca_plotS1<-fviz_pca_ind(pca,
             geom.ind = "point",
             col.ind = data_pca$Key2,
             repel=TRUE,
             palette = "futurama",
             addEllipses = TRUE,
             legend.title = "Group",
             pointsize = 1.5) +  # Set a consistent size for all points
  scale_shape_manual(values = shape_values) +
  geom_text(data = data_pca, aes(x = PC1, y = PC2, label = Sample), size = 2.5, vjust = 2, hjust = 0.5)  # Adding labels

pca_plotS1 <- pca_plotS1 +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )
# Save the plot with 300 dpi resolution
ggsave("FigureS1.png", plot = pca_plotS1, width = 10, height = 8, dpi = 300)



# Plot results (without labels)
pca_plot<-fviz_pca_ind(pca,
             geom.ind = "point",
             col.ind = data_pca$Key2,
             repel=TRUE,
             palette = "futurama",
             addEllipses = TRUE,
             legend.title = "Group",
             pointsize = 1.5) +  # Set a consistent size for all points
  scale_shape_manual(values = shape_values)

pca_plot <- pca_plot +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )
# Save the plot with 300 dpi resolution
ggsave("Figure4.png", plot = pca_plot, width = 10, height = 8, dpi = 300)

# Machine learning models

# Remove Escoural row from dataset
data_normalized <- data_normalized[-53,]

# Check levels
data_normalized$Key2 <- factor(data_normalized$Key2, levels = unique(data_normalized$Key2))

levels(data_normalized$Key2)

# Remove column Sample
data_normalized <- data_normalized[,-15,]

# Feature selection
set.seed(123)
rf_model <- randomForest(Key2 ~ ., data = data_normalized, importance = TRUE)
importance_scores <- rf_model$importance
important_features <- sort(importance_scores[, "MeanDecreaseGini"], decreasing = TRUE)
selected_features <- names(important_features)[1:10]  # Select top 10 important features

# Subset data to include only selected features
data_selected <- data_normalized %>% select(all_of(selected_features), Key2)

# Detect highly correlated features with correlation matrix
correlation_matrix <- cor(data_selected[,-11], use = "complete.obs")
# Plot correlation matrix
corrplot(correlation_matrix, method = "circle")
# Correlation values
correlation_matrix
write.table(correlation_matrix,"correlation_matrix.csv",sep=";")

# High correlation >0.7 between MC and MT, and between MC and VR.
# Remove MC
data_selected_reduced <- data_selected[,-6]

# Split data into training and test sets using stratified sampling, 70% training set, 30% testing set
# Stratified sampling is similar to K-Fold but ensures that each fold has approximately the same percentage of samples of each target class as the entire dataset, which can help in maintaining the balance of class distributions.
set.seed(123)
trainIndex <- createDataPartition(data_selected_reduced$Key2, p = .7, list = FALSE, times = 1)
data_train <- data_selected_reduced[trainIndex,]
data_test <- data_selected_reduced[-trainIndex,]

# Check for empty classes in the training set
table(data_train$Key2)

# Setup trainControl with bootstrapping and internal cross-validation
control <- trainControl(
  method = "boot",
  number = 100,  # Number of bootstrap iterations
  savePredictions = "final",
  index = createResample(data_train$Key2, times = 100),
  verboseIter = TRUE
)

# 1. Random Forest

# Define a grid of hyperparameters to search over
tune_grid <- expand.grid(
  mtry = c(1:9),
  splitrule = "gini",
  min.node.size = c(1, 5, 10)
)

# Train the Random Forest model using ranger with hyperparameter tuning
set.seed(123)
rf_model_tuned <- train(
  Key2 ~ .,
  data = data_train,
  method = "ranger",
  trControl = control,
  tuneGrid = tune_grid
)

# Evaluate the model on the test set
test_predictions_rf <- predict(rf_model_tuned, newdata = data_test)
confusion_rf <- confusionMatrix(test_predictions_rf, data_test$Key2)

# Print the results
print(rf_model_tuned)
print(confusion_rf)


# Support Vector Machine model

# Define a grid of hyperparameters to search over for SVM
tune_grid_svm <- expand.grid(
  C = c(0.1, 1, 10, 100),
  sigma = c(0.01, 0.1, 1)
)

# Train the SVM model with hyperparameter tuning
set.seed(123)
svm_model_tuned <- train(
  Key2 ~ .,
  data = data_train,
  method = "svmRadial",
  probability=TRUE,
  trControl = control,
  tuneGrid = tune_grid_svm
)

# Evaluate the model on the test set
test_predictions_svm <- predict(svm_model_tuned, newdata = data_test)
confusion_svm <- confusionMatrix(test_predictions_svm, data_test$Key2)

# Print the results
print(svm_model_tuned)
print(confusion_svm)


# Gradient Boost Model

# Define a grid of hyperparameters to search over
tune_grid <- expand.grid(
  n.trees = c(100, 200, 300),
  interaction.depth = c(1, 2, 3),
  shrinkage = c(0.01, 0.1),
  n.minobsinnode = c(5, 10)
)

# Train the GBM model with hyperparameter tuning
set.seed(123)
gbm_model_reduced <- train(
  Key2 ~ .,
  data = data_train,
  method = "gbm",
  trControl = control,
  tuneGrid = tune_grid,
  verbose = FALSE
)

# Evaluate the model on the test set
test_predictions_gbm_reduced <- predict(gbm_model_reduced, newdata = data_test)
confusion_gbm_reduced <- confusionMatrix(test_predictions_gbm_reduced, data_test$Key2)

# Print the results
print(gbm_model_reduced)
print(confusion_gbm_reduced)


# Classify Escoural

data <- read.table("3_anatom_rep_classif.txt",header=T, sep="\t")

predictions1 <- predict(gbm_model_reduced, newdata = data,type="prob")
predictions2 <- predict(rf_model_tuned, newdata = data)
predictions3 <- predict(svm_model_tuned, newdata = data)

print(predictions1)
print(predictions2)
print(predictions3)


# Compare the models using resamples
resamples_list <- resamples(list(RF = rf_model_tuned, SVM = svm_model_tuned, GBM = gbm_model_reduced))
summary(resamples_list)

# Plot the results
bwplot(resamples_list)
dotplot(resamples_list)

