library(rpart)
library(caret)
library(ROCR)
library(doParallel)

# Function to print debug statements
debug_print <- function(msg) {
  cat("[DEBUG]: ", msg, "\n")
}

# Function to calculate Gini impurity
calculate_gini <- function(labels) {
  probabilities <- table(labels) / length(labels)
  gini <- 1 - sum((1 - probabilities)^2) - sum(probabilities^2) - 2 *         sum(probabilities)
  # gini <- 1 - sum((complement_prob)^2)  -  sum((1 - complement_prob)^2) - 2 * sum(1 - complement_prob)
  return(gini)
}

# Function to split data based on a given feature and value
split_data <- function(data, feature, value) {
  left <- data[data[, feature] < value, ]
  right <- data[data[, feature] >= value, ]
  return(list(left = left, right = right))
}

# Function to calculate information gain
calculate_information_gain <- function(left_labels, right_labels, current_gini) {
  p <- length(left_labels) / (length(left_labels) + length(right_labels))
  information_gain <- current_gini - p * calculate_gini(left_labels) - (1 - p) * calculate_gini(right_labels)
  return(information_gain)
}

# Function to find the best split
find_best_split <- function(data) {
  features <- colnames(data)[-ncol(data)]
  best_gain <- -Inf
  best_feature <- NULL
  best_value <- NULL
  current_gini <- calculate_gini(data$Habitable)
  
  for (feature in features) {
    unique_values <- unique(data[[feature]])
    for (value in unique_values) {
      groups <- split_data(data, feature, value)
      left_labels <- groups$left$Habitable
      right_labels <- groups$right$Habitable
      gain <- calculate_information_gain(left_labels, right_labels, current_gini)
      if (gain > best_gain) {
        best_gain <- gain
        best_feature <- feature
        best_value <- value
      }
    }
  }
  
  return(list(feature = best_feature, value = best_value, gain = best_gain))
}

# Function to build decision tree
build_tree <- function(data, max_depth, min_size, depth) {
  if (depth >= max_depth || nrow(data) <= min_size) {
    return(mean(data$Habitable))
  }
  
  split <- find_best_split(data)
  if (is.null(split$feature)) {
    return(mean(data$Habitable))
  }
  
  left_data <- split_data(data, split$feature, split$value)$left
  right_data <- split_data(data, split$feature, split$value)$right
  
  left_branch <- build_tree(left_data, max_depth, min_size, depth + 1)
  right_branch <- build_tree(right_data, max_depth, min_size, depth + 1)
  
  return(list(feature = split$feature, value = split$value, 
              left_branch = left_branch, right_branch = right_branch))
}

# Function to make predictions with a decision tree
predict <- function(tree, row) {
  if (is.numeric(tree)) {
    return(tree)
  }
  
  if (row[[tree$feature]] < tree$value) {
    return(predict(tree$left_branch, row))
  } else {
    return(predict(tree$right_branch, row))
  }
}

# Read the dataset
data <- read.csv("C:/Users/hp/OneDrive/Desktop/Study Material/DS/new_data.csv")

# Convert "Habitable" column to numeric
data$Habitable <- as.numeric(factor(data$Habitable, levels = c("Yes", "No")))

# Remove rows with NA values
data <- na.omit(data)

# Define features and target variable
features <- c("Name", "Type", "Detection.Method", "Mass", "Radius", "Flux", "Tsurf", "Period", "Distance", "Age", "ESI")
target <- "Habitable"

# Split data into training and testing sets
#set.seed(123)  # for reproducibility
train_index <- sample(nrow(data), 0.75 * nrow(data))  # 75% for training
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Train the decision tree model
tree <- build_tree(train_data, max_depth = 5, min_size = 10, depth = 1)

# Make predictions for testing data
predictions <- apply(test_data[, features], 1, function(row) {
  pred <- predict(tree, as.list(row))
  #print(pred)
  if (is.na(pred) || is.null(pred)) {
    return("Unknown")
  }
  if (pred > 1) {
    return("Yes")
  } else {
    return("No")
  }
})

# Combine predictions with testing data
predictions_with_data <- cbind(test_data, Prediction = predictions)

# Print prediction results
print("Prediction results:")
print(predictions_with_data)

# Confusion Matrix
# Generate confusion matrix
confusion_matrix <- table(predictions_with_data$Habitable, predictions_with_data$Prediction)
print("Confusion Matrix:")
print(confusion_matrix)

# Metrics
TP <- confusion_matrix[2, 2]
FP <- confusion_matrix[1, 2]
TN <- confusion_matrix[1, 1]
FN <- confusion_matrix[2, 1]

accuracy <- (TP + TN) / sum(confusion_matrix)
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)
f1_score <- 2 * precision * recall / (precision + recall)

# Print evaluation metrics
print(paste("Accuracy:", accuracy))
print(paste("Precision:", precision))
print(paste("Recall:", recall))
print(paste("F1 Score:", f1_score))

# Plot the evaluation metrics
metrics_names <- c("Accuracy", "Precision", "Recall", "F1 Score")
metrics_values <- c(accuracy, precision, recall, f1_score)

barplot(metrics_values, names.arg = metrics_names, col = "skyblue", main = "Model Evaluation Metrics", ylab = "Value",
        ylim=c(0,1))

# Function to make predictions with a decision tree (returning probabilities)
predict_prob <- function(tree, row) {
  if (is.numeric(tree)) {
    return(tree)
  }
  
  if (row[[tree$feature]] < tree$value) {
    return(predict_prob(tree$left_branch, row))
  } else {
    return(predict_prob(tree$right_branch, row))
  }
}

# Make predictions for testing data (probabilities)
probabilities <- apply(test_data[, features], 1, function(row) {
  predict_prob(tree, as.list(row))
})

# Prepare predictions for ROC curve
pred <- prediction(probabilities, test_data$Habitable)

# Generate ROC curve
roc <- performance(pred, "tpr", "fpr")
plot(roc, main = "ROC Curve", col = "blue", lwd = 2)
abline(a = 0, b = 1, lty = 2, col = "red")

# Function to visualize the decision tree
visualize_tree <- function(tree, depth = 0) {
  if (is.numeric(tree)) {
    cat(paste(rep("  ", depth), collapse = ""), "Predicted class:", tree, "\n")
  } else {
    cat(paste(rep("  ", depth), collapse = ""), "Split on", tree$feature, "at value", tree$value, "\n")
    visualize_tree(tree$left_branch, depth + 1)
    visualize_tree(tree$right_branch, depth + 1)
  }
}

# Visualize the decision tree
cat("Decision Tree Visualization:\n")
visualize_tree(tree)

# Generate confusion matrix
confusion_matrix <- table(predictions_with_data$Habitable, predictions_with_data$Prediction)
print("Confusion Matrix:")
print(confusion_matrix)

# Plot confusion matrix as heatmap
library(graphics)
heatmap(confusion_matrix, col = terrain.colors(10), scale = "none", xlab = "Predicted Class", ylab = "True Class", main = "Confusion Matrix Heatmap")