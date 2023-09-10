data <- read.csv("D:/INTRODUCTION TO DATA SCIENCE/Final Term/Project/diabetes.csv")

str(data)

any(is.na(data$data))


normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
data <- as.data.frame(lapply(data, normalize))


install.packages("corrplot")
library(corrplot)

correlation_matrix <- cor(data, method = "pearson")
correlation_with_outcome <- correlation_matrix[, "outcome"]
print(correlation_with_outcome)



install.packages(caret)
library(caret)

if (!"Outcome" %in% colnames(data)) {
  stop("'Outcome' column not found in the dataset.")
}

trainIndex <- createDataPartition(data$Outcome, p = 0.7, list = FALSE)
training_data <- data[trainIndex, ]
testing_data <- data[-trainIndex, ]

k_values <- expand.grid(k = c(5))

knn_model <- train(
  Outcome ~ .,
  data = training_data,
  method = "knn",
  preProcess = c("center", "scale"),
  trControl = trainControl(method = "cv", number = 10),
  tuneGrid = k_values
)

print(knn_model)

predictions <- predict(knn_model, newdata = testing_data)

accuracy <- sum(predictions == testing_data$Outcome) / nrow(testing_data)
cat("Accuracy with optimal k:", accuracy, "\n")



install.packages("Metrics")
library(Metrics)

pred_values <- factor(data$Outcome)
actual_values <- factor(data$Outcome)

cf <- caret::confusionMatrix(data=pred_values,reference=actual_values)
print(cf)

predicted <- c(data$Outcome)
actual <- c(data$Outcome)

Metrics::precision(predicted, actual)
Metrics::recall(predicted, actual)


