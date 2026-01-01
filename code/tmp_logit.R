city_data <- read.csv("../data/City_Types.csv", stringsAsFactors = FALSE)
city_data$Type <- factor(city_data$Type)
feature_cols <- c("CO","NO2","SO2","O3","PM2.5","PM10")
for (col in feature_cols) {
  med <- median(city_data[[col]], na.rm = TRUE)
  city_data[[col]][is.na(city_data[[col]])] <- med
}
set.seed(123)
train_rows <- integer(0); test_rows <- integer(0)
for (cls in levels(city_data$Type)) {
  idx <- which(city_data$Type == cls); idx <- sample(idx)
  cut <- floor(0.7 * length(idx))
  train_rows <- c(train_rows, idx[seq_len(cut)])
  test_rows  <- c(test_rows,  idx[(cut+1):length(idx)])
}
train <- city_data[train_rows, c(feature_cols, "Type")]
test  <- city_data[test_rows,  c(feature_cols, "Type")]
train$Type <- droplevels(train$Type)
test$Type  <- factor(test$Type, levels = levels(train$Type))

logit <- glm(Type ~ ., data = train, family = binomial())
summary(logit)
prob <- predict(logit, newdata = test[, feature_cols], type = "response")
pos_level <- levels(train$Type)[2]  # Residential
pred <- ifelse(prob >= 0.5, pos_level, levels(train$Type)[1])
pred <- factor(pred, levels = levels(train$Type))
conf <- table(Predicted = pred, Actual = test$Type)
acc <- sum(diag(conf)) / sum(conf)
prec <- conf[pos_level, pos_level] / sum(conf[pos_level, ])
rec <- conf[pos_level, pos_level] / sum(conf[, pos_level])
f1 <- 2 * prec * rec / (prec + rec)
print(conf)
cat(sprintf("Accuracy: %.4f\nPrecision: %.4f\nRecall: %.4f\nF1: %.4f\n", acc, prec, rec, f1))
summary(logit)