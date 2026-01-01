city_data <- read.csv("../data/City_Types.csv", stringsAsFactors = FALSE)
city_data$Type <- factor(city_data$Type)
feature_cols <- c("CO","NO2","SO2","O3","PM2.5","PM10")
for (col in feature_cols) {
  med <- median(city_data[[col]], na.rm = TRUE)
  city_data[[col]][is.na(city_data[[col]])] <- med
}
model <- glm(Type ~ ., data = city_data[, c(feature_cols, "Type")], family = binomial())
sm <- summary(model)
print(coef(sm))
