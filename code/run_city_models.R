# 朴素贝叶斯与逻辑回归：训练、评估与可视化

# 加载依赖
user_lib <- Sys.getenv("R_LIBS_USER")
if (user_lib == "") {
  user_lib <- file.path(Sys.getenv("USERPROFILE"), "R", "win-library",
                        paste0(R.version$major, ".", strsplit(R.version$minor, "\\.")[[1]][1]))
}
if (!dir.exists(user_lib)) dir.create(user_lib, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(user_lib, .libPaths()))
if (!requireNamespace("e1071", quietly = TRUE)) {
  install.packages("e1071", repos = "https://cloud.r-project.org", quiet = TRUE, lib = user_lib)
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2", repos = "https://cloud.r-project.org", quiet = TRUE, lib = user_lib)
}
if (!requireNamespace("pROC", quietly = TRUE)) {
  install.packages("pROC", repos = "https://cloud.r-project.org", quiet = TRUE, lib = user_lib)
}

library(ggplot2)
dir.create("figures", showWarnings = FALSE)

font_family <- if (.Platform$OS.type == "windows") "Microsoft YaHei" else ""
base_theme <- theme_minimal() + theme(text = element_text(family = font_family))

# 读取数据
script_dir <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) "")
if (script_dir == "" || is.na(script_dir)) {
  script_dir <- getwd()
}
city_data <- read.csv("../data/City_Types.csv", stringsAsFactors = FALSE)
city_data$Type <- factor(city_data$Type)

# 空气指标特征
feature_cols <- c("CO", "NO2", "SO2", "O3", "PM2.5", "PM10")

# 缺失值中位数填充（如有）
for (col in feature_cols) {
  med <- median(city_data[[col]], na.rm = TRUE)
  city_data[[col]][is.na(city_data[[col]])] <- med
}

# 分层切分 70/30
set.seed(123)
train_rows <- integer(0); test_rows <- integer(0)
for (cls in levels(city_data$Type)) {
  idx <- which(city_data$Type == cls)
  idx <- sample(idx)
  cut <- floor(0.7 * length(idx))
  train_rows <- c(train_rows, idx[seq_len(cut)])
  test_rows  <- c(test_rows,  idx[(cut + 1):length(idx)])
}
train_data <- city_data[train_rows, c(feature_cols, "Type")]
test_data  <- city_data[test_rows,  c(feature_cols, "Type")]
train_data$Type <- droplevels(train_data$Type)
test_data$Type  <- factor(test_data$Type, levels = levels(train_data$Type))

# 朴素贝叶斯模型
nb_model <- e1071::naiveBayes(Type ~ ., data = train_data)
nb_pred  <- predict(nb_model, test_data)

# 混淆矩阵与指标（默认正类：Industrial）
pos_class <- if ("Industrial" %in% levels(train_data$Type)) "Industrial" else levels(train_data$Type)[1]
conf_mat <- table(Predicted = nb_pred, Actual = test_data$Type)
accuracy  <- sum(diag(conf_mat)) / sum(conf_mat)
precision <- conf_mat[pos_class, pos_class] / sum(conf_mat[pos_class, ])
recall    <- conf_mat[pos_class, pos_class] / sum(conf_mat[, pos_class])
f1_score  <- 2 * precision * recall / (precision + recall)

cat("样本量：", nrow(city_data), "\n")
cat("训练/测试：", nrow(train_data), "/", nrow(test_data), "\n")
cat("Type 分布（训练）：\n"); print(table(train_data$Type))
cat("Type 分布（测试）：\n"); print(table(test_data$Type))
cat("朴素贝叶斯混淆矩阵：\n"); print(conf_mat)
cat(sprintf("Accuracy: %.4f\n", accuracy))
cat(sprintf("Precision(%s): %.4f\n", pos_class, precision))
cat(sprintf("Recall(%s): %.4f\n", pos_class, recall))
cat(sprintf("F1 Score(%s): %.4f\n", pos_class, f1_score))

# 逻辑回归模型（仅支持二分类）
if (length(levels(train_data$Type)) == 2) {
  neg_class <- setdiff(levels(train_data$Type), pos_class)[1]
  train_data$Type <- relevel(train_data$Type, ref = neg_class)
  test_data$Type  <- factor(test_data$Type, levels = levels(train_data$Type))

  logit <- glm(Type ~ ., data = train_data, family = binomial())
  logit_prob <- predict(logit, newdata = test_data, type = "response")

  pred_df <- data.frame(
    Actual = test_data$Type,
    Prob = logit_prob
  )

  roc_obj <- pROC::roc(response = test_data$Type,
                       predictor = logit_prob,
                       levels = c(neg_class, pos_class),
                       direction = "<")
  auc_val <- as.numeric(pROC::auc(roc_obj))

  # 预测概率密度分布
  p_prob <- ggplot(pred_df, aes(x = Prob, fill = Actual)) +
    geom_density(alpha = 0.4) +
    labs(title = "逻辑回归预测概率分布",
         x = paste0("预测为 ", pos_class, " 的概率"),
         y = "密度") +
    base_theme
  ggsave("figures/logit_pred_probability.png", p_prob, width = 6, height = 4, dpi = 300)

  # 校准曲线（分箱）
  pred_df$bin <- cut(pred_df$Prob, breaks = seq(0, 1, 0.1), include.lowest = TRUE)
  cal_prob <- aggregate(Prob ~ bin, data = pred_df, mean)
  cal_rate <- aggregate(I(Actual == pos_class) ~ bin, data = pred_df, mean)
  cal_df <- data.frame(
    Prob = cal_prob$Prob,
    Rate = cal_rate[, 2]
  )

  p_cal <- ggplot(cal_df, aes(x = Prob, y = Rate)) +
    geom_point(size = 2) +
    geom_line(linewidth = 0.8) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#666666") +
    labs(title = "逻辑回归预测校准曲线",
         x = paste0("分箱平均预测概率（", pos_class, "）"),
         y = "实际为正类的比例") +
    base_theme
  ggsave("figures/logit_calibration.png", p_cal, width = 6, height = 4, dpi = 300)

  roc_df <- data.frame(
    FPR = 1 - roc_obj$specificities,
    TPR = roc_obj$sensitivities
  )
  p_roc <- ggplot(roc_df, aes(x = FPR, y = TPR)) +
    geom_line(color = "#2E86C1", linewidth = 1) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#666666") +
    labs(title = "Logistic ROC Curve",
         subtitle = sprintf("AUC = %.4f", auc_val),
         x = "False Positive Rate",
         y = "True Positive Rate") +
    base_theme
  ggsave("figures/logit_roc.png", p_roc, width = 6, height = 4, dpi = 300)
  cat(sprintf("Logistic AUC: %.4f\n", auc_val))
} else {
  warning("Type 为多分类，逻辑回归预测可视化已跳过。")
}
