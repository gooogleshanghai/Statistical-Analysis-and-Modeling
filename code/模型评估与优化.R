
# 1. 加载库
library(ggplot2)
library(dplyr)
library(car)      
library(caret)    

# 设置工作目录为代码目录
# 如果不在RStudio中运行，请手动设置工作目录为 code/ 目录
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
} else {
  # 如果不是在RStudio中，尝试自动检测
  if (file.exists("../data")) {
    # 已经在code目录
  } else if (file.exists("data")) {
    # 在项目根目录，需要进入code目录
    if (dir.exists("code")) setwd("code")
  }
}
options(scipen = 999)

# 2. 加载预处理好的数据

load("../data/data_preprocessed.RData") 

cat("=" %+% paste(rep("=", 70), collapse = "") %+% "\n")
cat("模型评估与预测阶段\n")
cat("=" %+% paste(rep("=", 70), collapse = "") %+% "\n\n")

# 3. 数据集划分 (70% 训练, 30% 测试)

set.seed(123) 

# 随机索引
train_index <- sample(1:nrow(data_clean), 0.7 * nrow(data_clean))

# 划分数据
train_data <- data_clean[train_index, ]
test_data  <- data_clean[-train_index, ]

cat("训练集样本数:", nrow(train_data), "\n")
cat("测试集样本数:", nrow(test_data), "\n\n")


# 4. 重新拟合最佳模型 
# 用 train_data 来训练

fit_final <- lm(CO.GT. ~ PT08.S1.CO. + log(PT08.S3.NOx.) + T + RH + 
                factor(Hour) + factor(Month), 
                data = train_data)

cat("--- 最终模型摘要 (训练集) ---\n")
print(summary(fit_final)$coefficients[1:5, ]) # 只打印前5行系数示例
cat("...\n\n")


# 5. 模型诊断 

# 5.1 检查多重共线性 (VIF)

cat("--- 多重共线性检查 (VIF) ---\n")
vif_values <- vif(fit_final)
print(vif_values)

# 5.2 残差诊断图
png("10_模型残差诊断图.png", width = 1200, height = 1200, res = 150)
par(mfrow = c(2, 2)) # 2x2 画布
plot(fit_final)
dev.off()
cat("✓ 残差诊断图已保存: 10_模型残差诊断图.png\n")


# 6. 预测与评估 (在测试集上)
# 对测试集进行预测
predictions <- predict(fit_final, newdata = test_data)

# 计算评估指标
# 1. RMSE
rmse_val <- sqrt(mean((test_data$CO.GT. - predictions)^2))

# 2. MAE
mae_val <- mean(abs(test_data$CO.GT. - predictions))

# 3. R-squared 

ssr <- sum((test_data$CO.GT. - predictions)^2)
sst <- sum((test_data$CO.GT. - mean(test_data$CO.GT.))^2)
r2_val <- 1 - (ssr / sst)

cat("--- 模型性能评估 (测试集) ---\n")
cat(sprintf("RMSE (均方根误差) : %.4f\n", rmse_val))
cat(sprintf("MAE  (平均绝对误差): %.4f\n", mae_val))
cat(sprintf("R^2  (测试集R方)   : %.4f\n", r2_val))


# 7. 可视化预测结果

# 创建绘图数据框
plot_data <- data.frame(
  Actual = test_data$CO.GT.,
  Predicted = predictions
)

# 绘制散点图
p_pred <- ggplot(plot_data, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.3, color = "#4E84C4") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", size = 1) + # 完美预测线(y=x)
  labs(title = "模型预测能力评估: 真实值 vs 预测值",
       subtitle = paste0("测试集 R-squared = ", round(r2_val, 3), 
                         " | RMSE = ", round(rmse_val, 3)),
       x = "真实 CO 浓度 (mg/m³)",
       y = "预测 CO 浓度 (mg/m³)") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

ggsave("11_预测结果对比图.png", p_pred, width = 8, height = 8, dpi = 300)
cat("✓ 预测结果图已保存: 11_预测结果对比图.png\n")

# 8. 保存评估结果
evaluation_res <- data.frame(
  Metric = c("RMSE", "MAE", "R-squared"),
  Value = c(rmse_val, mae_val, r2_val)
)
write.csv(evaluation_res, "../data/模型评估指标.csv", row.names = FALSE)
cat("✓ 评估指标已保存: 模型评估指标.csv\n")

cat("\n所有分析结束！\n")