
# 1. 加载数据 
load("../data/data_preprocessed.RData") 


# 2. 基础模型 

fit_full <- lm(CO.GT. ~ PT08.S1.CO. + PT08.S3.NOx. + T + RH + factor(Hour) + factor(Month), 
               data = data_clean)

cat("\n=== 基础模型统计摘要 ===\n")
summary(fit_full)


# 3. 优化模型：处理非线性关系
# 发现 PT08.S3.NOx. 和 CO 是反比/曲线关系，尝试添加对数项或平方项
fit_log <- lm(CO.GT. ~ PT08.S1.CO. + log(PT08.S3.NOx.) + T + RH + factor(Hour) + factor(Month), 
              data = data_clean)

cat("\n=== 优化后模型(对数变换) R方对比 ===\n")
cat("基础模型 R-squared:", summary(fit_full)$r.squared, "\n")
cat("优化模型 R-squared:", summary(fit_log)$r.squared, "\n")



# 4. 逐步回归 (Stepwise Regression) - 自动筛选变量
# 使用 AIC 准则自动剔除不显著的变量
fit_best <- step(fit_log, direction = "both", trace = 0)

cat("\n=== 最终最佳模型摘要 ===\n")
print(summary(fit_best))

# 5. 模型诊断 

par(mfrow = c(2, 2))
plot(fit_best) 

coef_table <- summary(fit_best)$coefficients
write.csv(coef_table, "../data/回归模型系数表.csv")
cat("\n✓ 回归分析完成，系数表已保存。\n")