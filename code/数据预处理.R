# 1. 加载库
library(dplyr)
library(ggplot2)
library(tidyr)       
library(patchwork)   
library(lubridate)   
library(ggcorrplot)  
library(stringi)     

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

# 统一绘图主题
my_theme <- theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(color = "gray50", size = 12))
my_fill <- "#4E84C4"

cat("=" %+% paste(rep("=", 70), collapse = "") %+% "\n")
cat("数据预处理 (特征工程增强版)\n")
cat("=" %+% paste(rep("=", 70), collapse = "") %+% "\n\n")

# 1. 读取与初步清洗

cat("【步骤1】读取数据并清洗...\n")
data <- read.csv("../data/AirQuality.csv", sep = ";", dec = ",", encoding = "UTF-8")

# 删除 Unnamed 列
data <- data[, !grepl("^Unnamed", names(data))]

# 将 -200 替换为 NA 
data[data == -200] <- NA

cat("原始数据形状:", nrow(data), "行", ncol(data), "列\n")

# 2. 特征工程 

cat("\n【步骤2】执行特征工程\n")

# 确保不包含全NA的行（Date列如果是NA则无法提取时间）
data <- data %>% filter(!is.na(Date) & !is.na(Time))

# 处理时间格式
data_eng <- data %>%
  mutate(
    # 将Time列转为字符处理，提取前两位作为小时
    Hour = as.numeric(substring(as.character(Time), 1, 2)),
    
    # 处理日期，提取月份
    Date_parsed = as.Date(Date, format = "%d/%m/%Y"),
    Month = as.numeric(format(Date_parsed, "%m")),
    
    # 提取星期几 (1=周日, 2=周一...)，可选
    Weekday = as.numeric(format(Date_parsed, "%w"))
  )

cat("新增特征: Hour, Month, Weekday\n")

# 3. 删除缺失值过多的列

cat("\n【步骤3】删除高缺失列...\n")
missing_pct <- colMeans(is.na(data_eng))
cols_to_remove <- names(missing_pct)[missing_pct > 0.9] # 阈值90%

if (length(cols_to_remove) > 0) {
  data_eng <- data_eng %>% select(-all_of(cols_to_remove))
  cat("已删除列:", paste(cols_to_remove, collapse = ", "), "\n")
}

# 4. 选择最终变量 & 删除缺失行

cat("\n【步骤4】选择关键变量并处理缺失行...\n")

features <- c("PT08.S1.CO.", "PT08.S3.NOx.", "T", "RH", "Hour", "Month")
target <- "CO.GT."

selected_cols <- c(features, target)
cat("最终选择特征:", paste(features, collapse = ", "), "\n")

# 筛选列
data_selected <- data_eng[, selected_cols]

# 记录删除前行数
n_before <- nrow(data_selected)

# 删除包含NA的行 (Casewise deletion)
data_clean <- na.omit(data_selected)
n_after <- nrow(data_clean)

cat("删除缺失行前:", n_before, "-> 删除后:", n_after, "\n")
cat("数据保留率:", round(n_after / n_before * 100, 2), "%\n")

# 5. 结果可视化 

cat("\n【步骤5】生成高质量可视化报告...\n")

# 5.1 数据质量漏斗图 
quality_df <- data.frame(
  Step = factor(c("原始数据", "清洗后数据"), levels = c("原始数据", "清洗后数据")),
  Count = c(n_before, n_after)
)

p1 <- ggplot(quality_df, aes(x = Step, y = Count, fill = Step)) +
  geom_col(width = 0.5, show.legend = FALSE) +
  geom_text(aes(label = paste0(Count, "\n(", round(Count/n_before*100, 1), "%)")), 
            vjust = -0.5, size = 5) +
  scale_fill_manual(values = c("lightcoral", "lightgreen")) +
  ylim(0, n_before * 1.2) +
  labs(title = "数据清洗前后样本量对比", x = "", y = "样本数") +
  my_theme

ggsave("7_数据质量提升.png", p1, width = 6, height = 6)

# 5.2 预处理后变量分布 (分面直方图)
long_data <- data_clean %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")

p2 <- ggplot(long_data, aes(x = Value)) +
  geom_histogram(bins = 30, fill = my_fill, color = "white", alpha = 0.8) +
  facet_wrap(~Variable, scales = "free") +
  labs(title = "预处理后各变量分布", x = "数值", y = "频数") +
  my_theme

ggsave("8_预处理后变量分布.png", p2, width = 12, height = 8)

# 5.3 最终相关性矩阵
p3 <- ggcorrplot(cor(data_clean), 
           method = "square", 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           colors = c("#6D9EC1", "white", "#E46726"),
           title = "最终变量相关性矩阵",
           ggtheme = theme_minimal)

ggsave("9_相关性矩阵.png", p3, width = 8, height = 8)

cat("✓ 图表已保存: 7_数据质量提升.png, 8_预处理后变量分布.png, 9_相关性矩阵.png\n")

# 6. 保存数据

cat("\n【步骤6】保存最终数据...\n")

# 1. 保存为 RData (保留数据类型信息)
save(data_clean, file = "../data/data_preprocessed.RData")

# 2. 保存为 CSV 
write.csv(data_clean, "../data/data_preprocessed.csv", row.names = FALSE)

cat("✓ 最终数据已保存至: data_preprocessed.csv\n")