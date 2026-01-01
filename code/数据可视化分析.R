
# 1. 加载库
library(ggplot2)
library(dplyr)
library(tidyr)       
library(patchwork)   
library(ggcorrplot)
library(scales)  

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

# 2. 统一绘图风格设置 
my_theme <- theme_minimal(base_size = 14) + 
  theme(
    text = element_text(family = "sans"), 
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(color = "gray50", size = 12),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

# 统一配色方案
my_fill_color <- "#4E84C4" 
my_color_palette <- "viridis"

# 3. 数据读取与预处理
cat("正在读取并清洗数据...\n")
data <- read.csv("../data/AirQuality.csv", sep = ";", dec = ",", encoding = "UTF-8")

# 关键变量列表
key_vars <- c("PT08.S1.CO.", "PT08.S3.NOx.", "T", "RH", "CO.GT.")

# --- 预处理：统一将 -200 替换为 NA ---
data_clean <- data %>%
  mutate(across(everything(), ~ifelse(. == -200, NA, .)))

# 可视化 1: 缺失值概览 

missing_data <- data_clean %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "变量", values_to = "缺失数") %>%
  mutate(缺失比例 = 缺失数 / nrow(data_clean)) %>%
  filter(缺失数 > 0) %>%
  arrange(desc(缺失比例))

p1 <- ggplot(missing_data, aes(x = reorder(变量, 缺失比例), y = 缺失比例)) +
  geom_col(fill = "tomato", width = 0.7) +
  geom_text(aes(label = scales::percent(缺失比例, accuracy = 0.1)), 
            hjust = -0.1, size = 3.5) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1.1)) + 
  coord_flip() + 
  labs(title = "数据缺失值比例", x = "", y = "缺失比例") +
  my_theme

ggsave("1_缺失值分析.png", p1, width = 8, height = 6, dpi = 300)
cat("✓ 1_缺失值分析.png 已保存\n")

# 可视化 2: 目标变量 CO(GT) 分布 

# 过滤掉目标变量的NA
df_target <- data_clean %>% filter(!is.na(CO.GT.))

# 直方图 + 密度线
p2_hist <- ggplot(df_target, aes(x = CO.GT.)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40, fill = my_fill_color, alpha = 0.7, color = "white") +
  geom_density(color = "darkred", linewidth = 1) + 
  labs(title = "CO(GT) 浓度分布", subtitle = "直方图与核密度估计", x = NULL) + 
  my_theme

# 箱线图 (横向)
p2_box <- ggplot(df_target, aes(x = CO.GT.)) +
  geom_boxplot(fill = "lightblue", width = 0.5, outlier.colour = "red") +
  labs(x = "CO(GT) (mg/m³)") +
  my_theme +
  theme(axis.text.y = element_blank()) 

# 使用 patchwork 拼图
p2_combined <- p2_hist / p2_box + plot_layout(heights = c(3, 1))

ggsave("2_目标变量分布.png", p2_combined, width = 8, height = 8, dpi = 300)
cat("✓ 2_目标变量分布.png 已保存\n")

# 可视化 3: 关键数值变量分布 

# 数据变长，方便分面
long_data <- data_clean %>%
  select(all_of(key_vars)) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%
  filter(!is.na(Value))

p3 <- ggplot(long_data, aes(x = Value)) +
  geom_histogram(bins = 30, fill = my_fill_color, color = "white", alpha = 0.8) +
  facet_wrap(~Variable, scales = "free", ncol = 3) + # scales="free" 允许各图坐标轴不同
  labs(title = "关键变量分布概览", x = "数值", y = "频数") +
  my_theme

ggsave("3_变量分布图.png", p3, width = 12, height = 8, dpi = 300)
cat("✓ 3_变量分布图.png 已保存\n")

# 可视化 4: 相关性热力图 

# 准备相关性矩阵
numeric_data <- data_clean %>% select(all_of(key_vars)) %>% na.omit()
corr_matrix <- cor(numeric_data)

# 绘制
p4 <- ggcorrplot(corr_matrix, 
           method = "square", 
           type = "lower",        # 只显示下半部分
           lab = TRUE,            # 显示数字
           lab_size = 4, 
           colors = c("#6D9EC1", "white", "#E46726"), # 专业的冷暖配色
           title = "变量相关性热力图",
           ggtheme = theme_minimal) +
           theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))

ggsave("4_相关性热力图.png", p4, width = 8, height = 8, dpi = 300)
cat("✓ 4_相关性热力图.png 已保存\n")

# 可视化 5: 特征 vs 目标变量 散点图 

# 准备数据：除了CO.GT.外的变量作为Feature
scatter_data <- data_clean %>%
  select(all_of(key_vars)) %>%
  pivot_longer(cols = -CO.GT., names_to = "Feature", values_to = "Value") %>%
  filter(!is.na(CO.GT.), !is.na(Value))

p5 <- ggplot(scatter_data, aes(x = Value, y = CO.GT.)) +
  # 使用 alpha=0.1 防止点太密集看不清分布
  geom_point(alpha = 0.2, color = "steelblue", size = 1) + 
  # 添加线性拟合线
  geom_smooth(method = "lm", color = "darkred", se = FALSE) +
  facet_wrap(~Feature, scales = "free_x") +
  labs(title = "特征与目标变量(CO.GT)的关系", 
       subtitle = "散点图与线性拟合趋势",
       y = "CO(GT) (mg/m³)", x = "特征值") +
  my_theme

ggsave("5_特征关系图.png", p5, width = 12, height = 10, dpi = 300)
cat("✓ 5_特征关系图.png 已保存\n")

# 可视化 6: 异常值检测 (优化：分面箱线图)

p6 <- ggplot(long_data, aes(x = Variable, y = Value)) +
  geom_boxplot(fill = "lightblue", outlier.colour = "red", outlier.size = 1.5, alpha = 0.7) +
  facet_wrap(~Variable, scales = "free", ncol = 3) + 
  labs(title = "异常值检测 (箱线图)", x = "", y = "数值") +
  my_theme +
  theme(axis.text.x = element_blank()) 

ggsave("6_异常值检测.png", p6, width = 12, height = 8, dpi = 300)
cat("✓ 6_异常值检测.png 已保存\n")


cat("\n所有图表已完成\n")