library(lubridate) 

# 1. 预处理：从 Date 和 Time 列合并并提取特征

data_time <- data_clean %>%

  mutate(

    Hour = as.numeric(substring(Time, 1, 2)), 
    
    # 处理日期，提取月份
    Date_parsed = as.Date(Date, format = "%d/%m/%Y"),
    Month = factor(month(Date_parsed), labels = month.abb)
  ) %>%
  filter(!is.na(CO.GT.))

# 2. 绘图：按小时变化的箱线图
p_hour <- ggplot(data_time, aes(x = factor(Hour), y = CO.GT.)) +
  geom_boxplot(fill = "#4E84C4", outlier.size = 0.5, outlier.alpha = 0.3) +
  labs(title = "一天中不同时段的CO浓度变化 (日内规律)", 
       x = "小时 (0-23)", y = "CO(GT)") +
  my_theme

# 3. 绘图：按月份变化的箱线图
p_month <- ggplot(data_time, aes(x = Month, y = CO.GT.)) +
  geom_boxplot(fill = "orange", outlier.size = 0.5, outlier.alpha = 0.3) +
  labs(title = "不同月份的CO浓度变化 (季节性)", 
       x = "月份", y = "CO(GT)") +
  my_theme

# 拼图
p_time_features <- p_hour / p_month
ggsave("7_时间特征分析.png", p_time_features, width = 10, height = 10, dpi = 300)