# 读取数据并做基础检查
library(dplyr)
library(ggplot2)
library(lubridate)
# 数据读取和基础检查
library(readr)
library(dplyr)
library(lubridate)

# 读取 CSV
df <- read_csv("../data/china_cities_2025_long_format.csv",
               col_types = cols(
                 datetime = col_character(),
                 city = col_character(),
                 pollutant = col_character(),
                 value = col_double()
               ))

# 解析 datetime
df <- df %>%
  mutate(
    # 尝试按完整格式解析
    datetime_parsed = as.POSIXct(datetime,
                                 format = "%Y-%m-%d %H:%M:%S",
                                 tz = "Asia/Shanghai"),
    datetime_parsed = ifelse(is.na(datetime_parsed),
                             paste0(datetime, " 00:00:00"),
                             datetime),
    # 最终转换为 POSIXct
    datetime_parsed = as.POSIXct(datetime_parsed,
                                 format = "%Y-%m-%d %H:%M:%S",
                                 tz = "Asia/Shanghai")
  ) %>%
  select(-datetime) %>%       # 可以删除原来的列
  rename(datetime = datetime_parsed)  # 重命名为 datetime

str(df)
summary(df$value)



# 时间覆盖情况
ggplot(df, aes(x = datetime)) +
  geom_density(
    fill = "steelblue",
    alpha = 0.6
  ) +
  labs(
    title = "时间密度分布",
    x = "Datetime",
    y = "Density"
  ) +
  theme_minimal()



# 缺失值比例
missing_summary <- df %>%
  group_by(city, pollutant) %>%
  summarise(
    missing_rate = mean(is.na(value))
  )

ggplot(missing_summary,
       aes(x = pollutant, y = city, fill = missing_rate)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "缺失值比例热力图",
       fill = "Missing Rate")


# 异常值初步识别（箱线图）
ggplot(df,
       aes(x = pollutant, y = value)) +
  geom_boxplot(outlier.color = "red") +
  facet_wrap(~ city, scales = "free") +
  labs(title = "异常值初步识别（箱线图）")


# 时间序列形态探索
library(scales)
ggplot(df_clean, aes(x = datetime, y = value)) +
  geom_line(alpha = 0.3) +
  facet_grid(pollutant ~ city, scales = "free_y") +
  scale_x_datetime(
    date_breaks = "1 month",   # 每个月一个刻度
    date_labels = "%Y-%m"      # 标签显示格式，例如 2025-12
  ) +
  labs(
    title = "各城市各污染物时间序列",
    x = "Month",
    y = "Value"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # 斜着显示避免重叠
  )

# 平滑趋势
ggplot(df_clean,
       aes(x = datetime, y = value)) +
  geom_line(alpha = 0.2) +
  geom_smooth(method = "loess", span = 0.1, color = "red") +
  facet_grid(pollutant ~ city, scales = "free_y") +
  labs(title = "时间趋势（LOESS 平滑）")









