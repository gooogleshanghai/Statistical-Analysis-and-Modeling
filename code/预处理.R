# 数据整理
library(dplyr)
library(tidyr)
library(lubridate)

# 时间序列缺失处理
library(zoo)        # na.approx
library(imputeTS)   # na.interpolation / na.kalman

# 可视化（用于检查）
library(ggplot2)

install.packages("imputeTS")

# 剔除没有数据的城市
city_missing <- df %>%
  group_by(city) %>%
  summarise(na_rate = mean(is.na(value))) %>%
  arrange(desc(na_rate))

city_missing
cities_to_remove <- city_missing %>%
  filter(na_rate > 0.9) %>%
  pull(city)

cities_to_remove
df_clean <- df %>%
  filter(!city %in% cities_to_remove)

# df_clean：
# 包含字段：
# datetime | city | pollutant | value
# 已剔除全红城市和明显无效序列

df <- df_clean
df$datetime <- as.POSIXct(df$datetime)


#平滑插补，使用 STL 分解方法插补缺失
df_filled <- df %>%
  group_by(city, pollutant) %>%
  arrange(datetime) %>%
  mutate(
    value_filled = na_interpolation(value, option = "spline")  # 样条插值
  ) %>%
  ungroup()

# 异常值处理
df_clean <- df_filled %>%
  group_by(city, pollutant) %>%
  mutate(
    value_clean = {
      # 计算 IQR
      q1 <- quantile(value_filled, 0.25)
      q3 <- quantile(value_filled, 0.75)
      iqr <- q3 - q1
      # 异常值处理，返回最终列
      ifelse(value_filled < q1 - 1.5*iqr | value_filled > q3 + 1.5*iqr,
             NA, value_filled)
    }
  ) %>%
  ungroup()
#再对空值进行插补
df_clean_filled <- df_clean %>%
  group_by(city, pollutant) %>%
  arrange(datetime) %>%
  mutate(
    value_filled = na_interpolation(value_clean, option = "spline")  # 样条插值
  ) %>%
  ungroup()



save(df_clean_filled, file = "../data/df_clean.RData")

load("../data/df_clean.RData")


