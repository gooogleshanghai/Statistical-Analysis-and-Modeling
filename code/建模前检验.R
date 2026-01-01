library(tseries)

library(dplyr)
library(tidyr)

# 平稳性检验
# 选择一个城市和污染物，例如 PM2.5 在 X1001A
ts_data <- df_clean_filled %>%
  filter(city == "X1001A", pollutant == "PM2.5") %>%
  arrange(datetime) %>%
  pull(value_filled)

# 去掉缺失值
ts_data <- na.omit(ts_data)

# ADF 检验
adf_result <- adf.test(ts_data)
adf_result
kpss.test(ts_data) # KPSS检验

#纯随机检验
# Ljung-Box 检验，lags 可设 10 或 20
Box.test(ts_data, lag = 20, type = "Ljung-Box")


results <- df_clean_filled %>%
  group_by(city, pollutant) %>%
  summarise(
    ts_data = list(na.omit(value_filled)),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    adf_p = if(length(ts_data) > 10) adf.test(ts_data)$p.value else NA,
    ljungbox_p = if(length(ts_data) > 20) Box.test(ts_data, lag = 20, type = "Ljung-Box")$p.value else NA
  )

results
