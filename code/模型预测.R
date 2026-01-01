library(dplyr)
library(forecast)
library(tseries)
library(ggplot2)
library(lubridate)

# 选择要处理的城市
cities <- unique(df_clean$city)

# 创建一个空的数据框来存储所有预测结果
forecast_results <- data.frame(city = character(),
                               datetime = character(),
                               pollutant = character(),
                               forecast_value = numeric(),
                               stringsAsFactors = FALSE)

# 循环处理每个城市
for(city_sel in cities) {
  # 只处理 AQI 指数
  pollutant_sel <- "AQI"
  
  # 提取当前城市和 AQI 的数据
  ts_data <- df_clean %>%
    filter(city == city_sel, pollutant == pollutant_sel) %>%
    arrange(datetime) %>%
    pull(value_filled)
  
  # 转换为时间序列对象（使用全部数据进行训练，用于未来预测）
  ts_series <- ts(ts_data, frequency = 24)
  
  # 取最后1000小时的数据进行建模（提高模型稳定性）
  if (length(ts_series) > 1000) {
    ts_subset <- ts_series[(length(ts_series)-999):length(ts_series)]
  } else {
    ts_subset <- ts_series
  }
  
  # -----------------------------
  # 自动 ARIMA 拟合（含 Box-Cox 变换）
  # -----------------------------
  fit <- auto.arima(
    ts_subset,
    seasonal = TRUE,         # 考虑季节性
    lambda = "auto"          # Box-Cox 自动变换
  )
  
  # -----------------------------
  # 未来24小时预测
  # -----------------------------
  forecast_horizon <- 24  # 预测未来24小时
  fc <- forecast(fit, h = forecast_horizon)
  
  # -----------------------------
  #  模型诊断
  # -----------------------------
  checkresiduals(fit)  # 绘制残差图、ACF/PACF、Ljung-Box 检验
  
  # 生成未来24小时的时间戳
  last_datetime <- max(df_clean %>% 
                       filter(city == city_sel, pollutant == pollutant_sel) %>% 
                       pull(datetime))
  
  # 创建未来24小时的时间序列
  future_datetimes <- seq(from = last_datetime + hours(1), 
                         by = "1 hour", 
                         length.out = forecast_horizon)
  
  # 将预测结果添加到数据框中
  forecast_data <- data.frame(
    city = rep(city_sel, forecast_horizon),
    datetime = as.character(future_datetimes),
    pollutant = rep(pollutant_sel, forecast_horizon),
    forecast_value = as.numeric(fc$mean),
    lower_80 = as.numeric(fc$lower[,1]),  # 80%置信区间下界
    upper_80 = as.numeric(fc$upper[,1]),  # 80%置信区间上界
    lower_95 = as.numeric(fc$lower[,2]),  # 95%置信区间下界
    upper_95 = as.numeric(fc$upper[,2]),  # 95%置信区间上界
    stringsAsFactors = FALSE
  )
  
  # 合并到所有预测结果中
  forecast_results <- bind_rows(forecast_results, forecast_data)
  
  # -----------------------------
  #  可视化预测（包含置信区间）
  # -----------------------------
  p <- autoplot(fc) +
    ggtitle(paste("未来24小时预测 - 城市:", city_sel, "污染物:", pollutant_sel)) +
    xlab("时间") +
    ylab("AQI值") +
    theme_minimal()
  
  # 保存图形（保存到当前目录的figures文件夹）
  if (!dir.exists("figures")) dir.create("figures")
  ggsave(filename = paste0("figures/", city_sel, "-forecast.png"), 
         plot = p, 
         width = 10, 
         height = 6, 
         dpi = 300)
  
  # 输出图形
  print(p)
}

# -----------------------------
# 保存结果到 CSV 文件
# -----------------------------
write.csv(forecast_results, "../data/forecast_results_AQI.csv", row.names = FALSE)

# -----------------------------
# 可选：生成JSON格式数据供HTML使用
# -----------------------------
library(jsonlite)

# 为每个城市生成单独的JSON文件
for(city_sel in cities) {
  city_forecast <- forecast_results %>%
    filter(city == city_sel, pollutant == "AQI") %>%
    select(datetime, forecast_value, lower_80, upper_80, lower_95, upper_95)
  
  # 转换为JSON格式
  json_data <- toJSON(city_forecast, pretty = TRUE)
  
  # 保存JSON文件（文件名与HTML中的城市代码对应）
  city_code_map <- c("X1001A" = "1001A", "X1345A" = "1345A", "X1432A" = "1432A")
  if (city_sel %in% names(city_code_map)) {
    json_filename <- paste0("../data/forecast_", city_code_map[city_sel], ".json")
    write(json_data, json_filename)
    cat("已保存JSON文件:", json_filename, "\n")
  }
}
