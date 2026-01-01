# set user library path
user_lib <- Sys.getenv("R_LIBS_USER")
if(user_lib == ""){
  user_lib <- file.path(Sys.getenv("USERPROFILE"), "R", "win-library", paste0(R.version$major, ".", strsplit(R.version$minor, "\\.")[[1]][1]))
}
if(!dir.exists(user_lib)) dir.create(user_lib, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(user_lib, .libPaths()))

# ensure ggplot2
if(!requireNamespace("ggplot2", quietly=TRUE)){
  install.packages("ggplot2", repos="https://cloud.r-project.org", quiet=TRUE, lib=user_lib)
}

# run EDA plots
library(ggplot2)
dir.create("figures", showWarnings = FALSE)

font_family <- if (.Platform$OS.type == "windows") "Microsoft YaHei" else ""
base_theme <- theme_minimal() + theme(text = element_text(family = font_family))

city_data <- read.csv("../data/City_Types.csv", stringsAsFactors = FALSE)
city_data$Type <- factor(city_data$Type)
city_data$City <- as.factor(city_data$City)
city_data$Date <- as.POSIXct(city_data$Date, format = "%Y-%m-%d %H:%M:%S%z", tz = "UTC")
lt <- as.POSIXlt(city_data$Date, tz = "UTC")
city_data$hour  <- lt$hour
city_data$month <- lt$mon + 1
pollutants <- c("CO","NO2","SO2","O3","PM2.5","PM10")
for (col in pollutants) {
  med <- median(city_data[[col]], na.rm = TRUE)
  city_data[[col]][is.na(city_data[[col]])] <- med
}

# Type distribution
p_type <- ggplot(city_data, aes(x = Type, fill = Type)) +
  geom_bar() +
  geom_text(stat = "count",
            aes(label = paste0(..count.., " (",
                               round(..count../sum(..count..) * 100, 1), "%)")),
            vjust = -0.3) +
  labs(title = "城市类型分布", x = "类型", y = "数量") +
  base_theme
ggsave("figures/type_distribution.png", p_type, width = 6, height = 4, dpi = 300)

# Pollutant boxplots
long_df <- do.call(rbind, lapply(pollutants, function(col) {
  data.frame(Pollutant = col,
             Value = city_data[[col]],
             Type = city_data$Type,
             stringsAsFactors = FALSE)
}))

p_box <- ggplot(long_df, aes(x = Type, y = Value, fill = Type)) +
  geom_boxplot(outlier.size = 0.6) +
  facet_wrap(~ Pollutant, scales = "free_y") +
  labs(title = "不同类型城市的污染物箱线图", x = "类型", y = "浓度") +
  base_theme
ggsave("figures/pollutant_boxplots.png", p_box, width = 10, height = 6, dpi = 300)

# Correlation heatmap
cor_mat <- cor(city_data[, pollutants], use = "pairwise.complete.obs")
cor_df  <- as.data.frame(as.table(cor_mat))
names(cor_df) <- c("Var1", "Var2", "Correlation")

p_cor <- ggplot(cor_df, aes(Var1, Var2, fill = Correlation)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B",
                       midpoint = 0, limits = c(-1, 1)) +
  labs(title = "污染物相关性热力图", x = "", y = "") +
  base_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("figures/pollutant_correlations.png", p_cor, width = 6, height = 5, dpi = 300)

# Hourly trend by Type
long_time_df <- do.call(rbind, lapply(pollutants, function(col) {
  data.frame(Pollutant = col,
             Value = city_data[[col]],
             Type = city_data$Type,
             hour = city_data$hour,
             month = city_data$month,
             stringsAsFactors = FALSE)
}))

hourly_df <- aggregate(Value ~ hour + Type + Pollutant, data = long_time_df, mean)
p_hour <- ggplot(hourly_df, aes(x = hour, y = Value, color = Type)) +
  geom_line(linewidth = 0.7) +
  facet_wrap(~ Pollutant, scales = "free_y") +
  labs(title = "不同类型城市的逐小时污染物均值", x = "小时", y = "均值") +
  base_theme
ggsave("figures/pollutant_hourly_trends.png", p_hour, width = 10, height = 6, dpi = 300)

# Monthly heatmap (all samples)
monthly_df <- aggregate(Value ~ month + Pollutant, data = long_time_df, mean)
monthly_df$month <- factor(monthly_df$month, levels = 1:12)
p_month <- ggplot(monthly_df, aes(x = month, y = Pollutant, fill = Value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#FEE8C8", high = "#E34A33") +
  labs(title = "污染物月度均值热力图（全体样本）", x = "月份", y = "污染物", fill = "均值") +
  base_theme
ggsave("figures/pollutant_monthly_heatmap.png", p_month, width = 7, height = 4, dpi = 300)

cat("Saved plots to figures/\n")
