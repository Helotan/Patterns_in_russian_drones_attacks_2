# Load necessary libraries
library(stats)
library(forecast)
library(ggplot2)

# Load the dataset
data <- read.csv ("Data/missile_attacks_daily.csv", header = TRUE, sep = ",")

# Filter data for the year 2024
data$time_start <- as.POSIXct(data$time_start, format="%Y-%m-%d %H:%M")
data_2024 <- subset(data, format(time_start, "%Y") == "2024")

# Aggregate the number of attacks per day
data_2024$date <- as.Date(data_2024$time_start)
daily_attacks <- aggregate(launched ~ date, data_2024, sum)

# Plot the time series of daily attacks
time_series_plot <- ggplot(daily_attacks, aes(x=date, y=launched)) +
  geom_line() +
  labs(title="Щоденні ракетні удари та атаки дронів", x="Дата", y="Кількість атак")

ggsave("Graphs/ACF/time_series_2024.png", plot = time_series_plot)
# Perform ACF analysis
png("Graphs/ACF/acf_daily_attacks.png")
acf(daily_attacks$launched, main="Графік автокореляції атак ракет і дронів у 2024 році")
dev.off()

# Exclude specified models
excluded_models <- c("Shahed-136/131", "ZALA", "Supercam", "Orlan-10", "Lancet", "Merlin-VR")
data_2024_filtered <- data_2024 %>% filter(!grepl(paste(excluded_models, collapse="|"), model))
png("Graphs/ACF/acf_daily_attacks_filtered.png")
acf(data_2024_filtered$launched, main="Графік автокореляції атак ракет")
dev.off()

