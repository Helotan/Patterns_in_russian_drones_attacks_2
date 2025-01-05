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

# Perform ACF analysis
png("Graphs/PACF/acf_daily_attacks.png")
pacf(daily_attacks$launched, main="PACF of Daily Attacks in 2024")
dev.off()

# Exclude specified models
excluded_models <- c("Shahed-136/131", "ZALA", "Supercam", "Orlan-10", "Lancet", "Merlin-VR")
data_2024_filtered <- data_2024 %>% filter(!grepl(paste(excluded_models, collapse="|"), model))
png("Graphs/PACF/pacf_daily_attacks_filtered.png")
pacf(data_2024_filtered$launched, main="PACF of Daily Attacks in 2024 (Filtered)")
dev.off()
