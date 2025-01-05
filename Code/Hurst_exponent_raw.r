# Load necessary libraries
library(pracma)

# Load the dataset
data <- read.csv ("Data/missile_attacks_daily.csv", header = TRUE, sep = ",")

# Filter data for the year 2024
data$time_start <- as.POSIXct(data$time_start, format="%Y-%m-%d %H:%M")
data_2024 <- subset(data, format(time_start, "%Y") == "2024")

# Aggregate the number of attacks per day
data_2024$date <- as.Date(data_2024$time_start)
daily_attacks <- aggregate(launched ~ date, data_2024, sum)
hurst_exponent <- hurstexp(daily_attacks$launched)
print(hurst_exponent)