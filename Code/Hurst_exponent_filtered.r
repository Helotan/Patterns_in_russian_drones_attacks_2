# Load necessary libraries
library(pracma)

# Load the dataset
data <- read.csv ("Data/missile_attacks_daily.csv", header = TRUE, sep = ",")

# Filter data for the year 2024
data$time_start <- as.POSIXct(data$time_start, format="%Y-%m-%d %H:%M")
data_2024 <- subset(data, format(time_start, "%Y") == "2024")

# Exclude specified models
excluded_models <- c("Shahed-136/131", "ZALA", "Supercam", "Orlan-10", "Lancet", "Merlin-VR")
data_2024_filtered <- data_2024 %>% filter(!grepl(paste(excluded_models, collapse="|"), model))

# Aggregate the number of attacks per day
data_2024_filtered$date <- as.Date(data_2024_filtered$time_start)
daily_attacks <- aggregate(launched ~ date, data_2024_filtered, sum)


hurst_exponent <- hurstexp(daily_attacks$launched)
print(hurst_exponent)
