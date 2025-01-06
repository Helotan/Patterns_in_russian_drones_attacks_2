# Load the dataset
data <- read.csv("Data/missile_attacks_daily.csv", header = TRUE, sep = ",")

library(stats)
library(tidyverse)
library(ggplot2)

data$time_start <- as.Date(data$time_start)

# Filter data for the year 2024
filtered_data <- data %>%
  filter(format(time_start, "%Y") == "2024")

# Classify attacks as drones or missiles
drone_models <- c("Shahed-136/131", "Orlan-10", "Lancet", "Supercam", "ZALA", "Merlin-VR")
filtered_data$type <- ifelse(filtered_data$model %in% drone_models, "Drone", "Missile")

# Create a new column for the next day's rocket launches
filtered_data <- filtered_data %>%
  arrange(time_start) %>%
  group_by(time_start) %>%
  summarise(drones_launched = sum(ifelse(type == "Drone", launched, 0))) %>%
  mutate(rockets_launched_next_day = lead(drones_launched, 1))

# Remove rows with NA values
filtered_data <- na.omit(filtered_data)

# Linear Regression: Predicting next day's rocket launches based on drones launched
linear_model <- lm(rockets_launched_next_day ~ drones_launched,
                   data = filtered_data)
print(summary(linear_model))

# Plot linear regression
png("Graphs/Linear_regression/Linear_regression.png")
linear_regression <- ggplot(filtered_data, aes(x = drones_launched, y = rockets_launched_next_day)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Linear Regression: Rocket Launches Next Day Based on Drones Launched",
       x = "Number of Drones Launched", y = "Number of Rocket Launches Next Day")
print(linear_regression)
dev.off()