# Load the dataset
data <- read.csv ("Data/missile_attacks_daily.csv",  header = TRUE, sep = ",")

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

# Logistic Regression: Predicting next day's rocket launches based on drones launched
logistic_model <- glm(rockets_launched_next_day > 0 ~ drones_launched, data = filtered_data, family = "binomial")
print(logistic_model)
filtered_data <- na.omit(filtered_data)

# Plot logistic regression
png("Graphs/Logistic_regression/Logistic_regression.png")
logistic_regression <- ggplot(filtered_data, aes(x = drones_launched, y = rockets_launched_next_day > 0.9)) +
  geom_point() +
  labs(title = "Logistic Regression: Probability of Rocket Launches Next Day Based on Drones Launched",
       x = "Number of Drones Launched", y = "Probability of Rocket Launches Next Day")
print(logistic_regression)
dev.off()


