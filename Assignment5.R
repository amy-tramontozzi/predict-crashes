library(tidyverse)
library(dplyr)

crashes <- read.csv("crashes.csv", stringsAsFactors = TRUE)

crashes$crash_type <- ifelse(
  crashes$INJURIES_FATAL > 0 | crashes$INJURIES_INCAPACITATING > 0, "Serious",
  ifelse(!(crashes$WEATHER_CONDITION %in% c("CLEAR", "UNKNOWN")), "Bad Weather",
         ifelse(!(crashes$ROADWAY_SURFACE_COND %in% c("DRY", "UNKNOWN")), "Bad Road", "Other")
  )
)

# Load required library
library(dplyr)

crashes$CRASH_DATE <- as.Date(crashes$CRASH_DATE)

daily <- crashes %>%
  group_by(CRASH_DATE) %>%
  summarize(
    count_serious_crashes = sum(crash_type == "Serious", na.rm = TRUE),
    proportion_bad_road = mean(crash_type == "Bad Road", na.rm = TRUE),
    proportion_bad_weather = mean(crash_type == "Bad Weather", na.rm = TRUE),
    day_of_week = first(weekdays(CRASH_DATE)), # Only one value per date
    month_of_year = first(format(CRASH_DATE, "%B")), # Only one value per date
    .groups = "drop"  # Explicitly drop grouping after summarizing
  )

daily <- daily %>%
  mutate(
    day_of_week = factor(day_of_week, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")),
    month_of_year = factor(month_of_year, levels = month.name)
  )

library(MASS)
model_negbinom_log <- glm.nb(
  count_serious_crashes ~ proportion_bad_road + proportion_bad_weather + day_of_week + month_of_year,
  link = "log",
  data = daily
)

# Negative Binomial Log model RMSE
daily$pred <- predict(model_negbinom_log, type = "response")
rmse_nb_log <- sqrt(mean((daily$count_serious_crashes - pred_nb_log)^2))


# Calculate means for each weekday
plot_data <- daily %>%
  group_by(day_of_week) %>%
  summarize(Avg_Observed = mean(count_serious_crashes),
            Avg_Predicted = mean(pred))

# Reshape the data from wide to long format for ggplot2
plot_data_long <- plot_data %>%
  pivot_longer(c(Avg_Observed, Avg_Predicted), names_to = "Type", values_to = "Value")

# Create the paired bar chart for observed vs predicted frequencies
ggplot(plot_data_long, aes(x = day_of_week, y = Value, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(x = "", y = "Average Number of Serious Crashes", fill = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

first_7_days <- daily %>%
  slice(1:7)

first_7_long <- first_7_days %>%
  pivot_longer(c(count_serious_crashes, pred), names_to = "Type", values_to = "Value")

ggplot(first_7_long, aes(x = day_of_week, y = Value, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(x = "", y = "Number of Serious Crashes", fill = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

