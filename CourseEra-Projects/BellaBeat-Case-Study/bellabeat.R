## install packages
install.packages("tidyverse")
install.packages("janitor")


## load packages
library(ggplot2)
library(janitor)
library(lubridate)
library(tidyverse)

## import all relevant datasets using read_csv
daily_activity <- read_csv("dailyActivity_merged.csv")
minute_sleep <- read_csv("minuteSleep_merged.csv")
hourly_steps <- read_csv("hourlySteps_merged.csv")

############## PREPARE: inspect/clean the data frames ##################

## check for duplicates
daily_activity <- daily_activity %>%
  distinct()
minute_sleep <- minute_sleep %>%
  distinct()
hourly_steps <- hourly_steps %>%
  distinct()

## check for unique users
n_distinct(daily_activity$Id)
n_distinct(minute_sleep$Id)
n_distinct(hourly_steps$Id)

## standardize column names 
daily_activity <- daily_activity %>% clean_names()
minute_sleep <- minute_sleep %>% clean_names()
hourly_steps <- hourly_steps %>% clean_names()

## format data
daily_activity <- daily_activity %>%
  mutate(activity_date = mdy(activity_date))
minute_sleep <- minute_sleep %>%
  separate(date, into = c("sleep_date", "sleep_time"), sep = " ") %>%
  mutate(sleep_date = mdy(sleep_date))
hourly_steps <- hourly_steps %>%
  mutate(activity_hour = mdy_hms(activity_hour))

## aggregate the sleep and hourly steps data to represent daily totals
daily_sleep_total <- minute_sleep %>%
  group_by(id, sleep_date) %>%
  summarize(total_minutes_asleep = sum(value), .groups = "drop") 
daily_steps_total <- hourly_steps %>%
  group_by(id, date = as_date(activity_hour)) %>%
  summarize(total_steps = sum(step_total), .groups = "drop") 


## merge datasets
combined_data <- daily_activity %>%
  inner_join(daily_sleep_total, by = c("id", "activity_date" = "sleep_date"))
combined_data <- combined_data %>%
  inner_join(daily_steps_total, by = c("id" = "id", "activity_date" = "date"))



############## ANALYZE: find patterns & trends ##################
### rename total_steps.y column and then calculate average daily steps, calories burned, and minutes slept
combined_data <- combined_data %>%
  rename(hourly_total_steps = total_steps.y)

combined_data %>%
  summarize(
    average_daily_steps = mean(hourly_total_steps),
    average_calories_burned = mean(calories),
    average_sleep_minutes = mean(total_minutes_asleep)
  )

## analyze daily usage trends
### add day_of_week column
combined_data <- combined_data %>%
  mutate(day_of_week = wday(activity_date, label = TRUE))

### calculate average steps by the weekday
daily_steps_by_day <- combined_data %>%
  group_by(day_of_week) %>%
  summarize(
    average_steps = mean(hourly_total_steps),
    average_calories = mean(calories)
  )

## segment users by activity level: categorize users by their average daily steps
### create a segmented user column
user_segments <- combined_data %>%
  group_by(id) %>%
  summarize(average_steps = mean(hourly_total_steps)) %>%
  ungroup() %>%
  mutate(
    user_type = case_when(
      average_steps < 5000 ~ "Not Active",
      average_steps >= 5000 & average_steps < 7500 ~ "Lightly Active",
      average_steps >= 7500 & average_steps < 10000 ~ "Fairly Active",
      average_steps >= 10000 ~ "Very Active"
    )
  )



## ggplot!

### total steps vs. calories burned
ggplot(data = combined_data, aes(x = hourly_total_steps, y = calories)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Total Steps vs. Calories Burned",
       x = "Total Steps",
       y = "Calories Burned")

### average steps by the day of the week
ggplot(data = daily_steps_by_day, aes(x = day_of_week, y = average_steps)) +
  geom_col(fill = "khaki1") +
  labs(title = "Average Steps by Day of the Week",
       x = "Day of the Week",
       y = "Average Steps")


