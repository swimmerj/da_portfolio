# ==============================================================================
# PROJECT: Cyclistic Bike-Share Analysis
# AUTHOR: Juraj Plavka
# DATE: 11.12.2025
# DESCRIPTION: 
# This script imports 12 months of Cyclistic trip data, cleans anomalies,
# and prepares the dataset for analysis to understand rider behavior.
# ==============================================================================

# 1. SETUP & CONFIGURATION
# ------------------------------------------------------------------------------
# Load necessary packages for data manipulation and date handling
library(tidyverse)
library(lubridate)
library(arrow)

# Verify Working Directory
print(paste("Current working directory:", getwd()))

# Define file path to the raw data
raw_data_path <- "../data/raw_data"
processed_data_path <- "../data/processed_data"

# 2. DATA IMPORT
# ------------------------------------------------------------------------------
# Import and merge 12 months of data into a single dataframe
df <- list.files(path = raw_data_path, pattern = "*.csv", full.names = TRUE) %>%
  map_df(~read_csv(.))

# Specific inspection of the raw data structure
glimpse(df)

# 3. DATA CLEANING & TRANSFORMATION
# ------------------------------------------------------------------------------
# Create a clean version of the dataset

df_clean <- df %>%
  
# STEP A: String Standardization
# Trim whitespace from IDs to ensure accurate counting/matching later
  mutate(
    across(c(ride_id, start_station_id, end_station_id), ~str_trim(.)),
    
# Lowercase and trim text fields to fix inconsistencies
    across(c(rideable_type, start_station_name, end_station_name, member_casual), 
           ~str_trim(str_to_lower(.))),
  ) %>%
  
# STEP B: Handling Missing Values
# Replace empty strings in text columns with "NA" for clarity in analysis.
  mutate(across(where(is.character), ~replace_na(., "NA")))

# Inspect the clean structure
glimpse(df_clean)

# 4. FEATURE ENGINEERING
# ------------------------------------------------------------------------------
# Add calculations required for the specific business questions.

df_final <- df_clean %>%
  mutate(
# Calculate trip duration
    trip_duration = ended_at - started_at,
# Extract ride starting hour
    hour_of_the_day = hour(started_at),
# Extract day of the week (1=Monday)
    weekday_started = wday(started_at, week_start = 1),
# Add a column with the day names as an ordered factor
    day_of_week_name = factor(weekday_started,
                              levels = 1:7,
                              labels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
    ))

# Inspect the structure
glimpse(df_final)

# 5. DATA QUALITY FILTERING
# ------------------------------------------------------------------------------
# Filter out "bad" data points that don't represent genuine trips..
# Criteria:
# - Remove trips < 60 seconds (potential false starts or docking errors)
# - Remove trips > 24 hours (potential stolen bikes or system errors)

min_duration_secs <- 60            # 1 minute
max_duration_secs <- 24 * 60 * 60  # 24 hours

df_filtered <- df_final %>%
  filter(
    trip_duration >= as.difftime(min_duration_secs, units = "secs"),
    trip_duration <= as.difftime(max_duration_secs, units = "secs"))    

# 6. EXPORT
# ------------------------------------------------------------------------------
# Save the clean dataset for future use or Tableau visualization.
# Note: Ensure the 'processed_data' folder exists.
write_parquet(df_filtered, "../data/processed_data/cyclistic_clean.parquet")
write_csv(df_filtered, "../data/processed_data/cyclistic_clean.csv")

# 7. ANALYSIS: AGGREGATIONS
# ------------------------------------------------------------------------------

# Metric 1: Average trip duration by user type
avg_duration_by_usertype <- df_filtered %>%
group_by(member_casual) %>%
  summarise(average_duration_mins = as.numeric(mean(trip_duration, na.rm = TRUE), units = "mins"))


# Metric 2: Most popular day of the week
mode_by_usertype <- df_filtered %>%
  count(member_casual, weekday_started) %>%
  group_by(member_casual) %>%
  slice_max(order_by = n, n = 1)

# 8. VISUALIZATION
# ------------------------------------------------------------------------------

# Chart 1: Total Rides by User Type
ggplot(df_filtered, aes(x = member_casual, fill = member_casual)) +
  geom_bar() +
# Add count labels on top
  geom_text(stat='count', aes(label=after_stat(count)), vjust=-0.5)+
  labs(
    title = "Total Number of Rides by User Type",
    subtitle = "Casual riders have a significant number of trips",
    x = "User Type",
    y = "Total Number of Rides",
    fill = "Membership type:") +
  theme_minimal()

  
# Chart 2: Average Trip Duration by User Type
  
ggplot(avg_duration_by_usertype, aes(x = member_casual, y = average_duration_mins, fill = member_casual)) +
  geom_col() +
  geom_text(aes(label = paste(round(average_duration_mins, 1), "mins")), vjust=-0.5) +
  labs(
    title = "Average Trip Duration by User Type",
    subtitle = "Casual riders' trips are significantly longer on average",
    x = "User Type",
    y = "Average Trip Duration (in seconds)",
    fill = "Membership type:") +
  theme_minimal()

# Chart 3: Rides by Day of the Week

ggplot(df_filtered, aes(x = day_of_week_name, fill = member_casual)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Ridership by Day of the Week",
    subtitle = "Members are consistent, while casual use peaks on weekends",
    x = "Day of the Week",
    y = "Total Number of Rides",
    fill = "Member type:") +
  theme_minimal()

# Chart 4: Bike Type Preference

ggplot(df_filtered, aes(x = member_casual, fill = rideable_type)) +
  geom_bar(position = "fill") +
  labs(
    title = "Bike Type Preference by User Group",
    subtitle = "A comparison of the proportion of each bike type used",
    x = "User Type",
    y = "Proportion of Total Rides",
    fill = "Bike Type:") +
  theme_minimal()

# Chart 5: Hourly Trends

ggplot(df_filtered, aes(x = hour_of_the_day, color = member_casual)) +
  geom_line(stat="count", linewidth=1.5) +
  labs(
    title = "Rides by Hour of the Day",
    subtitle = "Member usage peaks during commute hours; casual use is highest in the afternoon",
    x = "Hour of the Day (0-23)",
    y = "Total Number of Rides",
    color = "Member type:") +
  theme_minimal()
