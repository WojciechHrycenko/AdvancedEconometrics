library(dplyr)
library(lubridate)  # for extracting year

# Load pre-cleaned dataset (no missing values)
df_final <- read.csv("driver_at_fault_sample_cleaned_no_na.csv", stringsAsFactors = FALSE)

# Define columns to drop
columns_to_remove <- c(
  "beat_number", "pcf_violation", "officer_id", "jurisdiction",
  "id", "case_id", "process_date", "distance",
  "primary_road", "secondary_road", "county_city_location", "chp_road_type", "not_private_property",
  "severe_injury_count", "other_visible_injury_count", "complaint_of_pain_injury_count",
  "pedestrian_killed_count", "pedestrian_injured_count", "bicyclist_killed_count",
  "bicyclist_injured_count", "motorcyclist_killed_count", "motorcyclist_injured_count",
  "primary_collision_factor", "party_type", "party_number", "beat_type",
  "at_fault", "chp_beat_class", "statewide_vehicle_type_at_fault"
)

# Drop columns if present
df_pruned <- df_final %>%
  select(-any_of(columns_to_remove))

# Convert collision date to Date format
df_pruned$collision_date <- as.Date(df_pruned$collision_date)

# Derive car age
df <- df_pruned %>%
  mutate(
    collision_year = year(collision_date),
    vehicle_year = as.numeric(vehicle_year),
    car_age = collision_year - vehicle_year
  ) %>%
  select(-collision_year)

# Compute total number of victims
df <- df %>%
  mutate(
    party_number_killed = as.numeric(party_number_killed),
    party_number_injured = as.numeric(party_number_injured),
    total_victims = party_number_killed + party_number_injured
  )

# Save to CSV
write.csv(df, "driver_at_fault_sample_final.csv", row.names = FALSE)

# Output final column count
cat("Number of columns after pruning:", ncol(df), "\n")