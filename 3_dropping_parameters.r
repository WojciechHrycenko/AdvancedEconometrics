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
  "at_fault", "chp_beat_class", "statewide_vehicle_type_at_fault", "intersection", "bicycle_collision", "party_number_killed", 
  "party_number_injured", "chp_shift", "pedestrian_collision", "motorcycle_collision", "truck_collision", "direction", "chp_vehicle_type_towing", 
  "road_condition_1", "pedestrian_action", "control_device", "tow_away", "special_condition", "state_highway_indicator", "cellphone_use_type",
  "statewide_vehicle_type", "other_associate_factor_1"
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
    car_age = pmax(0, collision_year - vehicle_year)
  ) %>%
  select(-collision_year, -vehicle_year)

# Compute total number of victims
df <- df %>%
  mutate(
    killed_victims = as.numeric(killed_victims),
    injured_victims = as.numeric(injured_victims),
    total_victims = killed_victims + injured_victims
  ) %>%
  select(-killed_victims, -injured_victims)

df <- df %>%
  mutate(
    collision_date = as.Date(collision_date),
    month_day = format(collision_date, "%m-%d"),
    season = case_when(
      month_day >= "03-21" & month_day <= "06-20" ~ "Spring",
      month_day >= "06-21" & month_day <= "09-22" ~ "Summer",
      month_day >= "09-23" & month_day <= "12-20" ~ "Fall",
      month_day >= "12-21" | month_day <= "03-20" ~ "Winter",
      TRUE ~ NA_character_
    ),
    year = year(collision_date)
  ) %>%
  select(-collision_date, -month_day)


df <- df %>%
  mutate(
    region = case_when(
      vehicle_make %in% c("ford", "chevrolet", "gmc", "cadillac", "buick", "chrysler",
                          "dodge", "jeep", "lincoln", "plymouth", "pontiac", "saturn",
                          "ram", "hummer", "mercury", "oldsmobile", "american motors",
                          "tesla", "freightliner", "kenworth", "mack", "peterbilt",
                          "international harvester", "bluebird", "thomas", "winnebago",
                          "crown", "ITASCO", "sterling", "autocar", "western trucks",
                          "monaco", "coach", "newflyer", "oschk", "trave", "wstr", "kw",
                          "supre", "geo", "spcn", "ez go") ~ "USA",

      vehicle_make %in% c("toyota", "honda", "nissan", "mazda", "mitsubishi", "subaru",
                          "suzuki", "isuzu", "hino", "lexus", "infiniti", "datsun",
                          "yamaha", "kawasaki", "acura", "scion", "hondaA", "hnda",
                          "hoda", "nssan", "hyuinda") ~ "Japan",

      vehicle_make %in% c("hyundai", "kia", "daewoo") ~ "Korea",

      vehicle_make %in% c("bmw", "audi", "volkswagen", "mercedes-benz", "porsche",
                          "saab", "volvo", "peugeot", "fiat", "mini", "smart",
                          "alfa romera", "bentley", "jaguar", "land rover", "maserati",
                          "lotus", "vespa", "ducati", "triumph", "ktm", "mgb",
                          "mbw", "mbz", "mci", "mz", "lezu") ~ "Europe",

      vehicle_make == "acadian" ~ "Canada",
      vehicle_make == "indian (motorcycle)" ~ "India",
      TRUE ~ "Other"
    )
  ) %>%
  select(-vehicle_make)

df <- df %>%
  mutate(
    collision_hour = hour(hms(collision_time)),
    time_of_day = case_when(
      collision_hour >= 0 & collision_hour < 6  ~ "Night",
      collision_hour >= 6 & collision_hour < 12 ~ "Morning",
      collision_hour >= 12 & collision_hour < 18 ~ "Afternoon",
      collision_hour >= 18 & collision_hour <= 23 ~ "Evening",
      TRUE ~ NA_character_
    )
  ) %>%
  select(-collision_hour, -collision_time)

# Save to CSV
write.csv(df, "driver_at_fault_sample_final.csv", row.names = FALSE)

# Output final column count
cat("Number of columns after pruning:", ncol(df), "\n")