library(ggplot2)
library(dplyr)
library(pscl)
library(MASS)


# 1. Load the dataset
df <- read.csv("driver_at_fault_sample_final.csv", stringsAsFactors = FALSE)
df <- df[1:100000, ]


# 2. Ensure the 'total_victims' column is numeric
df$total_victims <- as.numeric(df$total_victims)

str(df)

cat_vars <- c("road_surface", "direction_of_travel", "lighting", "weather_1",
                          "party_sex", "party_sobriety", "financial_responsibility",
                          "cellphone_in_use", "party_race", "hit_and_run", "season", 
                          "region", "time_of_day", "chp_beat_type", 
                          "chp_vehicle_type_at_fault", "party_safety_equipment_1", 
                          "type_of_collision", "county_location", "population", 
                          "motor_vehicle_involved_with", "movement_preceding_collision")

df[cat_vars] <- lapply(df[cat_vars], as.factor)


unique(df$chp_vehicle_type_at_fault)

df$vehicle_general <- with(df, case_when(
  `chp_vehicle_type_at_fault` %in% c("passenger car, station",
                                     "mini-vans",
                                     "sport utility vehicle",
                                     "motor home",
                                     "motor home > 40 feet",
                                     "passenger car, station wagon, jeep: hazardous waste or hazardous waste/material combination") ~ "Passenger Vehicle",
  `chp_vehicle_type_at_fault` %in% c("pickups & panels",
                                     "pickup w/camper",
                                     "pickups and panels: hazardous material",
                                     "pickups and panels: hazardous waste or hazardous waste/material combination") ~ "Pickup/Panel Truck",
  `chp_vehicle_type_at_fault` %in% c("truck tractor",
                                     "two axle truck",
                                     "three or more axle truck",
                                     "three axle tank truck",
                                     "two-axle tow truck",
                                     "other commercial",
                                     "three or more axle truck: hazardous waste or hazardous waste/material combination",
                                     "three-axle tow truck",
                                     "two axle tank truck",
                                     "two-axle truck: hazardous waste or hazardous waste/material combination") ~ "Commercial Truck",
  `chp_vehicle_type_at_fault` %in% c("school bus public type i",
                                     "school bus public type ii",
                                     "school bus contractual type i",
                                     "school bus contractual type ii",
                                     "school bus private type i",
                                     "non-commercial bus",
                                     "public transit authority",
                                     "paratransit") ~ "Bus",
  `chp_vehicle_type_at_fault` %in% c("motorcycle",
                                     "police motorcycle",
                                     "all terrain vehicle") ~ "Motorcycle/ATV",
  `chp_vehicle_type_at_fault` %in% c("police car",
                                     "ambulance",
                                     "fire truck") ~ "Emergency Vehicle",
  `chp_vehicle_type_at_fault` %in% c("implement of husbandry",
                                     "farm labor transporter",
                                     "low speed vehicle",
                                     "motor driven") ~ "Special/Other",
  TRUE ~ "Other"
))

# Make it a factor
df$vehicle_general <- factor(df$vehicle_general)


unique(df$movement_preceding_collision)

df$move_general <- with(df, case_when(
  movement_preceding_collision %in% c("proceeding straight", "entering traffic", "merging") ~ "Straight Travel",
  movement_preceding_collision %in% c("making right turn", "making left turn", "making u-turn", "other unsafe turning", "other") ~ "Turning",
  movement_preceding_collision %in% c("changing lanes", "passing other vehicle", "crossed into opposing lane") ~ "Lane Change/Passing",
  movement_preceding_collision %in% c("slowing/stopping", "stopped", "backing", "parking maneuver", "parked") ~ "Stopping/Slowing",
  movement_preceding_collision %in% c("ran off road", "traveling wrong way") ~ "Loss of Control/Irregular",
  TRUE ~ "Other"
))

df$move_general <- factor(df$move_general)

unique(df$party_safety_equipment_1)

df$air_bag <- factor(
  ifelse(df$party_safety_equipment_1 == "air bag deployed", "Yes",
         ifelse(df$party_safety_equipment_1 == "air bag not deployed", "No", "Other safety")),
  levels = c("Yes", "No", "Other safety")
)
table(df$air_bag, useNA = "ifany")




unique(df$weather_1)

df$weather <- factor(
  ifelse(df$weather_1 == "clear", "clear_sky",
         ifelse(df$weather_1 %in% c("raining", "snowing", "fog"), "harder_conditions", "other"))
)
table(df$weather, useNA = "ifany")


unique(df$party_sobriety)

df$sobriety <- factor(
  ifelse(df$party_sobriety == "had not been drinking", "not drinking",
         ifelse(df$party_sobriety %in% c(
           "had been drinking, under influence",
           "had been drinking, not under influence",
           "had been drinking, impairment unknown"
         ), "drinking", "other"))
)
table(df$sobriety, useNA = "ifany")


unique(df$lighting)

str(df)

vars <- c(
  "road_surface",
  "weather",
  "party_sex",
  "party_age",
  "sobriety",
  "financial_responsibility",
  "cellphone_in_use",
  "party_race",
  "car_age",
  "season",
  "time_of_day",
  "air_bag",
  "move_general"
)
summary(vars)
sapply(df[vars], function(x) sum(is.na(x)))
lapply(df[vars], function(x) table(x, useNA="ifany"))

str(df[vars])
# previous analysis showed that Zero-Inflated Negative Binomial is the most appropriate choice
# Starting with most general model

general <- zeroinfl(total_victims ~ road_surface + weather + party_sex + party_age + sobriety + financial_responsibility 
                    + cellphone_in_use + party_race + car_age + season + time_of_day
                    + air_bag, data = df, dist = "negbin")

summary(general)
