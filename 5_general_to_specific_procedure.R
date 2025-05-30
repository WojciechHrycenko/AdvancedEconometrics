library(ggplot2)
library(dplyr)
library(pscl)
library(MASS)
install.packages("lmtest")  # only if not already installed
library(lmtest)
install.packages("modelsummary")  # only if not already installed
library(modelsummary)  # for table formatting
library(broom) 


df <- read.csv("driver_at_fault_sample_final.csv", stringsAsFactors = FALSE)
df <- df[1:65000, ]


df$total_victims <- as.numeric(df$total_victims)
str(df)

# turning all categorical variables into fators
cat_vars <- c("road_surface", "direction_of_travel", "lighting", "weather_1",
                          "party_sex", "party_sobriety", "financial_responsibility",
                          "cellphone_in_use", "party_race", "hit_and_run", "season", 
                          "region", "time_of_day", "chp_beat_type", 
                          "chp_vehicle_type_at_fault", "party_safety_equipment_1", 
                          "type_of_collision", "county_location", "population", 
                          "motor_vehicle_involved_with", "movement_preceding_collision")

df[cat_vars] <- lapply(df[cat_vars], as.factor)


# aggregating vehicle type into more general groups
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


df$vehicle_general <- factor(df$vehicle_general)


# aggregating movement into more general groups
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


# aggregating safety equipment into air_bag variable
unique(df$party_safety_equipment_1)

df$air_bag <- factor(
  ifelse(df$party_safety_equipment_1 == "air bag deployed", "Yes",
         ifelse(df$party_safety_equipment_1 == "air bag not deployed", "No", "Other safety")),
  levels = c("Yes", "No", "Other safety")
)
table(df$air_bag, useNA = "ifany")



# aggregating weather into more general groups
unique(df$weather_1)

df$weather <- factor(
  ifelse(df$weather_1 == "clear", "clear_sky",
         ifelse(df$weather_1 %in% c("raining", "snowing", "fog"), "harder_conditions", "other"))
)
table(df$weather, useNA = "ifany")


# aggregating sobriety into more general groups
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


# aggregating road surface into more general groups
unique(df$road_surface)

df$surface_dry <- ifelse(df$road_surface == "dry", "dry", "non-dry")
df$surface_dry <- factor(df$surface_dry)


str(df)

# converting age into 2 intervals: under and over 25 years old
df$age_group <- cut(
  df$party_age,
  breaks = c(-Inf, 24, Inf),
  labels = c("under 25", "over 25"),
  right = TRUE
)


colSums(is.na(df))


# aggregating weather into 2 seasons 
df$LA_season <- ifelse(df$season %in% c("Winter", "Spring"), "Wet", "Dry")
df$LA_season <- factor(df$LA_season, levels = c("Dry", "Wet"))
table(df$season)


# aggregating race into white and non-white
unique(df$party_race)

df$race_group <- ifelse(df$party_race == "white", "white", "non-white")
df$race_group <- factor(df$race_group, levels = c("white", "non-white"))


# aggregating insurence into 2 groups
unique(df$financial_responsibility)

df$financial_responsibility_bi <- ifelse(df$financial_responsibility == "proof of insurance obtained", 
                            "proof", "no or unknown proof")
df$financial_responsibility_bi <- factor(df$financial_responsibility_bi, levels = c("proof", "no or unknown proof"))


# aggregating car age into 2 intervals
df$car_age_group <- ifelse(df$car_age <= 15, "under_15", "over_15")
df$car_age_group <- factor(df$car_age_group, levels = c("under_15", "over_15"))


# aggregating lighting into 2 groups
df$lighting_simple <- ifelse(df$lighting == "daylight", "daylight", "dark")
df$lighting_simple <- factor(df$lighting_simple, levels = c("daylight", "dark"))


# INTERACTIONS

# sex x race
df$sex_race <- interaction(df$party_sex, df$race_group, drop = TRUE)
table(df$sex_race, useNA="ifany")

# time of the day x weather
df$timeofday_weather <- interaction(df$time_of_day, df$weather, drop = TRUE)
df$age_car <- interaction(df$age_group, df$car_age_group, drop = TRUE)

# weather x road surface
df$lighting_surface <- interaction(df$lighting_simple, df$surface_dry, drop = TRUE)
table(df$lighting_surface)



# MODELLING

# previous analysis showed that Zero-Inflated Negative Binomial is the most appropriate choice
# Starting with most general model


general <- zeroinfl(total_victims ~ weather + party_sex + age_group + sobriety + financial_responsibility_bi 
                    + cellphone_in_use + race_group + car_age_group + LA_season + time_of_day + air_bag + lighting_surface, data = df, dist = "negbin")

summary(general)


# to analyze it properly we'll use Likelihood Ratio Test. Its hypotheses:
# H0: removed variable is jointly significant
# H1: removed variable isn't jointly significant

# I'll start with dropping variable insignificant in both outputs - LA_season
step1 <- zeroinfl(total_victims ~ weather + party_sex + age_group + sobriety + financial_responsibility_bi 
                    + cellphone_in_use + race_group + car_age_group + time_of_day + air_bag + lighting_surface, data = df, dist = "negbin")

summary(step1)

lrtest(step1, general)
# p-value = 0,072 -> we fail to reject H0. No evidence that "LA_season" is jointly significant - removing it.


# in step 2 we are dropping the "weather" variable
step2 <- zeroinfl(total_victims ~ party_sex + age_group + sobriety + financial_responsibility_bi 
                  + cellphone_in_use + race_group + car_age_group + time_of_day + air_bag + lighting_surface, data = df, dist = "negbin")

summary(step2)

lrtest(step2, step1)
# p-value = 0,23 -> we fail to reject H0. No evidence that "weather" is jointly significant - removing it.

# in step 3 we are dropping "time_of_day" variable
step3 <- zeroinfl(total_victims ~ party_sex + age_group + sobriety + financial_responsibility_bi 
                  + cellphone_in_use + race_group + car_age_group + air_bag + lighting_surface, data = df, dist = "negbin")

summary(step3)

lrtest(step3, step2)
# p-value = <2.2e-16  -> we  reject H0. "time_of_day" is jointly significant.
# We are going back to model from step 2


# in step 4 we bring back "time_of_day" and remove "sobriety"
step4 <- zeroinfl(total_victims ~ party_sex + age_group + sobriety + financial_responsibility_bi 
                  + cellphone_in_use + race_group + car_age_group + time_of_day + air_bag + lighting_surface, data = df, dist = "negbin")
summary(step4)

lrtest(step4, step2)
# p-value = 7.551e-07  -> we reject H0. "sobriety" is jointly significant
# We are going back to model from step 2
 
# in step 5 we bring back "sobriety" and remove "lighting_surfacedark"
step5 <- zeroinfl(total_victims ~ party_sex + age_group + sobriety + financial_responsibility_bi 
                  + cellphone_in_use + race_group + car_age_group + time_of_day + air_bag, data = df, dist = "negbin")

summary(step5)

lrtest(step5, step2)
# p-value = 6.714e-06  -> we reject H0. "lighting_surfacedark" is jointly significant
# We are going back to model from step 2


# in step 6 we bring back "lighting_surfacedark" and remove "cellphone_in_use"
step6 <- zeroinfl(total_victims ~ party_sex + age_group + sobriety + financial_responsibility_bi 
                  + race_group + car_age_group + time_of_day + air_bag + lighting_surface, data = df, dist = "negbin")

summary(step6)

lrtest(step6, step2)
# p-value = 0.04098  -> we reject H0. "cellphone_in_use" is jointly significant
# We are going back to model from step 2


# in step 7 we bring back "cellphone_in_use" and remove "car_age_group"
step7 <- zeroinfl(total_victims ~ party_sex + age_group + sobriety + financial_responsibility_bi 
                  + cellphone_in_use + race_group + time_of_day + air_bag + lighting_surface, data = df, dist = "negbin")

summary(step7)

lrtest(step7, step2)
# p-value = 0.01959  -> we reject H0. "car_age_group" is jointly significant
# We are going back to model from step 2


# in step 8 we bring back "car_age_group" and remove "race_group"
step8 <- zeroinfl(total_victims ~ party_sex + age_group + sobriety + financial_responsibility_bi 
                  + cellphone_in_use +  car_age_group + time_of_day + air_bag + lighting_surface, data = df, dist = "negbin")

summary(step8)

lrtest(step8, step2)
# p-value = 1.524e-07  -> we reject H0. "race_group" is jointly significant
# We are going back to model from step 2



# in step 9 we bring back "race_group" and remove "air_bag"
step9 <- zeroinfl(total_victims ~ party_sex + age_group + sobriety + financial_responsibility_bi 
                  + cellphone_in_use + race_group + car_age_group + time_of_day + lighting_surface, data = df, dist = "negbin")

summary(step9)

lrtest(step9, step2)
# p-value = <2.2e-16 -> we reject H0. "air_bag" is jointly significant
# We are going back to model from step 2


# in step 10 we bring back "air_bag" and remove "age_group"
step10 <- zeroinfl(total_victims ~ party_sex +  sobriety + financial_responsibility_bi 
                  + cellphone_in_use + race_group + car_age_group + time_of_day + air_bag + lighting_surface, data = df, dist = "negbin")

summary(step10)

lrtest(step10, step2)
# p-value = 2.838e-06 -> we reject H0. "age_group" is jointly significant
# We are going back to model from step 2

# We checked all variables and left all jointly significant
final_model <- zeroinfl(total_victims ~ party_sex + age_group + sobriety + financial_responsibility_bi 
                  + cellphone_in_use + race_group + car_age_group + time_of_day + air_bag + lighting_surface, data = df, dist = "negbin")

summary(final_model)


# preparing baseline models with all initial variables
poisson_model <- glm(total_victims ~ weather + party_sex + age_group + sobriety + financial_responsibility_bi 
                     + cellphone_in_use + race_group + car_age_group + LA_season + time_of_day + air_bag + lighting_surface, family = "poisson", data = df)

# Negative Binomial
nb_model <- glm.nb(total_victims ~ weather + party_sex + age_group + sobriety + financial_responsibility_bi 
                   + cellphone_in_use + race_group + car_age_group + LA_season + time_of_day + air_bag + lighting_surface, data = df)

# ZIP
zip_model <- zeroinfl(total_victims ~ weather + party_sex + age_group + sobriety + financial_responsibility_bi 
                      + cellphone_in_use + race_group + car_age_group + LA_season + time_of_day + air_bag + lighting_surface, data = df, dist = "poisson")




# Named list of models
models <- list(
  "Poisson" = poisson_model,
  "Negative Binomial" = nb_model,
  "ZIP" = zip_model,
  "Full model (ZINB)" = general,
  "Final model (ZINB)" = final_model
)

# Custom formatting
modelsummary(models,
             statistic = "std.error",  # or "std.error" or "conf.int"
             stars = TRUE,
             output = "markdown"  # or "latex", "html", "data.frame"
)

modelsummary()
