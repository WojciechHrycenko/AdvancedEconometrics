library(ggplot2)
library(dplyr)
library(pscl)
library(MASS)

# 1. Load the dataset
df <- read.csv("driver_at_fault_sample_final.csv", stringsAsFactors = FALSE)
df <- df[1:10000, ]
# 2. Ensure the 'total_victims' column is numeric
df$total_victims <- as.numeric(df$total_victims)

# 3. Calculate the count and percentage of zeros in 'total_victims'
zero_count <- sum(df$total_victims == 0, na.rm = TRUE)
total_count <- sum(!is.na(df$total_victims))
zero_percent <- round(100 * zero_count / total_count, 2)

cat("Number of zeros: ", zero_count, " out of ", total_count, " observations\n")
cat("Percentage of zeros: ", zero_percent, "%\n")

# 4. Basic bar plot showing the distribution of total victims
ggplot(df, aes(x = total_victims)) +
  geom_bar(fill = "steelblue") +
  labs(
    title = "Distribution of Total Victims",
    x = "Number of Victims",
    y = "Number of Accidents"
  ) +
  theme_minimal()

# 5. Define a function to perform a score test for zero inflation and LT test
zero.test <- function(x) {
  if(is.table(x)) {
    if(length(dim(x)) > 1) stop("Input must be a 1-way table")
    x <- rep(as.numeric(names(x)), unname(c(x)))
  }
  lambda <- mean(x)
  p0_tilde <- exp(-lambda)
  n0 <- sum(!(x > 0))
  n <- length(x)
  numerator <- (n0 - n * p0_tilde)^2
  denominator <- n * p0_tilde * (1 - p0_tilde) - n * lambda * (p0_tilde^2)
  stat <- numerator / denominator
  pvalue <- pchisq(stat, df = 1, lower.tail = FALSE)
  result <- list(statistic = stat, df = 1, p_value = pvalue)
  
  cat(paste("Score test for zero inflation\n\n",
            "\tChi-square =", round(stat, 5), "\n",
            "\tdf = 1\n",
            "\tp-value:", format.pval(pvalue), "\n"))
  print(result)
}

lr_test <- function(model1, model2) {
  lr_stat <- 2 * (logLik(model2)[1] - logLik(model1)[1])
  df_diff <- attr(logLik(model2), "df") - attr(logLik(model1), "df")
  p_val <- pchisq(lr_stat, df = df_diff, lower.tail = FALSE)
  cat("LR test comparing models:\n")
  cat("  Statistic =", round(lr_stat, 4), "\n")
  cat("  Degrees of freedom =", df_diff, "\n")
  cat("  p-value =", p_val, "\n\n")
  return(list(statistic = lr_stat, df = df_diff, p.value = p_val))
}

# 6. Perform zero-inflation test on 'total_victims'
zero_test_result <- zero.test(df$total_victims)
p_val <- zero_test_result$p_value

cat("\n--- Interpretation of zero.test result ---\n")
if (p_val < 0.05) {
  cat("Reject H0: Evidence of zero inflation beyond the Poisson model.\n")
  cat("Consider zero-inflated models (ZIP or ZINB).\n")
} else {
  cat("Fail to reject H0: Poisson model may be adequate.\n")
}

# 7. Fit a Poisson regression model
# Poisson
poisson_model <- glm(total_victims ~ road_surface + direction_of_travel + lighting + weather_1 + party_sex + party_age + party_sobriety + financial_responsibility + 
cellphone_in_use + party_race +  + party_count + hit_and_run + car_age + season + year + region + time_of_day + chp_beat_type + chp_vehicle_type_at_fault + party_safety_equipment_1 
+   type_of_collision + county_location + population + motor_vehicle_involved_with + movement_preceding_collision, family = "poisson", data = df)

# Negative Binomial
nb_model <- glm.nb(total_victims ~ road_surface + direction_of_travel + lighting + weather_1 + party_sex + party_age + party_sobriety + financial_responsibility + 
cellphone_in_use + party_race +  + party_count + hit_and_run + car_age + season + year + region + time_of_day + chp_beat_type + chp_vehicle_type_at_fault + party_safety_equipment_1 
+ type_of_collision + county_location + population + motor_vehicle_involved_with + movement_preceding_collision, data = df)

# ZIP
zip_model <- zeroinfl(total_victims ~ road_surface + direction_of_travel + lighting + weather_1 + party_sex + party_age + party_sobriety + financial_responsibility + 
cellphone_in_use + party_race +  + party_count + hit_and_run + car_age + season + year + region + time_of_day + chp_beat_type + chp_vehicle_type_at_fault + party_safety_equipment_1 
+ type_of_collision + county_location + population + motor_vehicle_involved_with + movement_preceding_collision, data = df, dist = "poisson")

# ZINB
zinb_model <- zeroinfl(total_victims ~ road_surface + direction_of_travel + lighting + weather_1 + party_sex + party_age + party_sobriety + financial_responsibility 
+ cellphone_in_use + party_race + party_count + hit_and_run + car_age + season + year + region + time_of_day + chp_beat_type + chp_vehicle_type_at_fault + party_safety_equipment_1 
+ type_of_collision + county_location + population + motor_vehicle_involved_with + movement_preceding_collision, data = df, dist = "negbin")

# 11. Collect all models in a list for comparison
model_list <- list(
  Poisson = poisson_model,
  NegBin = nb_model,
  ZIP = zip_model,
  ZINB = zinb_model
)

# 12. Create a data frame summarizing AIC and log-likelihood for each model
model_comparison <- data.frame(
  Model = names(model_list),
  AIC = sapply(model_list, AIC),
  LogLik = sapply(model_list, function(m) as.numeric(logLik(m)))
)

# 13. Sort the comparison table by AIC (lower is better)
model_comparison <- model_comparison[order(model_comparison$AIC), ]


cat("Poisson vs Negative Binomial:\n")
lr_test(poisson_model, nb_model)

cat("Poisson vs Zero-Inflated Poisson:\n")
lr_test(poisson_model, zip_model)

cat("Negative Binomial vs Zero-Inflated Negative Binomial:\n")
lr_test(nb_model, zinb_model)

cat("Zero-Inflated Poisson vs Zero-Inflated Negative Binomial:\n")
lr_test(zip_model, zinb_model)

# 14. Print the comparison results
print(model_comparison)
