library(dplyr)
library(tidyr)
library(knitr)

# Load dataset
df <- read.csv("driver_at_fault_sample.csv", stringsAsFactors = FALSE)

# Helper function: count and percent of missing or empty values
null_stats <- function(x) {
  total <- length(x)
  nulls <- sum(is.na(x) | trimws(x) == "")
  percent <- round(nulls / total * 100, 2)
  c(Count = nulls, Percent = percent)
}

# Compute missing value summary for all columns
null_summary_all <- as.data.frame(t(sapply(df, null_stats)))
null_summary_all$Column <- rownames(null_summary_all)
null_summary_all <- null_summary_all %>%
  select(Column, everything()) %>%
  arrange(desc(Percent))

kable(null_summary_all, caption = "Missing data before column filtering")

# Drop columns with more than 40% missing data
columns_to_keep <- null_summary_all %>%
  filter(Percent <= 40) %>%
  pull(Column)

df_cleaned <- df[, columns_to_keep]

# Save cleaned dataset (columns only)
write.csv(df_cleaned, "driver_at_fault_sample_cleaned.csv", row.names = FALSE)

# Recalculate missing stats after dropping columns
null_summary_cleaned <- as.data.frame(t(sapply(df_cleaned, null_stats)))
null_summary_cleaned$Column <- rownames(null_summary_cleaned)
null_summary_cleaned <- null_summary_cleaned %>%
  select(Column, everything()) %>%
  arrange(desc(Percent))

kable(null_summary_cleaned, caption = "Missing data after dropping columns")

# Drop rows with any missing or empty values
df_final <- df_cleaned %>%
  filter(across(everything(), ~ !(is.na(.) | trimws(.) == "")))

# Save final dataset
write.csv(df_final, "driver_at_fault_sample_cleaned_no_na.csv", row.names = FALSE)

# Print final row count
cat("Number of rows after full cleanup:", nrow(df_final), "\n")
