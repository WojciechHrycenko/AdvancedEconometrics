library(DBI)
library(RSQLite)
library(dplyr)
library(tidyr)
library(knitr)

# Connect to SQLite database
conn <- dbConnect(RSQLite::SQLite(), dbname = "switrs.sqlite")

# Query drivers who were at fault and have complete information
parties_query <- "
  SELECT *
  FROM parties
  WHERE party_type = 'driver'
    AND at_fault = 1
    AND TRIM(party_age) != ''
    AND TRIM(party_sex) != ''
    AND TRIM(party_race) != ''
    AND TRIM(cellphone_in_use) != ''
    AND TRIM(financial_responsibility) != ''
    AND TRIM(party_sobriety) != ''
    AND TRIM(vehicle_make) != ''
    AND TRIM(vehicle_year) != ''
"

parties_filtered <- dbGetQuery(conn, parties_query)

# Sample up to 30,000 drivers
set.seed(123)
parties_sample_raw <- parties_filtered %>%
  sample_n(size = min(100000, nrow(.)))

# Keep one driver per case_id
parties_sample <- parties_sample_raw %>%
  group_by(case_id) %>%
  slice_sample(n = 1) %>%
  ungroup()

# Get matching collisions data
case_ids <- paste0("'", parties_sample$case_id, "'", collapse = ", ")
collisions_query <- paste0("
  SELECT *
  FROM collisions
  WHERE case_id IN (", case_ids, ")
")

collisions_subset <- dbGetQuery(conn, collisions_query)

# Merge datasets
df <- parties_sample %>%
  left_join(collisions_subset, by = "case_id")

dbDisconnect(conn)

# Preview merged dataset
kable(head(df), caption = "Sample of merged dataset", digits = 2)

# Save to CSV
write.csv(df, "driver_at_fault_sample.csv", row.names = FALSE)
