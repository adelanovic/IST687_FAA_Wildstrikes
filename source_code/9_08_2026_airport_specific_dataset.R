library(tidyverse)

INPUT <- "data/clean/faa_strikes_clean.rds"
OUTPUT_RDS <- "data/clean/airport_specific_dataset.rds"
OUTPUT_XLSX <- "data/clean/airport_specific_dataset.xlsx"

modal_value <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(x)]
  if (!length(x)) NA_character_ else names(which.max(table(x)))
}

# Stop at the last complete reporting year; 2026 ends in August and its
# missing autumn months would understate airports whose strikes peak in fall.
LAST_COMPLETE_YEAR <- 2025L

strikes <- readRDS(INPUT) %>%
  filter(INCIDENT_YEAR <= LAST_COMPLETE_YEAR)
data_end_year <- LAST_COMPLETE_YEAR

airport_specific <- strikes %>%
  filter(AIRPORT_ID != "ZZZZ") %>%
  group_by(AIRPORT_ID) %>%
  summarise(
    AIRPORT = modal_value(AIRPORT),
    STATE = modal_value(STATE),
    FAAREGION = modal_value(FAAREGION),
    LATITUDE = median(LATITUDE, na.rm = TRUE),
    LONGITUDE = median(LONGITUDE, na.rm = TRUE),

    STRIKE_TOTAL = n(),
    DAMAGING_STRIKES = sum(INDICATED_DAMAGE == 1),
    DAMAGE_RATE = mean(INDICATED_DAMAGE == 1),
    RECENT_5_YEAR_STRIKES = sum(INCIDENT_YEAR >= data_end_year - 4L),
    .groups = "drop"
  ) %>%
  arrange(desc(STRIKE_TOTAL), AIRPORT_ID)

dir.create(dirname(OUTPUT_RDS), showWarnings = FALSE, recursive = TRUE)
saveRDS(airport_specific, OUTPUT_RDS)
writexl::write_xlsx(airport_specific, OUTPUT_XLSX)
