# Prepare reported FAA wildlife strikes for the damage-combination analysis.

library(readxl)
library(dplyr)

data_file <- "data/raw/faa_wildstrike.xlsx"
csv_output_dir <- "data/data_output/damage-combinations/csv"
rds_output_dir <- "data/data_output/damage-combinations/rds"
dir.create(csv_output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(rds_output_dir, recursive = TRUE, showWarnings = FALSE)

faa_raw <- read_excel(data_file, guess_max = Inf)

is_unknown_species <- function(species) {
  value <- toupper(trimws(as.character(species)))
  is.na(value) | value == "" | grepl("^(UNKNOWN|UNIDENTIFIED)", value)
}

prepared <- faa_raw %>%
  transmute(
    INCIDENT_YEAR = as.integer(INCIDENT_YEAR),
    SPECIES = trimws(as.character(SPECIES)),
    PHASE_OF_FLIGHT = trimws(as.character(PHASE_OF_FLIGHT)),
    TIME_OF_DAY = trimws(as.character(TIME_OF_DAY)),
    DAMAGE = case_when(
      toupper(trimws(as.character(INDICATED_DAMAGE))) %in%
        c("TRUE", "T", "YES", "Y", "1") ~ 1L,
      toupper(trimws(as.character(INDICATED_DAMAGE))) %in%
        c("FALSE", "F", "NO", "N", "0") ~ 0L,
      TRUE ~ NA_integer_
    )
  ) %>%
  filter(!is.na(INCIDENT_YEAR), INCIDENT_YEAR < 2026)

quality_summary <- data.frame(
  measure = c(
    "Reports before completeness filtering",
    "Missing or unknown species",
    "Missing phase of flight",
    "Missing time of day",
    "Missing damage outcome",
    "Complete reports used"
  ),
  reports = c(
    nrow(prepared),
    sum(is_unknown_species(prepared$SPECIES)),
    sum(is.na(prepared$PHASE_OF_FLIGHT) | prepared$PHASE_OF_FLIGHT == ""),
    sum(is.na(prepared$TIME_OF_DAY) | prepared$TIME_OF_DAY == ""),
    sum(is.na(prepared$DAMAGE)),
    sum(
      !is_unknown_species(prepared$SPECIES) &
        !is.na(prepared$PHASE_OF_FLIGHT) & prepared$PHASE_OF_FLIGHT != "" &
        !is.na(prepared$TIME_OF_DAY) & prepared$TIME_OF_DAY != "" &
        !is.na(prepared$DAMAGE)
    )
  )
)

analysis_data <- prepared %>%
  filter(
    !is_unknown_species(SPECIES),
    !is.na(PHASE_OF_FLIGHT), PHASE_OF_FLIGHT != "",
    !is.na(TIME_OF_DAY), TIME_OF_DAY != "",
    !is.na(DAMAGE)
  ) %>%
  mutate(
    SPECIES = factor(SPECIES),
    PHASE_OF_FLIGHT = factor(PHASE_OF_FLIGHT),
    TIME_OF_DAY = factor(TIME_OF_DAY)
  )

saveRDS(
  analysis_data,
  file.path(rds_output_dir, "prepared_damage_combinations.rds")
)
write.csv(
  quality_summary,
  file.path(csv_output_dir, "data_quality_summary.csv"),
  row.names = FALSE
)

cat("Prepared", nrow(analysis_data), "complete reported strikes.\n")
