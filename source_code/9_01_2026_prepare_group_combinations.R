# Prepare wildlife-group combinations using the reviewed species mapping.

library(readxl)
library(dplyr)

data_file <- "data/raw/faa_wildstrike.xlsx"
mapping_file <- paste0(
  "source_code/species_group_prediction/output/csv/",
  "species_group_mapping.csv"
)
csv_output_dir <- "data/data_output/damage-combinations-2/csv"
rds_output_dir <- "data/data_output/damage-combinations-2/rds"
dir.create(csv_output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(rds_output_dir, recursive = TRUE, showWarnings = FALSE)

faa_raw <- read_excel(data_file, guess_max = Inf)
species_mapping <- read.csv(mapping_file, stringsAsFactors = FALSE) %>%
  mutate(SPECIES_KEY = tolower(trimws(SPECIES))) %>%
  select(SPECIES_KEY, SPECIES_GROUP) %>%
  distinct()

prepared <- faa_raw %>%
  transmute(
    INCIDENT_YEAR = as.integer(INCIDENT_YEAR),
    SPECIES = trimws(as.character(SPECIES)),
    SPECIES_KEY = tolower(SPECIES),
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
  filter(!is.na(INCIDENT_YEAR), INCIDENT_YEAR < 2026) %>%
  left_join(species_mapping, by = "SPECIES_KEY")

quality_summary <- data.frame(
  measure = c(
    "Reports before completeness filtering",
    "Species without a mapped wildlife group",
    "Missing phase of flight",
    "Missing time of day",
    "Missing damage outcome",
    "Complete reports used"
  ),
  reports = c(
    nrow(prepared),
    sum(is.na(prepared$SPECIES_GROUP)),
    sum(is.na(prepared$PHASE_OF_FLIGHT) | prepared$PHASE_OF_FLIGHT == ""),
    sum(is.na(prepared$TIME_OF_DAY) | prepared$TIME_OF_DAY == ""),
    sum(is.na(prepared$DAMAGE)),
    sum(
      !is.na(prepared$SPECIES_GROUP) &
        !is.na(prepared$PHASE_OF_FLIGHT) & prepared$PHASE_OF_FLIGHT != "" &
        !is.na(prepared$TIME_OF_DAY) & prepared$TIME_OF_DAY != "" &
        !is.na(prepared$DAMAGE)
    )
  )
)

analysis_data <- prepared %>%
  filter(
    !is.na(SPECIES_GROUP),
    !is.na(PHASE_OF_FLIGHT), PHASE_OF_FLIGHT != "",
    !is.na(TIME_OF_DAY), TIME_OF_DAY != "",
    !is.na(DAMAGE)
  ) %>%
  select(
    INCIDENT_YEAR, SPECIES, SPECIES_GROUP,
    PHASE_OF_FLIGHT, TIME_OF_DAY, DAMAGE
  ) %>%
  mutate(
    SPECIES_GROUP = factor(SPECIES_GROUP),
    PHASE_OF_FLIGHT = factor(PHASE_OF_FLIGHT),
    TIME_OF_DAY = factor(TIME_OF_DAY)
  )

saveRDS(
  analysis_data,
  file.path(rds_output_dir, "prepared_group_damage_combinations.rds")
)
write.csv(
  quality_summary,
  file.path(csv_output_dir, "data_quality_summary.csv"),
  row.names = FALSE
)

cat("Prepared", nrow(analysis_data), "complete reported strikes.\n")
