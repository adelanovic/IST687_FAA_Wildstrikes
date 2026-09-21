# Use cleaned wildlife groups, reported flight-phase groups and filled time of day.
library(dplyr)

csv_output_dir <- "data/data_output/damage-combinations-2/csv"
rds_output_dir <- "data/data_output/damage-combinations-2/rds"
dir.create(csv_output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(rds_output_dir, recursive = TRUE, showWarnings = FALSE)

prepared <- readRDS("data/clean/faa_strikes_clean.rds") %>%
  filter(between(INCIDENT_YEAR, 1990, 2025)) %>%
  transmute(INCIDENT_YEAR, SPECIES, SPECIES_GROUP, PHASE_GROUP,
            TIME_OF_DAY = TIME_OF_DAY_FILLED,
            DAMAGE = if_else(INDICATED_DAMAGE %in% c(0, 1),
                             INDICATED_DAMAGE, NA_real_))

analysis_data <- prepared %>%
  filter(!is.na(SPECIES_GROUP), SPECIES_GROUP != "Needs review",
         !is.na(PHASE_GROUP), !is.na(TIME_OF_DAY), !is.na(DAMAGE))

quality_summary <- data.frame(
  measure = c("Reports before completeness filtering",
              "Missing or unreviewed group", "Missing reported phase group",
              "Missing filled time of day", "Missing damage outcome",
              "Complete reports used"),
  reports = c(nrow(prepared),
              sum(is.na(prepared$SPECIES_GROUP) | prepared$SPECIES_GROUP == "Needs review"),
              sum(is.na(prepared$PHASE_GROUP)), sum(is.na(prepared$TIME_OF_DAY)),
              sum(is.na(prepared$DAMAGE)), nrow(analysis_data))
)
saveRDS(analysis_data, file.path(rds_output_dir, "prepared_group_damage_combinations.rds"))
write.csv(quality_summary, file.path(csv_output_dir, "data_quality_summary.csv"), row.names = FALSE)
cat("Prepared", nrow(analysis_data), "complete reports.\n")
