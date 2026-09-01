# Explore the columns used in the project business questions.

library(readxl)
library(dplyr)

output_dir <- "../data/data_output"
dir.create(output_dir, showWarnings = FALSE)

faa_raw <- read_excel("../data/raw/faa_wildstrike.xlsx", guess_max = Inf)

faa <- faa_raw %>%
  select(
    INCIDENT_MONTH,
    INCIDENT_YEAR,
    SPECIES,
    STATE,
    FAAREGION,
    AIRPORT_ID,
    INDICATED_DAMAGE,
    SIZE,
    PHASE_OF_FLIGHT,
    TIME_OF_DAY,
    DAMAGE_LEVEL,
    HEIGHT,
    SPEED,
    AC_MASS,
    NUM_ENGS,
    TYPE_ENG,
    NUM_STRUCK
  )

cat("NUMBER OF ROWS AND COLUMNS\n")
print(dim(faa))

dataset_size <- data.frame(
  rows = nrow(faa),
  columns = ncol(faa)
)
write.csv(
  dataset_size,
  file.path(output_dir, "dataset_size.csv"),
  row.names = FALSE
)

cat("\nCOLUMN TYPES\n")
str(faa)

column_types <- data.frame(
  column = names(faa),
  type = sapply(faa, function(column) class(column)[1])
)
write.csv(
  column_types,
  file.path(output_dir, "column_types.csv"),
  row.names = FALSE
)

cat("\nMISSING VALUES\n")
missing_values <- data.frame(
  column = names(faa),
  missing = colSums(is.na(faa)),
  percent_missing = round(100 * colMeans(is.na(faa)), 2)
)
print(missing_values)
write.csv(
  missing_values,
  file.path(output_dir, "missing_values.csv"),
  row.names = FALSE
)

columns_to_count <- c(
  "INCIDENT_MONTH",
  "INCIDENT_YEAR",
  "STATE",
  "FAAREGION",
  "INDICATED_DAMAGE",
  "SIZE",
  "PHASE_OF_FLIGHT",
  "TIME_OF_DAY",
  "DAMAGE_LEVEL",
  "AC_MASS",
  "NUM_ENGS",
  "TYPE_ENG",
  "NUM_STRUCK"
)

for (column in columns_to_count) {
  cat("\n", column, " VALUES\n", sep = "")

  value_counts <- faa %>%
    count(.data[[column]], sort = TRUE)

  print(value_counts, n = Inf)
  write.csv(
    value_counts,
    file.path(output_dir, paste0(tolower(column), "_values.csv")),
    row.names = FALSE
  )
}

cat("\nSPECIES SUMMARY\n")
species_counts <- faa %>%
  filter(!is.na(SPECIES)) %>%
  count(SPECIES, sort = TRUE)

cat("Unique wildlife entries:", nrow(species_counts), "\n")
cat("Top 20 wildlife entries by reported strikes:\n")
print(slice_head(species_counts, n = 20))
write.csv(
  species_counts,
  file.path(output_dir, "species_values.csv"),
  row.names = FALSE
)

cat("\nAIRPORT ID SUMMARY\n")
airport_counts <- faa %>%
  filter(!is.na(AIRPORT_ID)) %>%
  count(AIRPORT_ID, sort = TRUE)

cat("Unique airport IDs:", nrow(airport_counts), "\n")
cat("Top 20 airport IDs by reported strikes:\n")
print(slice_head(airport_counts, n = 20))
write.csv(
  airport_counts,
  file.path(output_dir, "airport_id_values.csv"),
  row.names = FALSE
)

cat("\nHEIGHT SUMMARY\n")
print(summary(faa$HEIGHT))

cat("\nSPEED SUMMARY\n")
print(summary(faa$SPEED))

numeric_summary <- data.frame(
  statistic = names(summary(faa$HEIGHT)),
  height = as.numeric(summary(faa$HEIGHT)),
  speed = as.numeric(summary(faa$SPEED))
)
write.csv(
  numeric_summary,
  file.path(output_dir, "numeric_summary.csv"),
  row.names = FALSE
)

cat("\nSaved exploratory CSV files to", output_dir, "\n")
