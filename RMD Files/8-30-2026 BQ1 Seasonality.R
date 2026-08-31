# BQ1: Do strikes cluster seasonally, and does the pattern differ by species or region?

library(readxl)
library(tidyverse)

data_file <- "C:/Users/L.Admin/Documents/ISE 687 Introduction to Data Science/Homework/Project/R Files/raw/faa_wildstrike.xlsx"

cat("Reading data...\n")
faa_raw <- read_excel(data_file, guess_max = Inf)

selected_columns <- c(
  "INDEX_NR", "INCIDENT_DATE", "INCIDENT_MONTH", "INCIDENT_YEAR",
  "STATE", "FAAREGION", "SPECIES",
  "TIME_OF_DAY", "PHASE_OF_FLIGHT"
)

faa <- faa_raw %>%
  select(all_of(selected_columns)) %>%
  mutate(
    INCIDENT_DATE = as.Date(INCIDENT_DATE),
    INCIDENT_MONTH = as.integer(INCIDENT_MONTH),
    INCIDENT_YEAR = as.integer(INCIDENT_YEAR),
    MONTH_NAME = factor(month.abb[INCIDENT_MONTH], levels = month.abb),
    SEASON = factor(
      case_when(
        INCIDENT_MONTH %in% c(12, 1, 2) ~ "Winter",
        INCIDENT_MONTH %in% 3:5 ~ "Spring",
        INCIDENT_MONTH %in% 6:8 ~ "Summer",
        INCIDENT_MONTH %in% 9:11 ~ "Fall"
      ),
      levels = c("Winter", "Spring", "Summer", "Fall")
    ),
    MIGRATION_PERIOD = factor(
      case_when(
        INCIDENT_MONTH %in% 3:5 ~ "Spring migration",
        INCIDENT_MONTH %in% 6:7 ~ "Summer/breeding",
        INCIDENT_MONTH %in% 8:11 ~ "Fall migration",
        INCIDENT_MONTH %in% c(12, 1, 2) ~ "Winter"
      ),
      levels = c("Winter", "Spring migration", "Summer/breeding", "Fall migration")
    )
  ) %>%
  filter(!is.na(INCIDENT_MONTH), INCIDENT_MONTH %in% 1:12)

# Keep the eight most common species and group the rest.
faa$SPECIES <- trimws(as.character(faa$SPECIES))

top_species <- faa %>%
  filter(!is.na(SPECIES), SPECIES != "") %>%
  count(SPECIES, sort = TRUE) %>%
  slice_head(n = 8) %>%
  pull(SPECIES)

faa <- faa %>%
  mutate(
    SPECIES_GROUP = case_when(
      SPECIES %in% top_species ~ SPECIES,
      TRUE ~ "Other species"
    ),
    SPECIES_GROUP = factor(SPECIES_GROUP)
  )

# Use years containing all 12 months for across-year seasonal comparisons.
year_coverage <- faa %>%
  group_by(INCIDENT_YEAR) %>%
  summarise(
    months_reported = n_distinct(INCIDENT_MONTH),
    first_date = min(INCIDENT_DATE, na.rm = TRUE),
    last_date = max(INCIDENT_DATE, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(complete_year = months_reported == 12)

complete_years <- year_coverage %>%
  filter(complete_year) %>%
  pull(INCIDENT_YEAR)

faa_complete <- faa %>%
  filter(INCIDENT_YEAR %in% complete_years)

cat("Raw:", nrow(faa_raw), "rows x", ncol(faa_raw), "columns\n")
cat("Analysis rows:", nrow(faa), "\n")
cat("Year range:", min(faa$INCIDENT_YEAR), "-", max(faa$INCIDENT_YEAR), "\n")
cat("Complete years:", min(complete_years), "-", max(complete_years), "\n\n")
print(year_coverage)

# Overall month, season, and migration-period summaries.
month_tbl <- faa_complete %>%
  count(MONTH_NAME, name = "strikes") %>%
  mutate(pct = 100 * strikes / sum(strikes))

season_tbl <- faa_complete %>%
  count(SEASON, name = "strikes") %>%
  mutate(pct = 100 * strikes / sum(strikes))

migration_tbl <- faa_complete %>%
  count(MIGRATION_PERIOD, name = "strikes") %>%
  mutate(pct = 100 * strikes / sum(strikes))

print(month_tbl)
print(season_tbl)
print(migration_tbl)

cat("Peak month:", as.character(month_tbl$MONTH_NAME[which.max(month_tbl$strikes)]), "\n")
cat("Peak-to-trough ratio:", round(max(month_tbl$strikes) / min(month_tbl$strikes), 2), "\n\n")

p1 <- ggplot(month_tbl, aes(MONTH_NAME, strikes)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Wildlife Strikes by Month",
    subtitle = "Complete years only",
    x = "Month", y = "Number of Strikes"
  ) +
  theme_minimal()

p2 <- ggplot(season_tbl, aes(SEASON, strikes)) +
  geom_col(fill = "darkorange") +
  geom_text(aes(label = scales::comma(strikes)), vjust = -0.4, size = 3.5) +
  labs(title = "Wildlife Strikes by Season", x = "Season", y = "Number of Strikes") +
  theme_minimal()

p3 <- ggplot(migration_tbl, aes(MIGRATION_PERIOD, pct)) +
  geom_col(fill = "mediumpurple4") +
  labs(
    title = "Wildlife Strikes by Migration Period",
    subtitle = "Migration periods are month-based approximations",
    x = NULL, y = "% of Strikes"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

print(p1)
print(p2)
print(p3)

# Year-by-month heatmap.
year_month_tbl <- faa %>%
  count(INCIDENT_YEAR, MONTH_NAME, name = "strikes")

p4 <- ggplot(year_month_tbl, aes(MONTH_NAME, factor(INCIDENT_YEAR), fill = strikes)) +
  geom_tile(color = "white", linewidth = 0.1) +
  scale_fill_viridis_c(labels = scales::comma) +
  labs(
    title = "Wildlife Strikes by Month and Year",
    subtitle = "Partial years are visible and should not be compared as annual totals",
    x = "Month", y = "Year", fill = "Strikes"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 6))

print(p4)

# Regional patterns are normalized within each region.
region_tbl <- faa_complete %>%
  filter(!is.na(FAAREGION), trimws(FAAREGION) != "") %>%
  count(FAAREGION, MONTH_NAME, name = "strikes") %>%
  group_by(FAAREGION) %>%
  mutate(pct_of_region = 100 * strikes / sum(strikes)) %>%
  ungroup()

p5 <- ggplot(region_tbl, aes(MONTH_NAME, pct_of_region, group = 1)) +
  geom_line(color = "seagreen4", linewidth = 0.8) +
  geom_point(color = "seagreen4", size = 1) +
  facet_wrap(~ FAAREGION) +
  labs(
    title = "Seasonal Pattern by FAA Region",
    subtitle = "Percent of each region's strikes; missing regions excluded",
    x = "Month", y = "% of Region's Strikes"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, size = 6))

print(p5)

# Seasonal patterns for the most common species.
species_tbl <- faa_complete %>%
  count(SPECIES_GROUP, MONTH_NAME, name = "strikes") %>%
  group_by(SPECIES_GROUP) %>%
  mutate(pct_of_species = 100 * strikes / sum(strikes)) %>%
  ungroup()

p6 <- ggplot(species_tbl, aes(MONTH_NAME, pct_of_species, group = 1)) +
  geom_line(color = "firebrick", linewidth = 0.8) +
  geom_point(color = "firebrick", size = 1) +
  facet_wrap(~ SPECIES_GROUP) +
  labs(
    title = "Seasonal Pattern by Species Group",
    subtitle = "Eight most common species; remaining species grouped as Other",
    x = "Month", y = "% of Group's Strikes"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, size = 6))

print(p6)

# Operational and environmental seasonal patterns.
time_tbl <- faa_complete %>%
  filter(!is.na(TIME_OF_DAY), trimws(as.character(TIME_OF_DAY)) != "") %>%
  count(TIME_OF_DAY, MONTH_NAME, name = "strikes") %>%
  group_by(TIME_OF_DAY) %>%
  mutate(pct_of_time = 100 * strikes / sum(strikes)) %>%
  ungroup()

p7 <- ggplot(time_tbl, aes(MONTH_NAME, pct_of_time, color = TIME_OF_DAY, group = TIME_OF_DAY)) +
  geom_line(linewidth = 0.8) +
  labs(title = "Seasonal Pattern by Time of Day", x = "Month", y = "% of Time-of-Day Group") +
  theme_minimal()
print(p7)

phase_tbl <- faa_complete %>%
  filter(!is.na(PHASE_OF_FLIGHT), trimws(as.character(PHASE_OF_FLIGHT)) != "") %>%
  mutate(PHASE_OF_FLIGHT = fct_lump_n(factor(PHASE_OF_FLIGHT), n = 8)) %>%
  count(PHASE_OF_FLIGHT, MONTH_NAME, name = "strikes") %>%
  group_by(PHASE_OF_FLIGHT) %>%
  mutate(pct_of_phase = 100 * strikes / sum(strikes)) %>%
  ungroup()

p8 <- ggplot(phase_tbl, aes(MONTH_NAME, pct_of_phase, group = 1)) +
  geom_line(color = "navy", linewidth = 0.8) +
  facet_wrap(~ PHASE_OF_FLIGHT) +
  labs(title = "Seasonal Pattern by Phase of Flight", x = "Month", y = "% of Phase's Strikes") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, size = 6))
print(p8)

# Raw counts describe reported strikes, not per-flight risk.
saveRDS(faa, "faa_bq1_seasonality.rds")
cat("\nSaved faa_bq1_seasonality.rds\n")
