# BQ1: Do strikes cluster seasonally, and does the pattern differ by species or region?

library(readxl)
library(tidyverse)

chart_dir <- "../graphs/8-29-2026"
dir.create(chart_dir, showWarnings = FALSE)

faa_raw <- read_excel("../data/raw/faa_wildstrike.xlsx", guess_max = Inf)

cat("Raw:", nrow(faa_raw), "rows x", ncol(faa_raw), "columns\n")
cat("Years:", min(faa_raw$INCIDENT_YEAR), "-", max(faa_raw$INCIDENT_YEAR), "\n\n")

faa <- faa_raw %>%
  select(INDEX_NR, INCIDENT_DATE, INCIDENT_MONTH, INCIDENT_YEAR,
         STATE, FAAREGION, SPECIES, SIZE, INDICATED_DAMAGE)

summary(faa)
head(faa)

season_lookup <- c("Winter","Winter","Spring","Spring","Spring","Summer",
                   "Summer","Summer","Fall","Fall","Fall","Winter")

faa <- faa %>%
  mutate(
    INCIDENT_DATE = as.Date(INCIDENT_DATE),
    MONTH_NAME = factor(month.abb[INCIDENT_MONTH], month.abb),
    SEASON = factor(season_lookup[INCIDENT_MONTH],
                    c("Winter","Spring","Summer","Fall")),
    SPECIES_IDENTIFIED = !grepl("^Unknown", SPECIES)
  )

cat("Missing INCIDENT_MONTH:", sum(is.na(faa$INCIDENT_MONTH)), "\n")
cat("Missing FAAREGION:", sum(is.na(faa$FAAREGION)),
    sprintf("(%.1f%%)\n", 100 * mean(is.na(faa$FAAREGION))))
cat("Species identified:", sum(faa$SPECIES_IDENTIFIED),
    sprintf("(%.1f%%)\n\n", 100 * mean(faa$SPECIES_IDENTIFIED)))

# Keep the eight most common identified species; group the rest as "Other."
top_species <- names(sort(table(faa$SPECIES[faa$SPECIES_IDENTIFIED]),
                          decreasing = TRUE))[1:8]
faa$SPECIES_GROUP <- factor(case_when(
  !faa$SPECIES_IDENTIFIED ~ NA_character_,
  faa$SPECIES %in% top_species ~ faa$SPECIES,
  TRUE ~ "Other"
))

cat("--- STRIKES BY MONTH ---\n")
month_tbl <- faa %>%
  group_by(MONTH_NAME) %>%
  summarise(strikes = n(),
            pct     = round(100 * n() / nrow(faa), 1))
print(as.data.frame(month_tbl))

cat("\n--- STRIKES BY SEASON ---\n")
season_tbl <- faa %>%
  group_by(SEASON) %>%
  summarise(strikes = n(),
            pct     = round(100 * n() / nrow(faa), 1))
print(as.data.frame(season_tbl))

cat("\nPeak month:", as.character(month_tbl$MONTH_NAME[which.max(month_tbl$strikes)]),
    "| Lowest month:", as.character(month_tbl$MONTH_NAME[which.min(month_tbl$strikes)]), "\n")
cat("Peak-to-trough ratio:",
    round(max(month_tbl$strikes) / min(month_tbl$strikes), 1), "\n")

p1 <- ggplot(month_tbl, aes(MONTH_NAME, strikes)) +
  geom_col(fill = "steelblue") +
  labs(title = "Wildlife Strikes by Month", subtitle = "All years combined",
       x = "Month", y = "Number of Strikes") +
  theme_minimal()
print(p1)

p2 <- ggplot(season_tbl, aes(SEASON, strikes)) +
  geom_col(fill = "darkorange") +
  geom_text(aes(label = strikes), vjust = -0.5, size = 3.5) +
  labs(title = "Wildlife Strikes by Season", x = "Season", y = "Number of Strikes") +
  theme_minimal()
print(p2)

region_tbl <- faa %>%
  filter(!is.na(FAAREGION)) %>%
  count(FAAREGION, MONTH_NAME, name = "strikes") %>%
  group_by(FAAREGION) %>%
  mutate(pct = 100 * strikes / sum(strikes)) %>%
  ungroup()

p3 <- ggplot(region_tbl, aes(MONTH_NAME, pct)) +
  geom_col(fill = "seagreen") +
  facet_wrap(~ FAAREGION) +
  labs(title = "Seasonal Pattern by FAA Region",
       subtitle = "Percent of each region's own strikes (excludes unknown-airport records)",
       x = "Month", y = "% of Region's Strikes") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, size = 6))
print(p3)

species_tbl <- faa %>%
  filter(SPECIES_IDENTIFIED) %>%
  count(SPECIES_GROUP, MONTH_NAME, name = "strikes") %>%
  group_by(SPECIES_GROUP) %>%
  mutate(pct = 100 * strikes / sum(strikes)) %>%
  ungroup()

p4 <- ggplot(species_tbl, aes(MONTH_NAME, pct)) +
  geom_col(fill = "firebrick") +
  facet_wrap(~ SPECIES_GROUP) +
  labs(title = "Seasonal Pattern by Species",
       subtitle = "Percent of each species' own strikes (identified species only)",
       x = "Month", y = "% of Species' Strikes") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, size = 6))
print(p4)

year_tbl <- count(faa, INCIDENT_YEAR, name = "strikes")

p5 <- ggplot(year_tbl, aes(INCIDENT_YEAR, strikes)) +
  geom_col(fill = "grey40") +
  labs(title = "Wildlife Strikes by Year", x = "Year", y = "Number of Strikes") +
  theme_minimal()
print(p5)

ggsave(file.path(chart_dir, "8-29 strikes by month.png"), p1, width = 8, height = 5, dpi = 300)
ggsave(file.path(chart_dir, "8-29 strikes by season.png"), p2, width = 8, height = 5, dpi = 300)
ggsave(file.path(chart_dir, "8-29 seasonal pattern by FAA region.png"), p3, width = 12, height = 8, dpi = 300)
ggsave(file.path(chart_dir, "8-29 seasonal pattern by species.png"), p4, width = 14, height = 9, dpi = 300)
ggsave(file.path(chart_dir, "8-29 strikes by year.png"), p5, width = 9, height = 5, dpi = 300)

saveRDS(faa, "faa_bq1.rds")
