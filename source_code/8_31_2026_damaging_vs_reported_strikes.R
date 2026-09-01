# Compare reported wildlife strikes with reported damaging strikes.

library(readxl)
library(dplyr)
library(ggplot2)

data_file <- "../data/raw/faa_wildstrike.xlsx"
graph_dir <- "../graphs/8-31-2026"
dir.create(graph_dir, showWarnings = FALSE)

faa_raw <- read_excel(data_file, guess_max = Inf)

yearly_strikes <- faa_raw %>%
  mutate(
    INCIDENT_YEAR = as.integer(INCIDENT_YEAR),
    DAMAGE = toupper(trimws(as.character(INDICATED_DAMAGE))) %in%
      c("TRUE", "T", "YES", "Y", "1")
  ) %>%
  filter(!is.na(INCIDENT_YEAR), INCIDENT_YEAR < 2026) %>%
  group_by(INCIDENT_YEAR) %>%
  summarise(
    reported_strikes = n(),
    damaging_strikes = sum(DAMAGE, na.rm = TRUE),
    .groups = "drop"
  )

p1 <- ggplot(yearly_strikes, aes(INCIDENT_YEAR)) +
  geom_col(aes(y = reported_strikes, fill = "Reported strikes")) +
  geom_col(aes(y = damaging_strikes, fill = "Reported damaging strikes")) +
  scale_fill_manual(values = c(
    "Reported strikes" = "lightsteelblue",
    "Reported damaging strikes" = "firebrick"
  )) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Reported Wildlife Strikes and Damaging Strikes by Year",
    subtitle = "Damaging strikes are a subset of all reported strikes",
    x = "Year",
    y = "Number of Strikes",
    fill = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(yearly_strikes)
print(p1)

ggsave(
  file.path(graph_dir, "damaging strikes vs reported strikes.png"),
  p1,
  width = 10,
  height = 6,
  dpi = 300
)

damaging_strikes_by_species <- faa_raw %>%
  mutate(
    DAMAGE = toupper(trimws(as.character(INDICATED_DAMAGE))) %in%
      c("TRUE", "T", "YES", "Y", "1")
  ) %>%
  filter(DAMAGE, !is.na(SPECIES)) %>%
  count(SPECIES, name = "damaging_strikes", sort = TRUE) %>%
  slice_head(n = 15)

p2 <- ggplot(
  damaging_strikes_by_species,
  aes(x = damaging_strikes, y = reorder(SPECIES, damaging_strikes))
) +
  geom_col(fill = "firebrick") +
  scale_x_continuous(labels = scales::comma) +
  labs(
    title = "Species with the Most Damaging Strikes",
    subtitle = "Top 15 species by number of reported damaging strikes",
    x = "Number of Damaging Strikes",
    y = "Species"
  ) +
  theme_minimal()

print(damaging_strikes_by_species)
print(p2)

ggsave(
  file.path(graph_dir, "damaging strikes by species.png"),
  p2,
  width = 10,
  height = 7,
  dpi = 300
)
