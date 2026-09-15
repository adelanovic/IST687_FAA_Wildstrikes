library(readxl)
library(dplyr)
library(ggplot2)

graph_dir <- "../graphs/9-02-2026"
csv_output_dir <- "../data/data_output"
dir.create(graph_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(csv_output_dir, recursive = TRUE, showWarnings = FALSE)

faa_raw <- read_excel("../data/raw/faa_wildstrike.xlsx", guess_max = Inf)

phase_data <- faa_raw %>%
  filter(INCIDENT_YEAR < 2026, !is.na(PHASE_OF_FLIGHT)) %>%
  transmute(
    DAMAGE = as.integer(INDICATED_DAMAGE),
    PHASE_GROUP = factor(
      case_when(
        PHASE_OF_FLIGHT %in% c("Descent", "Approach", "Landing Roll") ~ "Landing phases",
        PHASE_OF_FLIGHT %in% c("Take-off Run", "Climb") ~ "Takeoff phases",
        PHASE_OF_FLIGHT == "En Route" ~ "En route",
        TRUE ~ "Other phases"
      ),
      levels = c("Landing phases", "Takeoff phases", "En route", "Other phases")
    )
  )

phase_summary <- phase_data %>%
  group_by(PHASE_GROUP) %>%
  summarise(
    reported_strikes = n(),
    damaging_strikes = sum(DAMAGE, na.rm = TRUE),
    known_damage_outcomes = sum(!is.na(DAMAGE)),
    damage_rate = damaging_strikes / known_damage_outcomes,
    .groups = "drop"
  ) %>%
  mutate(
    share_of_reported_strikes = reported_strikes / sum(reported_strikes),
    share_percent = 100 * share_of_reported_strikes,
    damage_rate_percent = 100 * damage_rate
  )

print(phase_summary)

write.csv(phase_summary, file.path(csv_output_dir, "phase_grouping_summary.csv"),
          row.names = FALSE)

save_phase_bar <- function(file, column, fill, title, subtitle, y_label) {
  plot <- ggplot(phase_summary, aes(PHASE_GROUP, .data[[column]])) +
    geom_col(fill = fill, width = 0.70) +
    geom_text(aes(label = paste0(round(.data[[column]], 1), "%")), vjust = -0.4) +
    expand_limits(y = max(phase_summary[[column]]) * 1.10) +
    labs(title = title, subtitle = subtitle, x = "Grouped phase of flight",
         y = y_label, caption = "FAA voluntary wildlife-strike reports through 2025") +
    theme_minimal(base_size = 12) +
    theme(panel.grid.major.x = element_blank())
  ggsave(file.path(graph_dir, file), plot, width = 9, height = 6, dpi = 300)
}

save_phase_bar(
  "phase_grouping_share_of_strikes.png", "share_percent", "#2C6E9B",
  "Share of Reported Wildlife Strikes by Flight Phase",
  "Landing includes descent, approach, and landing roll; takeoff includes take-off run and climb",
  "Percent of reports with a known flight phase"
)

save_phase_bar(
  "phase_grouping_damage_rate.png", "damage_rate_percent", "#B22222",
  "Reported Damage Rate by Flight Phase",
  "Damage rate is the percentage of reports within each phase group that indicated damage",
  "Reported damage rate (%)"
)
