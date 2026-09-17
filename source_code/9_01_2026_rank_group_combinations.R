# Rank wildlife-group, flight-phase, and time-of-day combinations.

library(dplyr)
library(ggplot2)

csv_output_dir <- "data/data_output/damage-combinations-2/csv"
rds_output_dir <- "data/data_output/damage-combinations-2/rds"
graph_dir <- "graphs/damage_combos_2"
dir.create(csv_output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(rds_output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(graph_dir, recursive = TRUE, showWarnings = FALSE)

minimum_reports <- 30
minimum_damaging_strikes <- 3

data <- readRDS(
  file.path(rds_output_dir, "prepared_group_damage_combinations.rds")
)

combination_counts <- data %>%
  group_by(SPECIES_GROUP, PHASE_OF_FLIGHT, TIME_OF_DAY) %>%
  summarise(
    reported_strikes = n(),
    damaging_strikes = sum(DAMAGE),
    damage_rate = mean(DAMAGE),
    .groups = "drop"
  ) %>%
  arrange(desc(damage_rate), desc(reported_strikes))

ranked_combinations <- combination_counts %>%
  filter(
    reported_strikes >= minimum_reports,
    damaging_strikes >= minimum_damaging_strikes
  ) %>%
  arrange(desc(damage_rate), desc(reported_strikes)) %>%
  mutate(rank = row_number()) %>%
  select(
    rank, SPECIES_GROUP, PHASE_OF_FLIGHT, TIME_OF_DAY,
    reported_strikes, damaging_strikes, damage_rate
  )

write.csv(
  combination_counts,
  file.path(csv_output_dir, "group_combination_counts_all.csv"),
  row.names = FALSE
)
write.csv(
  ranked_combinations,
  file.path(csv_output_dir, "ranked_group_damage_combinations.csv"),
  row.names = FALSE
)

top_combinations <- ranked_combinations %>%
  slice_head(n = 20) %>%
  mutate(
    combination = paste(
      SPECIES_GROUP, PHASE_OF_FLIGHT, TIME_OF_DAY, sep = " | "
    ),
    combination = reorder(combination, damage_rate)
  )

top_plot <- ggplot(
  top_combinations,
  aes(x = damage_rate, y = combination)
) +
  geom_point(aes(size = reported_strikes), color = "#B22222") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Highest reported damage rates by wildlife group",
    subtitle = paste(
      "Minimum", minimum_reports, "reports and",
      minimum_damaging_strikes, "damaging strikes"
    ),
    x = "Observed damage rate",
    y = "Wildlife group | Phase of flight | Time of day",
    size = "Reported strikes",
    caption = "FAA voluntary wildlife-strike reports through 2025; not per-flight risk"
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.major.y = element_blank())

ggsave(
  file.path(graph_dir, "top_group_damage_combinations.png"),
  top_plot,
  width = 12,
  height = 9,
  dpi = 300
)

heatmap_plot <- ggplot(
  ranked_combinations,
  aes(x = SPECIES_GROUP, y = PHASE_OF_FLIGHT, fill = damage_rate)
) +
  geom_tile(color = "white", linewidth = 0.25) +
  facet_wrap(~ TIME_OF_DAY) +
  scale_fill_gradient(
    low = "#FFF5F0",
    high = "#99000D",
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    title = "Reported damage rate by wildlife group and strike conditions",
    subtitle = paste(
      "Only combinations with at least", minimum_reports,
      "reports and", minimum_damaging_strikes, "damaging strikes"
    ),
    x = "Wildlife group",
    y = "Phase of flight",
    fill = "Damage rate",
    caption = "FAA voluntary wildlife-strike reports through 2025; not per-flight risk"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 40, hjust = 1),
    panel.grid = element_blank()
  )

ggsave(
  file.path(graph_dir, "group_damage_rate_heatmap.png"),
  heatmap_plot,
  width = 14,
  height = 10,
  dpi = 300
)

cat("Saved", nrow(ranked_combinations), "eligible ranked combinations.\n")
