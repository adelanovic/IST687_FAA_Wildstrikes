# Rank species, flight-phase, and time-of-day combinations by damage risk.

library(dplyr)
library(ggplot2)

csv_output_dir <- "data/data_output/damage-combinations/csv"
rds_output_dir <- "data/data_output/damage-combinations/rds"
graph_dir <- "graphs/damage_combos"
dir.create(csv_output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(rds_output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(graph_dir, recursive = TRUE, showWarnings = FALSE)

minimum_reports <- 30
minimum_damaging_strikes <- 3

data <- readRDS(
  file.path(rds_output_dir, "prepared_damage_combinations.rds")
)

combination_counts <- data %>%
  group_by(SPECIES, PHASE_OF_FLIGHT, TIME_OF_DAY) %>%
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
    rank, SPECIES, PHASE_OF_FLIGHT, TIME_OF_DAY,
    reported_strikes, damaging_strikes, damage_rate
  )

write.csv(
  combination_counts,
  file.path(csv_output_dir, "combination_counts_all.csv"),
  row.names = FALSE
)
write.csv(
  ranked_combinations,
  file.path(csv_output_dir, "ranked_damage_combinations.csv"),
  row.names = FALSE
)

top_combinations <- ranked_combinations %>%
  slice_head(n = 20) %>%
  mutate(
    combination = paste(SPECIES, PHASE_OF_FLIGHT, TIME_OF_DAY, sep = " | "),
    combination = reorder(combination, damage_rate)
  )

plot <- ggplot(top_combinations, aes(x = damage_rate, y = combination)) +
  geom_point(aes(size = reported_strikes), color = "#B22222") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Highest reported damage-risk combinations",
    subtitle = paste(
      "Ranked by observed damage rate; minimum",
      minimum_reports, "reports and", minimum_damaging_strikes, "damaging strikes"
    ),
    x = "Observed damage rate",
    y = "Species | Phase of flight | Time of day",
    size = "Reported strikes",
    caption = "FAA voluntary wildlife-strike reports through 2025; not per-flight risk"
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.major.y = element_blank())

ggsave(
  file.path(graph_dir, "top_damage_combinations.png"),
  plot,
  width = 12,
  height = 9,
  dpi = 300
)

cat("Saved", nrow(ranked_combinations), "eligible ranked combinations.\n")
