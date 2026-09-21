# Full rankings are retained; only the chart displays are shortened.
library(dplyr)
library(tidyr)
library(ggplot2)

csv_output_dir <- "data/data_output/damage-combinations-2/csv"
graph_dir <- "graphs/damage_combos_2"
dir.create(graph_dir, recursive = TRUE, showWarnings = FALSE)
minimum_reports <- 30

data <- readRDS("data/data_output/damage-combinations-2/rds/prepared_group_damage_combinations.rds")
combination_counts <- data %>%
  group_by(SPECIES_GROUP, PHASE_GROUP, TIME_OF_DAY) %>%
  summarise(reported_strikes = n(), damaging_strikes = sum(DAMAGE),
            damage_rate = mean(DAMAGE), .groups = "drop") %>%
  arrange(desc(damage_rate), desc(reported_strikes), SPECIES_GROUP, PHASE_GROUP, TIME_OF_DAY)

# Zero-damage combinations remain eligible.
ranked_combinations <- combination_counts %>%
  filter(reported_strikes >= minimum_reports) %>%
  mutate(rank = row_number()) %>%
  select(rank, everything())

write.csv(combination_counts, file.path(csv_output_dir, "group_combination_counts_all.csv"), row.names = FALSE)
write.csv(ranked_combinations, file.path(csv_output_dir, "ranked_group_damage_combinations.csv"), row.names = FALSE)

top_combinations <- ranked_combinations %>%
  slice_head(n = 10) %>%
  mutate(combination = paste(SPECIES_GROUP, PHASE_GROUP, TIME_OF_DAY, sep = " | "),
         combination = factor(combination, levels = rev(combination)))

top_plot <- ggplot(top_combinations, aes(damage_rate, combination)) +
  geom_col(fill = "steelblue", width = 0.7) +
  geom_text(aes(label = paste0(scales::percent(damage_rate, accuracy = 0.1),
                               " (", damaging_strikes, "/", reported_strikes, ")")),
            hjust = -0.1, size = 4) +
  scale_x_continuous(labels = scales::percent,
                     expand = expansion(mult = c(0, 0.3))) +
  labs(title = "Top 10 Wildlife-Strike Damage Combinations",
       subtitle = "1990-2025 | Minimum 30 reports per combination",
       x = "Reported damage rate", y = NULL) +
  theme_minimal(base_size = 14) +
  theme(panel.grid.major.y = element_blank())

# Select by report volume, not by damage rate. Unknown groups remain eligible.
top_groups <- data %>%
  count(SPECIES_GROUP, sort = TRUE) %>%
  arrange(desc(n), SPECIES_GROUP) %>%
  slice_head(n = 5) %>%
  pull(SPECIES_GROUP)

top_common_combinations <- ranked_combinations %>%
  filter(SPECIES_GROUP %in% top_groups) %>%
  slice_head(n = 10) %>%
  mutate(combination = paste(SPECIES_GROUP, PHASE_GROUP, TIME_OF_DAY, sep = " | "),
         combination = factor(combination, levels = rev(combination)))

# Reuse the bar-chart style with the heatmap's five-group subset.
common_plot <- (top_plot %+% top_common_combinations) +
  labs(title = "Top 10 Damage Combinations: Five Most-Reported Groups")

heatmap_data <- combination_counts %>%
  filter(SPECIES_GROUP %in% top_groups) %>%
  mutate(SPECIES_GROUP = factor(SPECIES_GROUP, levels = top_groups),
         damage_rate = if_else(reported_strikes >= minimum_reports, damage_rate, NA_real_)) %>%
  complete(SPECIES_GROUP, PHASE_GROUP, TIME_OF_DAY)

heatmap_plot <- ggplot(heatmap_data, aes(SPECIES_GROUP, PHASE_GROUP, fill = damage_rate)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = if_else(is.na(damage_rate), "",
                                 scales::percent(damage_rate, accuracy = 1)),
                color = damage_rate > 0.5), size = 4, show.legend = FALSE) +
  scale_color_manual(values = c("FALSE" = "black", "TRUE" = "white"), na.value = "black") +
  facet_wrap(~ TIME_OF_DAY, ncol = 2, drop = FALSE) +
  scale_fill_gradient(low = "#FFF5F0", high = "#99000D", limits = c(0, 1),
                      na.value = "grey90", labels = scales::percent) +
  scale_x_discrete(labels = function(x) gsub(" & ", " &\n", x)) +
  labs(title = "Damage Rates for the Five Most-Reported Wildlife Groups",
       subtitle = "1990-2025 | Minimum 30 reports per cell",
       x = NULL, y = "Flight phase group", fill = "Damage rate",
       caption = "Gray cells: fewer than 30 reports, including no reports.") +
  theme_minimal(base_size = 13) +
  theme(panel.grid = element_blank(), axis.text.x = element_text(size = 10))

ggsave(file.path(graph_dir, "top_group_damage_combinations.png"),
       top_plot, width = 13, height = 7, dpi = 300)
ggsave(file.path(graph_dir, "group_damage_rate_heatmap.png"),
       heatmap_plot, width = 13, height = 8, dpi = 300)
ggsave(file.path(graph_dir, "top_common_group_damage_combinations.png"),
       common_plot, width = 14, height = 7, dpi = 300)
cat("Saved", nrow(ranked_combinations), "eligible combinations.\n")
