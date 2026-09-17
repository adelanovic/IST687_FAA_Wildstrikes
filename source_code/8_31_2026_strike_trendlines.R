# Reported strikes and damaging strikes on one trend chart.

library(readxl)
library(tidyverse)

graph_dir <- "../graphs/8-30-2026"
dir.create(graph_dir, showWarnings = FALSE, recursive = TRUE)

faa_raw <- read_excel("../data/raw/faa_wildstrike.xlsx", guess_max = Inf)

yearly <- faa_raw %>%
  filter(!is.na(INCIDENT_YEAR), INCIDENT_YEAR < 2026) %>%
  group_by(INCIDENT_YEAR = as.integer(INCIDENT_YEAR)) %>%
  summarise(`Reported strikes` = n(),
            `Damaging strikes` = sum(INDICATED_DAMAGE == 1),
            .groups = "drop") %>%
  pivot_longer(-INCIDENT_YEAR, names_to = "SERIES", values_to = "STRIKES") %>%
  mutate(SERIES = factor(SERIES, c("Reported strikes", "Damaging strikes")))

print(pivot_wider(yearly, names_from = SERIES, values_from = STRIKES))

trend_plot <- ggplot(yearly, aes(INCIDENT_YEAR, STRIKES, color = SERIES)) +
  geom_line(linewidth = 0.4, alpha = 0.5) +
  geom_point(size = 1.8, alpha = 0.8) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE,
              linetype = "dashed", linewidth = 0.9) +
  scale_color_manual(values = c("Reported strikes" = "steelblue",
                                "Damaging strikes" = "firebrick")) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Reported and Damaging Wildlife Strikes by Year",
       x = "Year", y = "Number of Strikes", color = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())

print(trend_plot)

ggsave(file.path(graph_dir, "8-30 reported vs damaging strike trend.png"),
       trend_plot, width = 10, height = 6, dpi = 300)
