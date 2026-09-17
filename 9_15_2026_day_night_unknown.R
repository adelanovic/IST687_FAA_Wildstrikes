library(tidyverse)

strikes <- readRDS("data/clean/faa_strikes_clean.rds")

time_summary <- strikes %>%
  count(TIME_GROUP = factor(replace_na(as.character(TIME_OF_DAY_FILLED), "Unknown"),
                            c("Day", "Night", "Dawn", "Dusk", "Unknown")),
        name = "STRIKES", .drop = FALSE) %>%
  mutate(PERCENT = STRIKES / sum(STRIKES))

time_plot <- ggplot(time_summary, aes(TIME_GROUP, STRIKES, fill = TIME_GROUP)) +
  geom_col(width = 0.65) +
  geom_text(aes(label = paste0(scales::comma(STRIKES), "\n",
                              scales::percent(PERCENT, accuracy = 0.1))),
            vjust = -0.3) +
  scale_fill_manual(values = c(Day = "goldenrod", Night = "steelblue4",
                               Dawn = "lightsalmon", Dusk = "mediumpurple3",
                               Unknown = "gray60")) +
  scale_y_continuous(labels = scales::comma,
                     expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Reported Wildlife Strikes by Time of Day",
       subtitle = "Missing time of day is shown as Unknown",
       x = "Time of day", y = "Reported strikes") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none", panel.grid.major.x = element_blank())

dir.create("graphs/9-15-2026", recursive = TRUE, showWarnings = FALSE)
ggsave("graphs/9-15-2026/day_night_unknown.png", time_plot,
       width = 9, height = 6, dpi = 300)
print(time_summary)
