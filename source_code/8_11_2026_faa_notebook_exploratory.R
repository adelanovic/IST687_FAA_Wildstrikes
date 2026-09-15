# FAA Wildlife Strike exploratory analysis

library(tidyverse)
library(readxl)

faaWildStrike <- read_excel("../data/raw/faa_wildstrike.xlsx", guess_max = Inf)

head(faaWildStrike)
length(faaWildStrike)
nrow(faaWildStrike)

data_dict <- read_excel("../data/raw/faa_wildstrike_data_dictionary.xls",
                        sheet = "Column Name") |>
  set_names(c("column", "description")) |>
  filter(!is.na(column))

data_dict

by_year <- faaWildStrike |>
  count(INCIDENT_YEAR, name = "STRIKES") |>
  mutate(INCIDENT_YEAR = as.integer(INCIDENT_YEAR)) |>
  filter(INCIDENT_YEAR < 2026)

p1 <- ggplot(by_year, aes(INCIDENT_YEAR, STRIKES)) +
  geom_line() +
  geom_point(shape = 21, fill = "white", size = 3) +
  theme_bw() +
  labs(title = "Aircraft Wildlife Strikes (1990-2025)", x = "Year", y = "Strikes")

p2 <- p1 +
  annotate("rect", xmin = 2020, xmax = 2021, ymin = -Inf, ymax = Inf,
           fill = "red", alpha = 0.10) +
  annotate("text", x = 2020, y = -Inf, label = "COVID-19",
           hjust = -0.1, vjust = -1, size = 3.5, color = "grey30")

print(p1)
print(p2)
