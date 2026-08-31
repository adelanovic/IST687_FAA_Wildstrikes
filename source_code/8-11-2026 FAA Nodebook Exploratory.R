# FAA Wildlife Strike exploratory analysis

library(tidyverse)
library(readxl)

data_file <- "../raw/faa_wildstrike.xlsx"
dictionary_file <- "../raw/faa_wildstrike_data_dictionary.xls"

faaWildStrike <- read_excel(data_file, guess_max = Inf)

head(faaWildStrike)
length(faaWildStrike)
nrow(faaWildStrike)

data_dict <- read_excel(dictionary_file, sheet = "Column Name") |>
  set_names(c("column", "description")) |>
  filter(!is.na(column))

data_dict

by_year <- count(faaWildStrike, INCIDENT_YEAR, name = "STRIKES")
by_year$INCIDENT_YEAR <- as.integer(by_year$INCIDENT_YEAR)
by_year <- by_year[by_year$INCIDENT_YEAR < 2026, ]

p1 <- ggplot(by_year, aes(x = INCIDENT_YEAR, y = STRIKES)) +
  geom_line() +
  geom_point(shape = 21, fill = "white", size = 3) +
  theme_bw() +
  labs(
    title = "Aircraft Wildlife Strikes (1990-2025)",
    x = "Year",
    y = "Strikes"
  )

p2 <- ggplot(by_year, aes(x = INCIDENT_YEAR, y = STRIKES)) +
  geom_line() +
  geom_point(shape = 21, fill = "white", size = 3) +
  annotate("rect", xmin = 2020, xmax = 2021, ymin = -Inf, ymax = Inf,
           fill = "red", alpha = 0.10) +
  annotate("text", x = 2020, y = max(by_year$strikes), label = "COVID-19",
           hjust = -0.1, vjust = -1, size = 3.5, color = "grey30") +
  theme_bw() +
  labs(
    title = "Aircraft Wildlife Strikes (1990-2025)",
    x = "Year",
    y = "Strikes"
  )

print(p1)
print(p2)
