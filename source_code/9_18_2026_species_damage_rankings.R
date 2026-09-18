# Run from the project root. Rank reported damage frequency, not repair cost.
library(tidyverse)

output_dir <- "data/data_output/species_damage_rankings"
graph_dir <- "graphs/9-18-2026"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(graph_dir, recursive = TRUE, showWarnings = FALSE)
minimum_reports <- 30

# Keep broad wildlife labels separate; match whole names, not partial words.
broad_labels <- c(
  "Bats", "Blackbirds", "Canids", "Cattle", "Chickadees",
  "Cormorants", "Cranes", "Crows", "Cuckoos", "Deer",
  "Diving duck (Aythya)", "Doves", "Drongos", "Ducks", "Eagles",
  "Egrets", "Flying foxes", "Foxes", "Free-tailed bats", "Frigatebirds",
  "Geese", "Grackles", "Grebes", "Ground squirrels", "Grouse",
  "Gulls", "Hares", "Hawks", "Herons", "Hummingbirds",
  "Ibises", "Iguanas", "Kingfishers", "Kites", "Kittiwakes",
  "Lagomorphs (rabbits, hares)", "Lapwings", "Larks", "Loons", "Magpies",
  "Mammals", "Marmots", "Meadowlarks", "Megabats", "Microbats",
  "Mockingbirds", "Munias", "Mynas", "New World quail", "New World vultures",
  "New World wood-warblers", "New world porcupines", "New world rats", "Nightjars", "Noddies",
  "Nuthatches", "Old World vultures", "Old World warblers", "Orioles", "Owls",
  "Oystercatchers", "Parakeets", "Parrots", "Partridges", "Peccaries",
  "Pelicans", "Perching birds (y)", "Perching birds (z)", "Pheasants", "Pigeons",
  "Plovers", "Pocket gophers", "Prairie dogs", "Ptarmigans", "Rabbits",
  "Rails", "Ravens", "Reptiles", "Rodents", "Shearwaters",
  "Shorebirds", "Skunks", "Snakes", "Sparrows", "Squirrels",
  "Storm-petrels", "Swallows", "Swans", "Swifts", "Swine (pigs)",
  "Thrashers", "Thrushes", "Towhees", "Tropicbirds", "Turtles",
  "Typical owls", "Tyrant (New World) flycatchers", "Vesper bats", "Vireos", "Woodpeckers",
  "Wrens"
)

summary_table <- readRDS("data/clean/faa_strikes_clean.rds") %>%
  filter(between(INCIDENT_YEAR, 1990, 2025)) %>%
  mutate(SPECIES = replace_na(na_if(str_squish(as.character(SPECIES)), ""), "Missing species"),
         LABEL_TYPE = case_when(
           SPECIES == "Missing species" |
             str_detect(str_to_lower(SPECIES), "^(unknown|unidentified)") ~ "Unknown/missing",
           SPECIES %in% broad_labels |
             str_detect(str_to_lower(SPECIES), "complex|,|^raptors:") ~ "Broad/ambiguous label",
           TRUE ~ "Named species"
         )) %>%
  group_by(SPECIES, LABEL_TYPE) %>%
  summarise(REPORTED_STRIKES = n(),
            KNOWN_DAMAGE_REPORTS = sum(INDICATED_DAMAGE %in% c(0, 1)),
            DAMAGING_STRIKES = sum(INDICATED_DAMAGE == 1, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(MISSING_DAMAGE_REPORTS = REPORTED_STRIKES - KNOWN_DAMAGE_REPORTS,
         DAMAGE_RATE = DAMAGING_STRIKES / na_if(KNOWN_DAMAGE_REPORTS, 0L))

count_ranking <- summary_table %>%
  filter(LABEL_TYPE == "Named species") %>%
  arrange(desc(DAMAGING_STRIKES), desc(KNOWN_DAMAGE_REPORTS), SPECIES) %>%
  mutate(COUNT_RANK = min_rank(desc(DAMAGING_STRIKES)))

# No minimum damaging-count filter: zero-damage species remain eligible.
rate_ranking <- count_ranking %>%
  filter(KNOWN_DAMAGE_REPORTS >= minimum_reports) %>%
  arrange(desc(DAMAGE_RATE), desc(KNOWN_DAMAGE_REPORTS), SPECIES) %>%
  mutate(RATE_RANK = min_rank(desc(DAMAGE_RATE)))

other_labels <- summary_table %>%
  filter(LABEL_TYPE != "Named species") %>%
  arrange(desc(DAMAGING_STRIKES), SPECIES)

write.csv(summary_table, file.path(output_dir, "all_species_labels.csv"), row.names = FALSE)
write.csv(count_ranking, file.path(output_dir, "species_by_damage_count.csv"), row.names = FALSE)
write.csv(rate_ranking, file.path(output_dir, "species_by_damage_rate.csv"), row.names = FALSE)
write.csv(other_labels, file.path(output_dir, "broad_and_unknown_labels.csv"), row.names = FALSE)

count_plot <- count_ranking %>% head(15) %>%
  ggplot(aes(DAMAGING_STRIKES, reorder(SPECIES, DAMAGING_STRIKES))) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = scales::comma(DAMAGING_STRIKES)), hjust = -0.15, size = 3.5) +
  scale_x_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Named Species with the Most Reported Damaging Strikes",
       subtitle = "1990-2025; broad and unknown wildlife labels shown separately",
       x = "Reported damaging strikes", y = NULL) +
  theme_minimal(base_size = 12)

rate_plot <- rate_ranking %>% head(15) %>%
  ggplot(aes(DAMAGE_RATE, reorder(SPECIES, DAMAGE_RATE))) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = paste0(scales::percent(DAMAGE_RATE, accuracy = 0.1),
                               " (", DAMAGING_STRIKES, "/", KNOWN_DAMAGE_REPORTS, ")")),
            hjust = -0.1, size = 3.5) +
  scale_x_continuous(labels = scales::percent,
                     expand = expansion(mult = c(0, 0.4))) +
  labs(title = "Named Species with the Highest Reported Damage Rates",
       subtitle = paste("1990-2025; at least", minimum_reports, "reports with known damage outcomes"),
       x = "Damaging reports / reports with known damage outcomes", y = NULL) +
  theme_minimal(base_size = 12)

other_plot <- other_labels %>% head(15) %>%
  ggplot(aes(DAMAGING_STRIKES, reorder(SPECIES, DAMAGING_STRIKES), fill = LABEL_TYPE)) +
  geom_col() +
  scale_fill_manual(values = c("Broad/ambiguous label" = "steelblue", "Unknown/missing" = "gray60"),
                    labels = c("Broad/ambiguous label" = "Species", "Unknown/missing" = "Unknown/missing")) +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Damage Reports with Broad or Unknown Wildlife Labels",
       subtitle = "1990-2025; reports recorded as wildlife groups or unidentified wildlife",
       x = "Reported damaging strikes", y = NULL, fill = NULL) +
  theme_minimal(base_size = 12) + theme(legend.position = "bottom")

ggsave(file.path(graph_dir, "species_damage_counts.png"), count_plot, width = 12, height = 8, dpi = 300)
ggsave(file.path(graph_dir, "species_damage_rates.png"), rate_plot, width = 13, height = 8, dpi = 300)
ggsave(file.path(graph_dir, "broad_unknown_damage_counts.png"), other_plot, width = 12, height = 8, dpi = 300)
