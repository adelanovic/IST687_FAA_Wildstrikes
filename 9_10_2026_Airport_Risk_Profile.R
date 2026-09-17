library(tidyverse)
library(sf)
library(patchwork)
sf_use_s2(FALSE)

INPUT <- "data/clean/airport_specific_dataset.rds"
OUTPUT_RDS <- "data/clean/airport_risk_profile.rds"
OUTPUT_XLSX <- "data/clean/airport_risk_profile.xlsx"
graph_dir <- "graphs/9-14-2026"

# Not true strike probabilities: no flight-volume data per airport.

risk_score <- function(damage_rate, recent_strikes) {
  round(100 * (0.50 * percent_rank(damage_rate) +
               0.50 * percent_rank(recent_strikes)), 1)
}
risk_band <- function(score) {
  case_when(score >= 67 ~ "High", score >= 33 ~ "Moderate", TRUE ~ "Low")
}

airports <- readRDS(INPUT)

# Below 20 strikes, a single event swings the rate too much to profile.
profiled_airports <- airports %>%
  filter(STRIKE_TOTAL >= 20, !is.na(DAMAGE_RATE)) %>%
  mutate(RISK_SCORE = risk_score(DAMAGE_RATE, RECENT_5_YEAR_STRIKES),
         RISK_PROFILE = risk_band(RISK_SCORE)) %>%
  select(AIRPORT_ID, RISK_SCORE, RISK_PROFILE)

airport_risk_profile <- airports %>%
  left_join(profiled_airports, by = "AIRPORT_ID") %>%
  mutate(RISK_PROFILE = replace_na(RISK_PROFILE, "Limited data")) %>%
  select(AIRPORT_ID, AIRPORT, STATE, FAAREGION, LATITUDE, LONGITUDE,
         RISK_PROFILE, RISK_SCORE,
         STRIKE_TOTAL, RECENT_5_YEAR_STRIKES, DAMAGING_STRIKES, DAMAGE_RATE) %>%
  arrange(factor(RISK_PROFILE, c("High", "Moderate", "Low", "Limited data")),
          desc(RISK_SCORE), desc(STRIKE_TOTAL))

dir.create(dirname(OUTPUT_RDS), showWarnings = FALSE, recursive = TRUE)
saveRDS(airport_risk_profile, OUTPUT_RDS)
writexl::write_xlsx(airport_risk_profile, OUTPUT_XLSX)
dir.create(graph_dir, showWarnings = FALSE, recursive = TRUE)

map_airports <- airports %>%
  filter(FAAREGION %in% c("AAL", "ACE", "AEA", "AGL", "ANE", "ANM", "ASO", "ASW", "AWP"),
         STATE %in% c(state.abb, "DC"))

# Location is needed for labels, not for inclusion in the pooled counts.
region_labels <- map_airports %>%
  filter(!STATE %in% c("AK", "HI")) %>%
  group_by(FAAREGION) %>%
  summarise(LONGITUDE = weighted.mean(LONGITUDE, STRIKE_TOTAL, na.rm = TRUE),
            LATITUDE = weighted.mean(LATITUDE, STRIKE_TOTAL, na.rm = TRUE),
            .groups = "drop") %>%
  # Keep AGL and ANE labels inland, clear of lakes and coastlines.
  mutate(LONGITUDE = if_else(FAAREGION == "AGL", -89.5, LONGITUDE),
         LATITUDE = if_else(FAAREGION == "AGL", 43.5, LATITUDE),
         LONGITUDE = if_else(FAAREGION == "ANE", -69.5, LONGITUDE),
         LATITUDE = if_else(FAAREGION == "ANE", 45, LATITUDE))

region_risk_profile <- map_airports %>%
  group_by(FAAREGION) %>%
  summarise(
    LONGITUDE = weighted.mean(LONGITUDE, STRIKE_TOTAL, na.rm = TRUE),
    LATITUDE = weighted.mean(LATITUDE, STRIKE_TOTAL, na.rm = TRUE),
    DAMAGE_RATE = sum(DAMAGING_STRIKES) / sum(STRIKE_TOTAL),
    STRIKE_TOTAL = sum(STRIKE_TOTAL),
    RECENT_5_YEAR_STRIKES = sum(RECENT_5_YEAR_STRIKES),
    .groups = "drop"
  ) %>%
  mutate(RISK_SCORE = risk_score(DAMAGE_RATE, RECENT_5_YEAR_STRIKES),
         RISK_PROFILE = risk_band(RISK_SCORE))

state_risk_profile <- map_airports %>%
  group_by(STATE) %>%
  summarise(FAAREGION = names(which.max(table(FAAREGION))),
            DAMAGE_RATE = sum(DAMAGING_STRIKES) / sum(STRIKE_TOTAL),
            STRIKE_TOTAL = sum(STRIKE_TOTAL),
            RECENT_5_YEAR_STRIKES = sum(RECENT_5_YEAR_STRIKES),
            .groups = "drop") %>%
  mutate(RISK_SCORE = risk_score(DAMAGE_RATE, RECENT_5_YEAR_STRIKES),
         RISK_PROFILE = risk_band(RISK_SCORE),
         region = if_else(STATE == "DC", "district of columbia",
                          tolower(state.name[match(STATE, state.abb)])))

write.csv(state_risk_profile, file.path(graph_dir, "state_risk_scores.csv"), row.names = FALSE)
write.csv(region_risk_profile, file.path(graph_dir, "faa_region_risk_scores.csv"), row.names = FALSE)

state_shapes <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE)) %>%
  st_make_valid() %>%
  left_join(state_risk_profile, by = c("ID" = "region"))

faa_region_borders <- state_shapes %>%
  filter(!is.na(FAAREGION)) %>%
  group_by(FAAREGION) %>%
  summarise()

region_map_data <- faa_region_borders %>%
  left_join(select(region_risk_profile, FAAREGION, RISK_SCORE), by = "FAAREGION")

# Both maps use the same 0-100 color scale.
risk_map <- function(shapes, outline, outline_width, legend,
                     title, subtitle, region_outlines = FALSE, nudge_y = 0) {
  layers <- list(
    geom_sf(data = shapes, aes(fill = RISK_SCORE), color = outline,
            linewidth = outline_width),
    if (region_outlines) {
      geom_sf(data = faa_region_borders, fill = NA, color = "black", linewidth = 1.1)
    },
    geom_text(data = region_labels,
              aes(x = LONGITUDE, y = LATITUDE, label = FAAREGION),
              nudge_y = nudge_y, fontface = "bold")
  )
  ggplot() +
    layers +
    scale_fill_gradientn(colors = c("forestgreen", "gold", "firebrick"),
                         values = c(0, 0.5, 1),
                         limits = c(0, 100), name = legend) +
    coord_sf(xlim = c(-125, -66), ylim = c(24, 50), expand = FALSE) +
    labs(title = title, subtitle = subtitle, x = NULL, y = NULL) +
    theme_minimal() +
    theme(axis.text = element_blank(), panel.grid = element_blank(),
          legend.position = "bottom")
}

# Installed world boundaries supply the two states absent from maps::map("state").
inset_map <- function(state, region, regional = FALSE) {
  shapes <- st_as_sf(maps::map("world", regions = paste0("^USA:", state),
                               plot = FALSE, fill = TRUE)) %>%
    st_transform(if (state == "Alaska") 3338 else 4326) %>%
    st_make_valid()
  score <- if (regional) {
    region_risk_profile$RISK_SCORE[region_risk_profile$FAAREGION == region]
  } else {
    state_risk_profile$RISK_SCORE[state_risk_profile$region == tolower(state)]
  }
  shapes$RISK_SCORE <- score
  ggplot(shapes) +
    geom_sf(aes(fill = RISK_SCORE), color = "black", linewidth = 0.25) +
    scale_fill_gradientn(colors = c("forestgreen", "gold", "firebrick"),
                         values = c(0, 0.5, 1), limits = c(0, 100),
                         name = "Risk score (0-100)") +
    coord_sf(datum = NA) +
    labs(title = paste0(state, " (", region, ")")) +
    theme_void() +
    theme(plot.title = element_text(size = 11), legend.position = "none")
}

add_insets <- function(main_map, regional = FALSE) {
  (main_map / (inset_map("Alaska", "AAL", regional) |
                inset_map("Hawaii", "AWP", regional))) +
    plot_layout(heights = c(3, 1))
}

risk_plot <- risk_map(
  state_shapes, "white", 0.4,
  "Risk score (0-100)",
  "State Reported-Strike Risk Scores",
  NULL,
  region_outlines = TRUE, nudge_y = 1
)
risk_plot <- add_insets(risk_plot)
ggsave(file.path(graph_dir, "airport_risk_profile.png"), risk_plot,
       width = 12, height = 10, dpi = 300)

region_plot <- risk_map(
  region_map_data, "black", 1.1,
  "Risk score (0-100)",
  "FAA Regional Reported-Strike Risk Scores",
  NULL
)
region_plot <- add_insets(region_plot, regional = TRUE)
ggsave(file.path(graph_dir, "faa_region_risk_profile.png"), region_plot,
       width = 12, height = 10, dpi = 300)
