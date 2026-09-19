library(tidyverse)
library(readxl)

INPUT  <- "C:/Users/L.Admin/Documents/ISE 687 Introduction to Data Science/Homework/Project/Data Files/FAA Wildstrike CSV.xlsx"
OUTPUT <- "data/clean/faa_strikes_clean.rds"

# Exact-name assignments; edit the lookup when reviewing a new species.
species_lookup <- read_csv("data/reference/species_group_lookup.csv", show_col_types = FALSE)
stopifnot(!anyDuplicated(species_lookup$SPECIES))

KEEP <- c("INDEX_NR","INCIDENT_DATE","INCIDENT_MONTH","INCIDENT_YEAR","TIME",
          "TIME_OF_DAY","AIRPORT_ID","AIRPORT","LATITUDE","LONGITUDE","STATE",
          "FAAREGION","OPERATOR","AC_CLASS","AC_MASS","TYPE_ENG","NUM_ENGS",
          "PHASE_OF_FLIGHT","HEIGHT","SPEED","DISTANCE","SKY","PRECIPITATION",
          "COST_REPAIRS_INFL_ADJ","INDICATED_DAMAGE","DAMAGE_LEVEL","EFFECT",
          "SPECIES_ID","SPECIES","NUM_SEEN","NUM_STRUCK","SIZE","WARNED",
          "NR_INJURIES","NR_FATALITIES")

NUM <- c("INDEX_NR","INCIDENT_MONTH","INCIDENT_YEAR","LATITUDE","LONGITUDE",
         "AC_MASS","NUM_ENGS","HEIGHT","SPEED","DISTANCE",
         "COST_REPAIRS_INFL_ADJ","INDICATED_DAMAGE","NR_INJURIES","NR_FATALITIES")

BANDS <- c("1","2-10","11-100","More than 100")

SUN <- tibble(INCIDENT_MONTH = 1:12,
              rise = c(440,420,435,390,355,340,355,385,415,445,415,440),
              set  = c(1030,1065,1155,1185,1215,1235,1230,1200,1155,1110,1020,1010))

modal <- function(x) {
  x <- x[!is.na(x)]
  if (length(x)) names(which.max(table(x))) else NA
}

hdr <- names(read_excel(INPUT, n_max = 0))

strikes <- read_excel(INPUT, col_types = ifelse(hdr %in% KEEP, "text", "skip"),
                      na = c("","NA","N/A","UNKNOWN")) %>%
  mutate(across(where(is.character), str_squish),
         across(all_of(NUM), ~ suppressWarnings(as.numeric(.x))),
         INCIDENT_DATE = as.Date(INCIDENT_DATE),
         HEIGHT = if_else(between(HEIGHT, 0, 60000), HEIGHT, NA_real_),
         SPEED  = if_else(between(SPEED, 0, 700), SPEED, NA_real_)) %>%
  filter(between(INCIDENT_YEAR, 1990, as.numeric(format(Sys.Date(), "%Y")))) %>%
  distinct(INDEX_NR, .keep_all = TRUE)

apt <- strikes %>%
  filter(AIRPORT_ID != "ZZZZ") %>%
  group_by(AIRPORT_ID) %>%
  summarise(rg = modal(FAAREGION), st = modal(STATE),
            lat = median(LATITUDE, na.rm = TRUE),
            lon = median(LONGITUDE, na.rm = TRUE), .groups = "drop")

strikes <- strikes %>%
  left_join(apt, by = "AIRPORT_ID") %>%
  left_join(SUN, by = "INCIDENT_MONTH") %>%
  mutate(
    FAAREGION = coalesce(FAAREGION, rg), STATE = coalesce(STATE, st),
    LATITUDE = coalesce(LATITUDE, lat), LONGITUDE = coalesce(LONGITUDE, lon),
    
    m = str_pad(na_if(str_remove_all(TIME, "\\D"), ""), 4, "left", "0"),
    m = as.numeric(str_sub(m, 1, 2)) * 60 + as.numeric(str_sub(m, 3, 4)),
    m = if_else(between(m, 0, 1439), m, NA_real_),
    TIME_OF_DAY_FILLED = coalesce(TIME_OF_DAY, case_when(
      is.na(m)                      ~ NA_character_,
      m >= rise - 30 & m < rise     ~ "Dawn",
      m >= rise      & m < set      ~ "Day",
      m >= set       & m < set + 30 ~ "Dusk",
      TRUE                          ~ "Night")),
    across(c(TIME_OF_DAY, TIME_OF_DAY_FILLED), ~ factor(.x, c("Dawn","Day","Dusk","Night"))),
    
    SPECIES_GROUP = if_else(is.na(SPECIES), NA_character_,
      coalesce(species_lookup$SPECIES_GROUP[match(SPECIES, species_lookup$SPECIES)],
               "Needs review")),
    
    DAMAGE_LEVEL = str_remove(DAMAGE_LEVEL, fixed("?")),
    DAMAGE_LEVEL = factor(if_else(is.na(DAMAGE_LEVEL) & INDICATED_DAMAGE == 0, "N", DAMAGE_LEVEL),
                          c("N","M","S","D"), ordered = TRUE),
    DAMAGED = factor(if_else(INDICATED_DAMAGE == 1, "Yes", "No"), c("No","Yes")),
    
    PHASE_OF_FLIGHT = str_to_title(PHASE_OF_FLIGHT),
    PHASE_GROUP = case_when(
      PHASE_OF_FLIGHT %in% c("Parked","Taxi") ~ "Ground",
      PHASE_OF_FLIGHT %in% c("Take-Off Run","Climb","Departure") ~ "Departure",
      PHASE_OF_FLIGHT %in% c("Approach","Descent","Landing Roll","Arrival") ~ "Arrival",
      PHASE_OF_FLIGHT %in% c("En Route","Local") ~ "En route"),
    PHASE_GROUP_FILLED = coalesce(PHASE_GROUP, case_when(
      is.na(HEIGHT) ~ NA_character_, HEIGHT == 0 ~ "Ground", HEIGHT >= 1500 ~ "En route")),
    across(c(PHASE_GROUP, PHASE_GROUP_FILLED),
           ~ factor(.x, c("Ground","Departure","En route","Arrival"))),
    
    SIZE = factor(SIZE, c("Small","Medium","Large"), ordered = TRUE),
    NUM_STRUCK = factor(NUM_STRUCK, BANDS, ordered = TRUE),
    NUM_SEEN = factor(NUM_SEEN, BANDS, ordered = TRUE),
    AC_MASS = factor(AC_MASS, 1:5, ordered = TRUE),
    WARNED = factor(coalesce(WARNED, "Unknown"), c("No","Yes","Unknown")),
    across(c(AC_CLASS, TYPE_ENG, SKY, PRECIPITATION, EFFECT, FAAREGION, STATE), factor),
    
    MONTH_NAME = factor(month.abb[INCIDENT_MONTH], month.abb),
    SEASON = factor(case_when(INCIDENT_MONTH %in% c(12,1,2) ~ "Winter",
                              INCIDENT_MONTH %in% 3:5 ~ "Spring",
                              INCIDENT_MONTH %in% 6:8 ~ "Summer",
                              TRUE ~ "Fall"), c("Winter","Spring","Summer","Fall"))) %>%
  select(-rg, -st, -lat, -lon, -m, -rise, -set)

dir.create(dirname(OUTPUT), showWarnings = FALSE, recursive = TRUE)
saveRDS(strikes, OUTPUT)
writexl::write_xlsx(mutate(strikes, across(where(is.factor), as.character)),
                    sub("\\.rds$", ".xlsx", OUTPUT))
