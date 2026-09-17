# Summarize observed damage rates across eligible wildlife-group combinations.

csv_output_dir <- "data/data_output/damage-combinations-2/csv"
dir.create(csv_output_dir, recursive = TRUE, showWarnings = FALSE)

ranked_combinations <- read.csv(
  file.path(csv_output_dir, "ranked_group_damage_combinations.csv")
)

if (nrow(ranked_combinations) == 0) {
  stop("No eligible wildlife-group combinations were found.")
}

summary_results <- data.frame(
  eligible_combinations = nrow(ranked_combinations),
  total_reported_strikes = sum(ranked_combinations$reported_strikes),
  total_damaging_strikes = sum(ranked_combinations$damaging_strikes),
  overall_damage_rate = sum(ranked_combinations$damaging_strikes) /
    sum(ranked_combinations$reported_strikes),
  highest_combination_rate = max(ranked_combinations$damage_rate),
  lowest_combination_rate = min(ranked_combinations$damage_rate)
)

write.csv(
  summary_results,
  file.path(csv_output_dir, "group_damage_rate_summary.csv"),
  row.names = FALSE
)

cat("Descriptive summary of eligible wildlife-group combinations:\n")
print(summary_results)
cat("\nUse ranked_group_damage_combinations.csv for the detailed ranking.\n")
