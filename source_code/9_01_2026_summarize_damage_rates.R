# Summarize the observed damage rates across eligible combinations.
# The table from 9_01_2026_rank_combinations.R is the primary answer to the business question.

csv_output_dir <- "data/data_output/damage-combinations/csv"
dir.create(csv_output_dir, recursive = TRUE, showWarnings = FALSE)

ranked_combinations <- read.csv(
  file.path(csv_output_dir, "ranked_damage_combinations.csv")
)

if (nrow(ranked_combinations) == 0) {
  stop("No eligible combinations were found.")
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
  file.path(csv_output_dir, "damage_rate_summary.csv"),
  row.names = FALSE
)

cat("Descriptive summary of eligible combinations:\n")
print(summary_results)
cat("\nUse ranked_damage_combinations.csv to identify the highest rates.\n")
