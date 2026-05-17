#!/bin/bash
# Script to combine all counterfactual minimum wage results
# Usage: bash combine_counterfactual_minimum_wage.sh

cd $HOME/Dispatching || exit 1

echo "=========================================="
echo "Combining results at: $(date)"
echo "=========================================="

# Create output directory if it doesn't exist
mkdir -p output/multihome/counterfactual/minimum_wage

# Create R script to combine results
RSCRIPT=$(cat <<'EOF'
library(dplyr)
library(purrr)

# Find all result files
result_dir <- "output/multihome/counterfactual/minimum_wage"
pattern <- "counterfactual_minimum_wage_t\\d+_j\\d+\\.rds$"
files <- list.files(result_dir, pattern = pattern, full.names = TRUE)

if (length(files) == 0) {
  stop("No result files found")
}

cat("Found", length(files), "result files\n")

# Read and combine all results
results <- map_dfr(files, function(f) {
  tryCatch({
    readRDS(f)
  }, error = function(e) {
    cat("Error reading", f, ":", conditionMessage(e), "\n")
    NULL
  })
})

if (nrow(results) == 0) {
  stop("No valid results to combine")
}

cat("Combined", nrow(results), "rows\n")

# Save combined results
output_file <- file.path(result_dir, "counterfactual_minimum_wage_combined.rds")
saveRDS(results, file = output_file)
cat("Saved combined results to:", output_file, "\n")

# Print summary
cat("\nSummary by market and zone:\n")
print(results %>% 
  group_by(t, j) %>% 
  summarise(n = n(), .groups = "drop") %>%
  arrange(t, j))

cat("\nTotal unique market-zone combinations:", 
    n_distinct(paste(results$t, results$j, sep = "_")), "\n")
EOF
)

# Run R script
echo "$RSCRIPT" | Rscript - 2>&1

if [ $? -eq 0 ]; then
  echo ""
  echo "=========================================="
  echo "Combination completed successfully"
  echo "Output: output/multihome/counterfactual/minimum_wage/counterfactual_minimum_wage_combined.rds"
  echo "=========================================="
else
  echo ""
  echo "ERROR: Failed to combine results" >&2
  exit 1
fi

