df <- read.csv(
  here::here("utils/figure_captions_summary.csv"),
  stringsAsFactors = FALSE
)

# Function to find non-ASCII characters
find_non_ascii <- function(x) {
  # grep returns indices of strings containing non-ASCII
  which(grepl("[^\x01-\x7f]", x))
}

# Apply the search across all columns
results <- lapply(df, find_non_ascii)

# Print the results
results
