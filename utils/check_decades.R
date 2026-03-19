# Load necessary library
if (!require("stringr")) install.packages("stringr")
library(stringr)

# 1. Define the file path
input_file <- "SOE2026_NEFMC_IR_Edits.tex"

# 2. Define the Regex Pattern
# Targets 4-digit years ending in 0 followed by 's
pattern <- "\\b(1[89][0-9]0|20[0-9]0)'s"

if (file.exists(input_file)) {
  # Read the file line by line
  lines <- readLines(input_file, warn = FALSE)
  
  # 3. Find line numbers where the pattern matches
  line_indices <- grep(pattern, lines)
  
  # 4. Create the dataframe
  if (length(line_indices) > 0) {
    results_df <- data.frame(
      line_number = line_indices,
      detected_text = str_extract(lines[line_indices], pattern),
      full_line = str_trim(lines[line_indices]),
      stringsAsFactors = FALSE
    )
    
    # Print the results to console
    print(results_df)
    
  } else {
    message("No 'YYYY's' patterns found in the file.")
    results_df <- data.frame(line_number=integer(), detected_text=character(), full_line=character())
  }
  
} else {
  stop("File not found.")
}