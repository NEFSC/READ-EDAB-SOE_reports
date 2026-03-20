# Load necessary package
if (!require("stringr")) install.packages("stringr")
library(stringr)

find_orphaned_figures <- function(file_path) {
  if (!file.exists(file_path)) {
    stop("The specified .tex file does not exist.")
  }
  
  # 1. Read and clean the file
  lines <- readLines(file_path, warn = FALSE)
  lines_clean <- str_replace_all(lines, "(?<!\\\\\\\\)%.*", "")
  doc_text <- paste(lines_clean, collapse = "\n")
  
  # 2. Extract figure environments sequentially
  fig_blocks <- str_extract_all(doc_text, "(?s)\\\\begin\\{figure\\}.*?\\\\end\\{figure\\}")[[1]]
  
  if (length(fig_blocks) == 0) {
    message("No figure environments found.")
    return(data.frame())
  }
  
  # 3. Build a dataframe of all figures and their sequential order
  # Extract the label for each block. If a figure has no label, it returns NA.
  labels_extracted <- sapply(fig_blocks, function(block) {
    str_match(block, "\\\\label\\{([^}]+)\\}")[, 2]
  })
  
  fig_data <- data.frame(
    expected_fig_number = seq_along(fig_blocks),
    fig_label = unname(labels_extracted),
    stringsAsFactors = FALSE
  )
  
  # Drop any figures that don't have a label (you can't reference them anyway)
  fig_data <- fig_data[!is.na(fig_data$fig_label), ]
  
  # 4. Extract all reference calls from the document
  ref_matches <- str_match_all(doc_text, "\\\\(?:ref|cref|Cref|autoref|subref)\\{([^}]+)\\}")[[1]][, 2]
  all_refs <- unlist(str_split(ref_matches, "\\s*,\\s*"))
  all_refs <- str_trim(all_refs) # Clean any rogue spaces
  
  # 5. Filter the dataframe to only keep labels NOT found in all_refs
  orphaned_df <- fig_data[!fig_data$fig_label %in% all_refs, ]
  rownames(orphaned_df) <- NULL # Reset row numbers for a clean look
  
  # 6. Return the results
  if (nrow(orphaned_df) > 0) {
    return(orphaned_df)
  } else {
    message("Great job! All defined figures are referenced in the text.")
    return(data.frame(expected_fig_number = integer(), fig_label = character()))
  }
}

# --- How to run it ---
# Replace with your actual file path
results_df <- find_orphaned_figures("SOE2026_NEFMC_IR_Edits.tex")
print(results_df)