library(tidyverse)
library(stringr)
library(httr)

# ==============================================================================
# CONFIGURATION
# ==============================================================================

# Point this to your generated .tex file (usually same name as .Rmd)
TEX_FILE <- here::here("SOE2026_NEFMC_IR_V2.tex" )

# ==============================================================================
# PARSING LOGIC
# ==============================================================================

extract_urls_from_tex <- function(file_path) {
  
  if (!file.exists(file_path)) {
    stop(paste("Could not find file:", file_path, "\nMake sure to set 'keep_tex: true' in your YAML header and render the document first."))
  }
  
  lines <- readLines(file_path, warn = FALSE)
  
  # Create base dataframe
  df_text <- tibble(
    line_number = 1:length(lines),
    text = lines
  )
  
  # 1. Map Sections (Context)
  # LaTeX headers look like \section{Title}, \subsection{Title}, etc.
  df_text <- df_text %>%
    mutate(
      # Find lines that look like sections
      is_header = str_detect(text, "\\\\(sub)*section\\{"),
      # Extract the title between the curly braces
      # Note: This simple regex assumes the title doesn't contain a closing curly brace '}'
      section_name = str_match(text, "\\\\(sub)*section\\{([^}]+)\\}")[,3]
    ) %>%
    fill(section_name, .direction = "down") %>%
    mutate(section_name = replace_na(section_name, "Preamble"))
  
  # 2. Extract URLs
  # We look for two standard LaTeX patterns:
  # A: \href{URL}{Text}
  # B: \url{URL}
  
  extracted_links <- df_text %>%
    rowwise() %>%
    mutate(
      # Pattern A: \href{...}{...}
      # Group 1 is URL, Group 2 is Text
      matches_href = list(str_match_all(text, "\\\\href\\{([^}]+)\\}\\{([^}]*)\\}")[[1]]),
      
      # Pattern B: \url{...}
      # Group 1 is URL
      matches_url = list(str_match_all(text, "\\\\url\\{([^}]+)\\}")[[1]])
    ) %>%
    ungroup()
  
  # Process \href results
  # FIX: We construct the tibble directly from matrix columns to avoid "Empty names" error
  href_df <- extracted_links %>%
    select(line_number, section_name, matches_href) %>%
    filter(map_lgl(matches_href, ~nrow(.x) > 0)) %>% # Only keep lines with matches
    mutate(matches_href = map(matches_href, function(m) {
      # m is a matrix: col 1 = full match, col 2 = URL, col 3 = Text
      tibble(
        url = m[, 2],
        shorthand = m[, 3]
      )
    })) %>%
    unnest(matches_href)
  
  # Process \url results
  # FIX: We construct the tibble directly from matrix columns to avoid "Empty names" error
  url_cmd_df <- extracted_links %>%
    select(line_number, section_name, matches_url) %>%
    filter(map_lgl(matches_url, ~nrow(.x) > 0)) %>% # Only keep lines with matches
    mutate(matches_url = map(matches_url, function(m) {
      # m is a matrix: col 1 = full match, col 2 = URL
      tibble(
        url = m[, 2]
      )
    })) %>%
    unnest(matches_url) %>%
    mutate(shorthand = "Raw URL")
  
  # Combine and Clean
  final_df <- bind_rows(href_df, url_cmd_df) %>%
    mutate(
      # LaTeX sometimes escapes special chars in URLs (like underscores or percentages)
      # We need to unescape them for the link to work
      url = str_replace_all(url, "\\\\_", "_"),
      url = str_replace_all(url, "\\\\%", "%"),
      url = str_replace_all(url, "\\\\&", "&"),
      url = str_replace_all(url, "\\\\#", "#")
    )
  
  return(final_df)
}

# ==============================================================================
# VERIFICATION LOGIC
# ==============================================================================

check_status <- function(url) {
  url <- str_trim(url)
  
  # Skip internal LaTeX links (like #references)
  if(str_starts(url, "#")) return("Internal Link")
  
  tryCatch({
    res <- HEAD(url, timeout(10), user_agent("Mozilla/5.0 (R; URLChecker)"))
    status <- status_code(res)
    
    if (status %in% c(405, 403, 404, 500)) {
      res <- GET(url, timeout(10), user_agent("Mozilla/5.0 (R; URLChecker)"))
      status <- status_code(res)
    }
    return(as.character(status))
  }, error = function(e) {
    return(paste("Error:", e$message))
  })
}

# ==============================================================================
# EXECUTION
# ==============================================================================

message("1. parsing .tex file...")
links_df <- extract_urls_from_tex(TEX_FILE)

if(nrow(links_df) == 0) {
  stop("No links found. Check if the .tex file is empty or formatted unexpectedly.")
}

message(paste("Found", nrow(links_df), "links. Verifying..."))

results <- links_df %>%
  mutate(
    status_code = map_chr(url, function(x) {
      cat(".") 
      check_status(x)
    })
  ) %>%
  mutate(
    verified = status_code == "200"
  )

cat("\nDone!\n")

# Report
output_filename <- paste0(basename(tools::file_path_sans_ext(TEX_FILE)),"_url_check_", format(Sys.Date(), "%Y%m%d"), ".csv")

# Select clean columns for output
final_report <- results %>%
  select(section_name, line_number, shorthand, url, verified, status_code)

write_csv(final_report, output_filename)

print("--- Issues Found ---")
print(final_report %>% filter(!verified) %>% select(section_name, url, status_code))
message(paste("Report saved to", output_filename))
