library(tidyverse)
library(stringr)

# ==============================================================================
# CONFIGURATION
# ==============================================================================

# 1. The compiled TeX file (Source of Truth for what is broken)
TEX_FILE <- here::here("SOE2026_NEFMC_IR_V2.tex" )

# 2. Directories to scan for Rmd source files
#    (Adjust if your child docs are elsewhere)
SOURCE_DIRS <- c(".", "child_docs")

# ==============================================================================
# PART 1: TEX ANALYSIS (Identify what is broken)
# ==============================================================================

get_tex_status <- function(tex_path) {
  if (!file.exists(tex_path)) stop("TeX file not found. Render PDF with 'keep_tex: true' first.")
  
  lines <- readLines(tex_path, warn = FALSE)
  text_blob <- paste(lines, collapse = "\n")
  
  # 1. Extract Valid Labels (\label{...})
  labels <- str_extract_all(text_blob, "(?<=\\\\label\\{)[^}]+(?=\\})")[[1]]
  valid_keys <- unique(labels)
  
  # 2. Extract References (\ref{...})
  # Get context lines for refs
  df_text <- tibble(line_number = 1:length(lines), text = lines)
  
  refs_df <- df_text %>%
    filter(str_detect(text, "\\\\ref\\{")) %>%
    mutate(
      # Extract key
      ref_key = str_extract(text, "(?<=\\\\ref\\{)[^}]+(?=\\})"),
      # Check validity
      is_valid = ref_key %in% valid_keys
    ) %>%
    filter(!is_valid) # We only care about broken ones now
  
  return(refs_df)
}

# ==============================================================================
# PART 2: SOURCE RMD ANALYSIS (Find context & fixes)
# ==============================================================================

scan_rmd_files <- function(dirs) {
  
  rmd_files <- list.files(dirs, pattern = "\\.Rmd$", full.names = TRUE, recursive = TRUE)
  
  results_list <- list()
  
  for (f in rmd_files) {
    lines <- readLines(f, warn = FALSE)
    
    # Create DF for this file
    df <- tibble(file = f, line = 1:length(lines), text = lines)
    
    # A. Find Chunk Names (Potential Figure/Table targets)
    # Regex looks for ```{r chunk-name, ...
    chunks <- df %>%
      filter(str_detect(text, "^```\\{r")) %>%
      mutate(
        name = str_match(text, "^```\\{r\\s+([^,}\\}]+)")[,2],
        type = "chunk"
      ) %>%
      filter(!is.na(name)) %>%
      select(file, line, type, name)
    
    # B. Find Headers (Potential Section targets)
    # 1. Explicit IDs: # Header {#my-id}
    headers_explicit <- df %>%
      filter(str_detect(text, "\\{#")) %>%
      mutate(
        name = str_match(text, "\\{#([^}]+)\\}")[,2],
        type = "section_id"
      ) %>%
      filter(!is.na(name)) %>%
      select(file, line, type, name)
    
    # 2. Auto IDs: # My Header -> my-header
    # (Simplified: RMarkdown usually lowercases and hyphenates)
    headers_auto <- df %>%
      filter(str_detect(text, "^#+ ")) %>%
      mutate(
        raw_header = str_remove(text, "^#+\\s+"),
        # Strip explicitly named IDs if present to avoid duplication
        raw_header = str_remove(raw_header, "\\s*\\{.*\\}$"),
        name = str_to_lower(str_replace_all(raw_header, "[^a-zA-Z0-9]+", "-")),
        # remove trailing hyphens
        name = str_remove(name, "-+$"),
        type = "header_auto"
      ) %>%
      select(file, line, type, name)
    
    # C. Find Broken Ref Usage in Source
    # This helps locate where the error actually is
    ref_usage <- df %>%
      filter(str_detect(text, "\\\\ref\\{")) %>%
      mutate(
        name = str_extract(text, "(?<=\\\\ref\\{)[^}]+(?=\\})"),
        type = "usage"
      ) %>%
      select(file, line, type, name)
    
    results_list[[f]] <- bind_rows(chunks, headers_explicit, headers_auto, ref_usage)
  }
  
  bind_rows(results_list)
}

# ==============================================================================
# PART 3: RECONCILIATION & RECOMMENDATION
# ==============================================================================

generate_diagnostics <- function(broken_refs_df, rmd_data_df) {
  
  # 1. Where is the broken ref used?
  # Map TeX broken refs to Rmd usage
  issues <- broken_refs_df %>%
    distinct(ref_key) %>%
    left_join(
      rmd_data_df %>% filter(type == "usage") %>% select(file, line, ref_key = name),
      by = "ref_key"
    )
  
  # 2. Analyze each issue
  diagnostics <- issues %>%
    rowwise() %>%
    mutate(
      recommendation = {
        # Defaults
        rec <- "No obvious match found."
        
        # A. Check for "fig:" or "tab:" prefix issues
        # Often user types \ref{fig:plot} but chunk is just 'plot'
        clean_key <- str_remove(ref_key, "^(fig|tab):")
        
        # Search for exact match of the cleaned key in chunks
        chunk_match <- rmd_data_df %>% 
          filter(type == "chunk", name == clean_key) %>% 
          slice(1)
        
        if (nrow(chunk_match) > 0) {
          rec <- paste0("FOUND CHUNK: '", clean_key, "' in ", basename(chunk_match$file), 
                        " (Line ", chunk_match$line, "). RMarkdown adds 'fig:' automatically. ",
                        "Ensure chunk has a caption!")
        } else {
          # B. Fuzzy Match (Spelling errors)
          # Get all known definitions
          all_defs <- rmd_data_df %>% filter(type != "usage") %>% pull(name)
          
          # Calculate distances
          dists <- stringdist::stringdist(ref_key, all_defs, method = "lv")
          best_idx <- which.min(dists)
          
          if (length(best_idx) > 0 && dists[best_idx] <= 3) { # Threshold for typo
            match_val <- all_defs[best_idx]
            match_meta <- rmd_data_df %>% filter(name == match_val, type != "usage") %>% slice(1)
            rec <- paste0("TYPO? Did you mean '", match_val, "'? Found in ", 
                          basename(match_meta$file), " (Line ", match_meta$line, ")")
          }
        }
        rec
      }
    ) %>%
    ungroup() %>%
    select(ref_key, source_file = file, source_line = line, recommendation)
  
  return(diagnostics)
}

# ==============================================================================
# EXECUTION
# ==============================================================================

# Helper for string distance (simple implementation if package not available)
if (!requireNamespace("stringdist", quietly = TRUE)) {
  message("Installing 'stringdist' for fuzzy matching...")
  install.packages("stringdist")
}

message("1. Scanning compiled TeX for broken references...")
tryCatch({
  broken <- get_tex_status(TEX_FILE)
  
  if (nrow(broken) == 0) {
    message("No broken references found in TeX!")
  } else {
    message(paste("Found", nrow(broken), "broken references. Scanning source Rmds..."))
    
    rmd_data <- scan_rmd_files(SOURCE_DIRS)
    
    report <- generate_diagnostics(broken, rmd_data)
    
    # Print nice table
    print(report)
    
    # Save
    out_file <- paste0("reference_fixer_", format(Sys.Date(), "%Y%m%d"), ".csv")
    write_csv(report, out_file)
    message(paste("\nReport with recommendations saved to:", out_file))
  }
  
}, error = function(e) {
  message("Error: ", e$message)
})
