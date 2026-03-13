# Install required packages if you haven't already:
# install.packages(c("xml2", "dplyr", "purrr", "readr", "tibble"))

library(xml2)
library(dplyr)
library(purrr)
library(readr)
library(tibble)

# --- Helper Function to Clean Acrobat Dates ---
# Acrobat dates look like: "D:20260227111840-05'00'"
# This function converts it to: "2026-02-27 11:18:40"
clean_acrobat_date <- function(d_string) {
  if (is.na(d_string)) return(NA_character_)
  cleaned <- sub("^D:(\\d{4})(\\d{2})(\\d{2})(\\d{2})(\\d{2})(\\d{2}).*", 
                 "\\1-\\2-\\3 \\4:\\5:\\6", 
                 d_string)
  return(cleaned)
}

# --- Main Consolidation Function ---
#' Extract, Combine, and Deduplicate XFDF Comments
#'
#' @param directory The folder path containing the .xfdf files.
#' @param file_pattern A string to match against file names (e.g., "SOE2026").
#' @param output_csv The full path and filename for the resulting CSV.
#' @return Invisibly returns the final deduplicated dataframe.
consolidate_xfdf_comments <- function(directory, file_pattern, output_csv) {
  
  # 1. Find matching .xfdf files
  # This regex combines your specific string with the .xfdf extension (case-insensitive)
  search_pattern <- paste0("(?i).*", file_pattern, ".*\\.xfdf$")
  xfdf_files <- list.files(path = directory, pattern = search_pattern, full.names = TRUE)
  
  if(length(xfdf_files) == 0) {
    stop(paste("No .xfdf files found in", directory, "matching pattern:", file_pattern))
  }
  
  cat("Found", length(xfdf_files), "matching .xfdf files. Processing...\n")
  
  # 2. Iterate through files and extract comments
  all_comments <- map_df(xfdf_files, function(file_path) {
    
    # Get just the filename (e.g., "SOE2026_NEFMC_IR_Final_commented AT.xfdf")
    doc_name <- basename(file_path)
    cat("  Reading:", doc_name, "\n")
    
    doc <- read_xml(file_path)
    
    # CRITICAL FIX: Strip all namespaces to handle Acrobat's rich text tags
    xml_ns_strip(doc)
    
    # Find all annotation nodes
    annots <- xml_find_all(doc, "//annots/*")
    
    # If a file has zero comments, return an empty tibble to prevent mapping errors
    if (length(annots) == 0) {
      return(tibble()) 
    }
    
    # Map the individual nodes for this specific file
    file_comments <- map_df(annots, function(node) {
      
      author <- xml_attr(node, "title")
      page_attr <- xml_attr(node, "page")
      page <- ifelse(!is.na(page_attr), as.numeric(page_attr) + 1, NA)
      date_raw <- xml_attr(node, "date")
      subject <- xml_attr(node, "subject")
      annot_type <- xml_name(node) 
      
      content_node <- xml_find_first(node, ".//contents-richtext | .//contents")
      comment_text <- xml_text(content_node) 
      
      # Build the row, inserting the Document name
      tibble(
        Document = doc_name,
        Author   = ifelse(is.na(author), "Unknown", author),
        Page     = page,
        Date     = clean_acrobat_date(date_raw),
        Type     = annot_type,
        Subject  = ifelse(is.na(subject), "", subject),
        Comment  = ifelse(is.na(comment_text), "", trimws(comment_text))
      )
    })
    
    # Filter out empty comments before combining
    file_comments %>% filter(Comment != "")
  })
  
  # 3. Deduplicate across the combined dataframe
  initial_rows <- nrow(all_comments)
  
  final_comments <- all_comments %>%
    distinct(Page, Date, Comment, .keep_all = TRUE) %>% # Removes duplicates matching Page, Date, and Comment
    arrange(Document, Page, Date) # Sort logically for readability
  
  duplicates_removed <- initial_rows - nrow(final_comments)
  
  # 4. Print Summary to Console
  cat("\n--- Processing Summary ---\n")
  cat("Total comments extracted:  ", initial_rows, "\n")
  cat("Duplicate comments dropped:", duplicates_removed, "\n")
  cat("Final unique comments:     ", nrow(final_comments), "\n\n")
  
  # 5. Export to CSV
  write_csv(final_comments, output_csv)
  cat("Success! Master deduplicated list exported to:\n", output_csv, "\n")
  
  # Return the dataframe silently so it can be assigned to an R variable if desired
  invisible(final_comments)
}

# ==========================================
# EXAMPLE USAGE
# ==========================================
# You can run the function like this:
#
my_comments <- consolidate_xfdf_comments(
  directory    = here::here('drafts'),
  file_pattern = "SOE2026_",
  output_csv   = here::here('drafts', "SOE2026_IR_Master_Comments.csv")
)