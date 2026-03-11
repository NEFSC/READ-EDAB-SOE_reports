# Install required packages if you haven't already:
# install.packages(c("xml2", "dplyr", "purrr", "readr", "tibble"))

library(xml2)
library(dplyr)
library(purrr)
library(readr)
library(tibble)

# 1. Define your file paths
xfdf_file <- here::here('drafts',"SOE2026_NEFMC_IR_Final_commented.xfdf") # Replace with your actual file path
csv_output <- here::here('drafts',"NEFMC_IR_consolidated_comments.csv")

# --- Helper Function to Clean Acrobat Dates ---
# Acrobat dates look like: "D:20260227111840-05'00'"
# This function converts it to: "2026-02-27 11:18:40"
clean_acrobat_date <- function(d_string) {
  if (is.na(d_string)) return(NA_character_)
  # Use regex to extract the YYYY, MM, DD, HH, MM, SS
  cleaned <- sub("^D:(\\d{4})(\\d{2})(\\d{2})(\\d{2})(\\d{2})(\\d{2}).*", 
                 "\\1-\\2-\\3 \\4:\\5:\\6", 
                 d_string)
  return(cleaned)
}

# 2. Read the XML/XFDF document
doc <- read_xml(xfdf_file)

# CRITICAL FIX: Strip all namespaces. 
# Acrobat uses mixed namespaces for rich text which breaks standard searches.
xml_ns_strip(doc)

# 3. Find all annotation nodes (now that namespaces are gone, we don't need 'd1:')
# We look for any child node inside <annots>
annots <- xml_find_all(doc, "//annots/*")

cat("Found", length(annots), "comments/annotations. Parsing now...\n")

# 4. Extract data and map it into a dataframe
comments_df <- map_df(annots, function(node) {
  
  # Extract attributes
  author <- xml_attr(node, "title")
  
  # XFDF pages are 0-indexed (page="1" means page 2 of the PDF)
  page_attr <- xml_attr(node, "page")
  page <- ifelse(!is.na(page_attr), as.numeric(page_attr) + 1, NA)
  
  date_raw <- xml_attr(node, "date")
  subject <- xml_attr(node, "subject")
  annot_type <- xml_name(node) # e.g., 'highlight', 'text', 'strikeout'
  
  # CRITICAL FIX: Find either <contents-richtext> OR <contents>
  content_node <- xml_find_first(node, ".//contents-richtext | .//contents")
  
  # xml_text() will automatically strip out the <p> and <span> tags
  comment_text <- xml_text(content_node) 
  
  # Return as a single-row tibble
  tibble(
    Author = ifelse(is.na(author), "Unknown", author),
    Page = page,
    Date = clean_acrobat_date(date_raw),
    Type = annot_type,
    Subject = ifelse(is.na(subject), "", subject),
    Comment = ifelse(is.na(comment_text), "", trimws(comment_text))
  )
})

# 5. Clean up the dataframe
# Filter out empty comments (like highlights without attached text)
comments_df <- comments_df %>%
  filter(Comment != "") %>%
  arrange(Page, Date) # Sort chronologically by page

# 6. Preview the dataframe in the console
print(head(comments_df))

# 7. Write to CSV
write_csv(comments_df, csv_output)
cat("\nSuccess! Consolidated comments exported to:", csv_output, "\n")
