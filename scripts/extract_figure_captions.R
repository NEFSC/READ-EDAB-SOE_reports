# install.packages("knitr") # Uncomment and run if you don't have knitr
# install.packages("rmarkdown") # Uncomment and run if you don't have rmarkdown
# install.packages("curl") # Uncomment and run if you don't have curl (for download.file)

library(knitr)
library(rmarkdown)
library(dplyr) # For data manipulation
library(readr) # For reading content from URL

# --- Define the URLs for your R Markdown files ---
ne_rmd_url <- "https://raw.githubusercontent.com/NOAA-EDAB/SOE-NEFMC/refs/heads/NE_SOE_2025/SOE-NEFMC.Rmd"
ma_rmd_url <- "https://raw.githubusercontent.com/NOAA-EDAB/SOE-MAFMC/refs/heads/master/SOE-MAFMC-2025.Rmd"

# --- Function to extract chunk info ---
extract_figure_captions <- function(rmd_url, report_name) {
  message(paste0("Processing ", report_name, " report from: ", rmd_url))

  # Safely read content from the URL
  rmd_content <- tryCatch(
    {
      read_file(rmd_url)
    },
    error = function(e) {
      stop(paste0("Failed to read RMD content from ", rmd_url, ": ", e$message))
    }
  )

  # Parse the R Markdown content to extract chunk options
  # knitr::parse_params() is excellent for this, but needs to be applied carefully
  # A simpler approach using regular expressions might be more robust for just captions.

  # This regex tries to find R chunks with a name and a fig.cap option
  # It looks for lines starting with ```{r chunk_name, ..., fig.cap="your caption", ...}
  # It's a simplified regex and might need adjustment for complex chunk headers.
  # Group 1: chunk_name, Group 2: caption content
  chunk_pattern <- "```\\{r\\s+([^,}\n]+)(?:,.*?)?fig\\.cap\\s*=\\s*\"([^\"]*)\"(?:,.*?)*\\}"

  matches <- gregexpr(chunk_pattern, rmd_content, perl = TRUE)
  chunk_matches <- regmatches(rmd_content, matches)

  if (length(chunk_matches[[1]]) == 0) {
    message("No figure chunks with 'fig.cap' found in ", report_name)
    return(NULL)
  }

  extracted_data <- lapply(chunk_matches[[1]], function(match_str) {
    # Extracting the details using sub for each match
    chunk_name <- sub(chunk_pattern, "\\1", match_str, perl = TRUE)
    caption <- sub(chunk_pattern, "\\2", match_str, perl = TRUE)
    data.frame(
      Report = report_name,
      ChunkName = trimws(chunk_name),
      FigureCaption = trimws(caption),
      stringsAsFactors = FALSE
    )
  })

  bind_rows(extracted_data)
}

# --- Run the extraction for both reports ---
ne_captions <- extract_figure_captions(ne_rmd_url, "New England")
ma_captions <- extract_figure_captions(ma_rmd_url, "Mid Atlantic")

# --- Combine and display results ---
all_captions <- bind_rows(ne_captions, ma_captions)

if (!is.null(all_captions) && nrow(all_captions) > 0) {
  message("\n--- Extracted Figure Chunk Names and Captions ---")
  print(all_captions)

  # Optional: Save to CSV
  # write.csv(all_captions, "utils/figure_captions_summary.csv", row.names = FALSE)
  message("\nOutput printed above. You can also uncomment the line to save to 'figure_captions_summary.csv'.")
} else {
  message("No figure chunks with captions were found in either report.")
}

all_captions <- dplyr::full_join(
  ne_captions |>
    dplyr::select(-Report) |>
    dplyr::rename(FigureCaption_NewEngland = FigureCaption),
  ma_captions |>
    dplyr::select(-Report) |>
    dplyr::rename(FigureCaption_MidAtlantic = FigureCaption)
)
write.csv(all_captions |> dplyr::arrange(ChunkName), "utils/figure_captions_summary.csv", row.names = FALSE)
