clean_dated_files <- function(directory = ".", dry_run = TRUE) {
  
  # 1. List all PNG files in the directory
  all_files <- list.files(directory, pattern = "\\.png$", full.names = FALSE)
  
  if (length(all_files) == 0) {
    message("No PNG files found in directory.")
    return(invisible(NULL))
  }
  
  # 2. Define Regex to capture: (Basename)_(YYYY-MM-DD).png
  #    ^(.*)       : Group 1 - Matches any characters (greedy)
  #    _           : The underscore separating name and date
  #    ([0-9]{4}-[0-9]{2}-[0-9]{2}) : Group 2 - Matches YYYY-MM-DD
  #    \\.png$     : Ends with .png
  pattern <- "^(.*)_([0-9]{4}-[0-9]{2}-[0-9]{2})\\.png$"
  
  # 3. Create a data frame to organize file info
  file_info <- data.frame(
    filename = all_files,
    stringsAsFactors = FALSE
  )
  
  # Extract the groups (Basename and Date)
  # strcapture is a base R function that populates a proto-dataframe structure
  matches <- strcapture(
    pattern = pattern, 
    x = file_info$filename, 
    proto = data.frame(basename = character(), date_str = character())
  )
  
  # Combine and filter
  file_info <- cbind(file_info, matches)
  
  # Filter out files that didn't match the pattern (no date appended)
  # These are the ones we must NOT touch.
  dated_files <- file_info[!is.na(file_info$basename), ]
  
  if (nrow(dated_files) == 0) {
    message("No files matching the 'basename_YYYY-MM-DD.png' pattern found.")
    return(invisible(NULL))
  }
  
  # Convert string date to actual Date object for comparison
  dated_files$date <- as.Date(dated_files$date_str)
  
  # 4. Identify files to delete
  #    We split by 'basename', find the max date, and mark others for deletion
  files_to_delete <- c()
  
  unique_bases <- unique(dated_files$basename)
  
  for (base in unique_bases) {
    # Get all files for this specific basename
    subset <- dated_files[dated_files$basename == base, ]
    
    # If there is more than one version
    if (nrow(subset) > 1) {
      # Find the newest date
      max_date <- max(subset$date)
      
      # Identify files strictly older than the max date
      # (This keeps the newest one. If there are duplicates of the NEWEST date, both stay)
      old_files <- subset[subset$date < max_date, "filename"]
      
      files_to_delete <- c(files_to_delete, old_files)
    }
  }
  
  # 5. Execute Deletion (or Print for Dry Run)
  if (length(files_to_delete) > 0) {
    full_paths <- file.path(directory, files_to_delete)
    
    if (dry_run) {
      message("--- DRY RUN (No files deleted) ---")
      message("The following files would be deleted:")
      print(files_to_delete)
    } else {
      unlink(full_paths)
      message(paste("Deleted", length(files_to_delete), "old files."))
      print(files_to_delete)
    }
  } else {
    message("No old duplicate files found to clean.")
  }
}

# Assumes images are in a folder named "images"
clean_dated_files(directory = "images/NewEngland", dry_run = TRUE)
clean_dated_files(directory = "images/MidAtlantic", dry_run = T)
clean_dated_files(directory = "images/BothReports", dry_run = T)

clean_dated_files(directory = "images/NewEngland", dry_run = FALSE)
clean_dated_files(directory = "images/MidAtlantic", dry_run = FALSE)
clean_dated_files(directory = "images/BothReports", dry_run = FALSE)
