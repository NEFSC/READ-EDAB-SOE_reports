find_all_files <- function(text, path = here::here()) {
  all_files <- c(
    list.files(path, recursive = TRUE, full.names = TRUE) |>
      stringr::str_subset("\\.R$"),
    list.files(path, recursive = TRUE, full.names = TRUE) |>
      stringr::str_subset("\\.Rmd$"),
    list.files(path, recursive = TRUE, full.names = TRUE) |>
      stringr::str_subset("\\.qmd$")
  )
  out <- c()
  for (i in seq_len(length(all_files))) {
    results <- grep(text, readLines(all_files[i]), value = FALSE) |>
      suppressWarnings()
    if (length(results) > 0) {
      results <- paste(results, collapse = ", ")
      this_data <- c(all_files[i], results)
      out <- rbind(out, this_data)
    }
    percent <- (i / length(all_files) * 100) |> round(digits = 0)
    if ((i %% 10) == 0) {
      print(paste(i, " files searched, ", percent, "% done", ".....", sep = ""))
    }
  }
  if (is.null(out)) {
    print("Not found")
  } else {
    colnames(out) <- c("file", "line(s)")
    return(out)
  }
}

create_contributors <- function(contrib.file, mode = "") {
  if (file.exists(contrib.file)) {
    contributors <- read.csv(contrib.file, stringsAsFactors = FALSE)

    # 1. Rejoin the names and apply the NEFSC logic
    reconstructed_entries <- contributors |>
      dplyr::arrange(Last_Name, .locale = "en") |>
      dplyr::mutate(
        # Combine names and trim in case Last_Name is empty
        Full_Name = stringr::str_trim(paste(First_Name, Last_Name)),
        # If affiliation is NEFSC, just show name; otherwise, show Name (Affiliation)
        formatted = ifelse(
          Affiliation == "NEFSC",
          Full_Name,
          paste0(Full_Name, " (", Affiliation, ")")
        )
      ) |>
      dplyr::pull(formatted)

    if (mode == "slide") {
      return(reconstructed_entries)
    }

    # 2. Collapse into a single comma-separated string
    final_string <- paste(
      c(reconstructed_entries, "NEFSC staff"),
      collapse = ", "
    )

    # 3. Add the header and render as Markdown
    prefix <- "**Contributors** (NEFSC unless otherwise noted): "
    cat(paste0(prefix, final_string))
  } else {
    cat("Contributor list file not found.")
  }
}
