FitFlextableToPage <- function(ft, pgwidth = 6) {
  ft_out <- ft %>% flextable::autofit()

  ft_out <- flextable::width(
    ft_out,
    width = dim(ft_out)$widths *
      pgwidth /
      (flextable::flextable_dim(ft_out)$widths)
  )
  return(ft_out)
}

return_caption <- function(key = caption_key, chunk_name, region) {
  col <- dplyr::case_when(
    region == "MidAtlantic" ~ "fig_cap_ma",
    region == "NewEngland" ~ "fig_cap_ne",
    region == "BothReports" ~ "fig_cap_both"
  )
  # caption <- key$caption[which(key$chunk_name == chunk_name)]
  caption <- key |>
    dplyr::filter(.data$chunkName == chunk_name) |>
    dplyr::pull(col)
  return(caption)
}


return_filepath <- function(
  key = caption_key,
  chunk_name,
  region,
  mode = "pdf"
) {
  if (mode == "pdf") {
    col <- dplyr::case_when(
      region == "MidAtlantic" ~ "fig_path_ma",
      region == "NewEngland" ~ "fig_path_ne",
      region == "BothReports" ~ "fig_path_both"
    )
  } else if (mode == "slide") {
    col <- dplyr::case_when(
      region == "MidAtlantic" ~ "slide_fig_path_ma",
      region == "NewEngland" ~ "slide_fig_path_ne",
      region == "BothReports" ~ "slide_fig_path_both"
    )
  }

  # message(col)

  filepath <- key |>
    dplyr::filter(.data$chunkName == chunk_name) |>
    dplyr::pull(col)
  return(filepath)
}

# return_plot <- function(
#   name,
#   cap_key = caption_key,
#   ... # passes region to return_filepath
# ) {
#   file <- return_filepath(key = cap_key, chunk_name = name, ...) |>
#     here::here()
#
#   if (file.exists(file) & file.info(file)$isdir == FALSE) {
#     file |>
#       knitr::include_graphics(dpi = 300)
#   } else if (!file.exists(file)) {
#     # key could include the file name without the date or png extension
#     new_file <- list.files(
#       path = here::here("images"),
#       pattern = paste0('^',basename(file)),
#       full.names = TRUE,
#       recursive = TRUE
#     )[1] # pick the first file if there are multiple
#     new_file |>
#       knitr::include_graphics(dpi = 300)
#   } else {
#     stop(paste0("Cannot find file specified: ", file))
#   }
# }
return_plot <- function(
  name,
  cap_key = caption_key,
  caption_col = NULL,
  ... # passes arguments to return_filepath
) {
  # 1. Internal Helper: Resolve Path for a Single Name
  find_single_path <- function(single_name) {
    file <- return_filepath(key = cap_key, chunk_name = single_name, ...) |>
      here::here()

    if (file.exists(file) && file.info(file)$isdir == FALSE) {
      return(file)
    }

    # Regex fallback
    new_file <- list.files(
      path = here::here("images"),
      pattern = paste0('^', basename(file)),
      full.names = TRUE,
      recursive = TRUE
    )[1]

    if (is.na(new_file)) {
      stop(paste0("Cannot find file specified: ", file))
    }
    return(new_file)
  }

  # 2. Get all file paths
  image_paths <- vapply(name, find_single_path, FUN.VALUE = character(1))

  # 3. Use 'magick' to read and combine images
  #    This preserves original pixel dimensions (fixing the "shrinking" issue)

  # only do this if there are multiple images to combine
  if (length(image_paths) == 1) {
    return(knitr::include_graphics(image_paths, dpi = 300))
  }
  image_type = tools::file_ext(image_paths)
  if (image_type[1] == 'pdf') {
    loaded_images <- magick::image_read_pdf(image_paths)
  } else {
    loaded_images <- magick::image_read(image_paths)
  }

  # stack = FALSE appends them horizontally (side-by-side)
  combined_image <- magick::image_append(loaded_images, stack = FALSE)

  # 4. Write to a file so include_graphics can read it
  #    We create a file with the same extension as the first input
  # ext <- tools::file_ext(image_paths[1])

  # get dir
  dir_name <- dirname(image_paths[1])
  base_name <- basename(image_paths[1])
  processed_name <- paste0(dir_name, "/processed_", base_name)

  # processed_file <- tempfile(fileext = paste0(".", ext))
  magick::image_write(combined_image, path = processed_name)

  # 5. Handle Caption dynamically
  #    We force the current chunk's fig.cap option to update based on the key
  if (!is.null(caption_col)) {
    cap_text <- cap_key[cap_key$chunkName == name[1], caption_col]

    if (length(cap_text) > 0 && !is.na(cap_text)) {
      # This pushes the caption into the Markdown chunk options
      knitr::opts_current$set(fig.cap = cap_text)
    }
  }

  # 6. Return the single combined image
  knitr::include_graphics(processed_name, dpi = 300)
}

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
