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

return_caption <- function(
  key = caption_key,
  chunk_name,
  region,
  automated_cap = here::here("utils/automated.captions.csv")
) {
  col <- dplyr::case_when(
    region == "MidAtlantic" ~ "fig_cap_ma",
    region == "NewEngland" ~ "fig_cap_ne",
    region == "BothReports" ~ "fig_cap_both"
  )
  # caption <- key$caption[which(key$chunk_name == chunk_name)]
  caption <- key |>
    dplyr::filter(chunkName == chunk_name) |>
    dplyr::pull(col)

  ## TODO: this matching relies on the Indicator column in the automated captions file being a substring of the chunk_name.
  # relies on updated chunkName column in the manual caption spreadsheet
  auto_cap <- read.csv(automated_cap) |>
    dplyr::filter(stringr::str_detect(
      Indicator |>
        stringr::str_remove_all("_") |>
        stringr::str_remove_all("-"),
      chunk_name |>
        stringr::str_remove_all("_") |>
        stringr::str_remove_all("-")
    )) |>
    dplyr::pull(Caption)

  output <- paste0(caption, auto_cap)
  return(output)
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

### automated captions ----
library(dplyr)
library(stringr)
library(purrr)
library(glue)

#' Format p-value into publication standard
format_pval <- function(pval) {
  if (is.na(pval)) {
    return("N/A")
  }
  if (pval < 0.001) {
    return("p < 0.001")
  } else {
    return(sprintf("p = %.3f", pval))
  }
}

#' Generate concise series label from Group and Indicator columns
get_series_label <- function(df_row) {
  groups <- c(df_row$Group1, df_row$Group2, df_row$Group3)
  groups <- groups[!is.na(groups) & groups != "" & tolower(groups) != "unit"]

  if (length(groups) > 0) {
    label <- paste(groups, collapse = " - ")
  } else {
    label <- df_row$Indicator
  }
  return(label)
}

#' Describe a single dataset/series within a plot
#'
#' @param row A single row data.frame / tibble
#' @param lt_model Choice of long-term model: "ar1" (default), "normal", or "aicc" (select lower AICc)
describe_series <- function(row, lt_model = "ar1") {
  label <- get_series_label(row)
  recent_yr <- row$stats.recent_year
  status <- row$stats.status

  # --- 1. Long-Term Trend Analysis ---
  # Check if long-term model was executed
  lt_notes <- c(
    row$trends.n_years_long_term_linear_ar1_error,
    row$trends.n_years_long_term_linear_normal_error
  )
  lt_untested <- any(
    str_detect(lt_notes, "less than 30 years"),
    na.rm = TRUE
  ) ||
    is.na(row$trends.sig_long_term_linear_ar1_error)

  if (lt_untested) {
    lt_text <- "Long-term trends were not evaluated (< 30 years of data)"
  } else {
    # Determine model preference
    use_ar1 <- TRUE
    if (
      lt_model == "aicc" &&
        !is.na(row$trends.aicc_long_term_linear_ar1_error) &&
        !is.na(row$trends.aicc_long_term_linear_normal_error)
    ) {
      use_ar1 <- row$trends.aicc_long_term_linear_ar1_error <=
        row$trends.aicc_long_term_linear_normal_error
    } else if (lt_model == "normal") {
      use_ar1 <- FALSE
    }

    if (use_ar1) {
      sig <- row$trends.sig_long_term_linear_ar1_error
      dir <- row$trends.trend_long_term_linear_ar1_error
      pval <- format_pval(row$trends.pval_long_term_linear_ar1_error)
      mod_type <- "AR1"
    } else {
      sig <- row$trends.sig_long_term_linear_normal_error
      dir <- row$trends.trend_long_term_linear_normal_error
      pval <- format_pval(row$trends.pval_long_term_linear_normal_error)
      mod_type <- "linear normal"
    }

    if (isTRUE(sig)) {
      lt_text <- glue(
        "a statistically significant long-term {dir} trend ({mod_type}, {pval}) was detected"
      )
    } else {
      lt_text <- glue(
        "no statistically significant long-term trend was detected ({mod_type}, {pval})"
      )
    }
  }

  # --- 2. Short-Term Trend Analysis ---
  st_nyears <- row$trends.n_years_short_term_ar1
  st_sig <- row$trends.sig_short_term_ar1
  st_dir <- row$trends.trend_short_term_ar1
  st_pval <- format_pval(row$trends.pval_short_term_ar1)

  if (isTRUE(st_sig)) {
    st_text <- glue(
      "A statistically significant short-term {st_dir} trend ({st_pval}) was observed over the last {st_nyears}"
    )
  } else {
    st_text <- glue(
      "No statistically significant short-term trend was detected over the last {st_nyears} ({st_pval})"
    )
  }

  # --- 3. Assemble Series Description ---
  res <- list(
    label = label,
    recent_year = recent_yr,
    status = status,
    lt_untested = lt_untested,
    lt_text = lt_text,
    st_text = st_text
  )

  return(res)
}

#' Generate a complete, formatted caption for a single plot (single or multi-series)
#'
#' @param plot_df Data frame containing rows corresponding to a single plot
#' @param lt_model Long-term model choice ("ar1", "normal", "aicc")
generate_plot_caption <- function(plot_df, lt_model = "ar1") {
  n_series <- nrow(plot_df)
  file_name <- basename(plot_df$File_Generated[1])
  region <- plot_df$Region[1]

  series_descriptions <- lapply(seq_len(n_series), function(i) {
    describe_series(plot_df[i, ], lt_model = lt_model)
  })

  # Check homogeneity across series
  unique_years <- unique(sapply(series_descriptions, function(x) x$recent_year))

  # Single series caption
  if (n_series == 1) {
    s <- series_descriptions[[1]]
    status_str <- if (!is.na(s$status)) {
      glue(" Status is currently {s$status}.")
    } else {
      ""
    }
    caption <- glue(
      "Plot ({s$label}): Data extends through {s$recent_year}.{status_str} ",
      "{s$lt_text}. ",
      "{s$st_text}."
    )
    return(caption)
  }

  # Multi-series caption logic
  caption_header <- if (length(unique_years) == 1) {
    glue(
      "Plot Summary ({n_series} datasets, Region: {region}): Data across all series extends through {unique_years[1]}."
    )
  } else {
    glue(
      "Plot Summary ({n_series} datasets, Region: {region}): Data timeframes vary across series."
    )
  }

  # Individual series details
  series_lines <- sapply(series_descriptions, function(s) {
    status_str <- if (!is.na(s$status)) glue("{s$status}") else ""
    glue(
      "• {s$label} (Most recent year: {s$recent_year}, {status_str}): {str_to_sentence(s$lt_text)}. {str_to_sentence(s$st_text)}."
    )
  })

  full_caption <- paste(
    "\\newline",
    "\\newline",
    c(caption_header, series_lines),
    collapse = "\\newline"
  )
  return(full_caption)
}

#' Pipeline function to process all plots in the spreadsheet
#'
#' @param csv_file Path to figure_stats_summaries3.csv
#' @param lt_model Model preference ("ar1", "normal", "aicc")
generate_all_plot_captions <- function(csv_file, lt_model = "ar1") {
  df <- read.csv(csv_file, stringsAsFactors = FALSE)

  captions_df <- df %>%
    group_by(File_Generated) %>%
    group_split() %>%
    map_dfr(function(plot_df) {
      file_path <- plot_df$File_Generated[1]
      indicator_name <- paste(unique(plot_df$Indicator), collapse = ", ")
      region_name <- plot_df$Region[1]
      num_datasets <- nrow(plot_df)
      caption <- generate_plot_caption(plot_df, lt_model = lt_model)

      tibble::tibble(
        File_Generated = file_path,
        Indicator = indicator_name,
        Region = region_name,
        Num_Datasets = num_datasets,
        Caption = caption
      )
    })

  return(captions_df)
}

# Load data and run caption generation
captions <- generate_all_plot_captions(
  here::here("utils/figure_stats_summaries3.csv"),
  lt_model = "ar1"
)

# Display first few generated plot captions
cat(captions$Caption[1])
cat("\n\n-----------------------------------\n\n")
cat(captions$Caption[2])

write.csv(captions, here::here("utils/automated.captions.csv"))
