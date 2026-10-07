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
    ## TODO: the default is manually passed model preference
    ## we might prefer to automatically select the model with the lowest AICc if both were run (lt_model = "aicc")
    ## or select the model that is statistically significant if it has higher AICc
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

# # Load data and run caption generation
# captions <- generate_all_plot_captions(
#   here::here("utils/figure_stats_summaries3.csv"),
#   lt_model = "ar1"
# )
#
# # Display first few generated plot captions
# cat(captions$Caption[1])
# cat("\n\n-----------------------------------\n\n")
# cat(captions$Caption[2])
#
# write.csv(captions, here::here("utils/automated.captions.csv"))
