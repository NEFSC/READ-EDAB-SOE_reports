### automated captions ----
library(dplyr)
library(stringr)
library(glue)
library(purrr)

#' Format p-values to 3 decimal places or '< 0.001'
format_pval <- function(pval) {
  if (is.na(pval)) {
    return("p = NA")
  }
  if (pval < 0.001) {
    return("p < 0.001")
  } else {
    return(sprintf("p = %.3f", pval))
  }
}

#' Extract and format clean uppercase series labels
get_series_label <- function(df_row) {
  # Prefer Group2 (e.g. Albatross / Bigelow) or Group1 if Group2 is absent
  groups <- c(df_row$Group1, df_row$Group2, df_row$Group3)
  groups <- groups[!is.na(groups) & groups != "" & tolower(groups) != "unit"]

  if (length(groups) > 0) {
    label <- paste(groups, collapse = " - ")
  } else {
    label <- df_row$Indicator
  }
  return(toupper(label))
}

#' Describe a single series according to the new specification
describe_series <- function(row) {
  label <- get_series_label(row)
  recent_yr <- row$stats.recent_year
  status <- ifelse(
    is.na(row$stats.status),
    "status unrecorded",
    tolower(row$stats.status)
  )

  # ----------------------------------------------------
  # 1. Long-Term Trend
  # ----------------------------------------------------
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
    lt_str <- "No long-term trend evaluated."
  } else {
    sig_ar1 <- isTRUE(row$trends.sig_long_term_linear_ar1_error)
    sig_normal <- isTRUE(row$trends.sig_long_term_linear_normal_error)
    p_ar1 <- format_pval(row$trends.pval_long_term_linear_ar1_error)
    p_norm <- format_pval(row$trends.pval_long_term_linear_normal_error)
    dir_lt <- row$trends.trend_long_term_linear_normal_error

    # Check if autocorrelation drove the simple linear trend
    # (Significant under standard normal error, but non-significant under AR1)
    aicc_normal <- row$trends.aicc_long_term_linear_normal_error
    aicc_ar1 <- row$trends.aicc_long_term_linear_ar1_error
    ar1_favored <- (!is.na(aicc_normal) &&
      !is.na(aicc_ar1) &&
      (aicc_normal - aicc_ar1 > 2))

    is_ar_driven <-
      # linear model is sig and AR1 model is not sig
      (sig_normal && !sig_ar1)

    is_partly_ar_driven <-
      # AR1 model has a lower AIC, both models are sig
      (ar1_favored && sig_ar1 && sig_normal)

    ar_clause <- if (is_ar_driven) {
      ", driven by autoregression"
    } else if (is_partly_ar_driven) {
      ", partly driven by autoregression"
    } else {
      ""
    }

    if (sig_ar1) {
      lt_str <- glue(
        "Significant {dir_lt} long-term trend ({p_norm}){ar_clause}."
      )
    } else {
      lt_str <- glue("No long-term trend ({p_norm}).")
    }
  }

  # ----------------------------------------------------
  # 2. Short-Term Trend
  # ----------------------------------------------------
  st_sig <- isTRUE(row$trends.sig_short_term_ar1)
  st_p <- format_pval(row$trends.pval_short_term_ar1)
  st_dir <- row$trends.trend_short_term_ar1
  st_years_raw <- as.character(row$trends.n_years_short_term_ar1)

  # Extract numeric count of years
  st_num_years <- str_extract(st_years_raw, "^[0-9]+")

  # Omit duration if it is the standard 10 years; display if custom (e.g. 15, 25 years)
  if (!is.na(st_num_years) && st_num_years != "10") {
    st_timing <- glue("{st_num_years} years, ")
  } else {
    st_timing <- ""
  }

  if (st_sig) {
    st_str <- glue("Significant {st_dir} short-term trend ({st_timing}{st_p}).")
  } else {
    st_str <- glue("No short-term trend ({st_timing}{st_p}).")
  }

  # ----------------------------------------------------
  # Full Series String
  # ----------------------------------------------------
  glue("{label}: {recent_yr}, {status}. {lt_str} {st_str}")
}

#' Generate complete plot caption
#'
#' @param plot_df Data frame containing row(s) for a single plot
#' @param line_break Formatting for line breaks: "markdown" ("  \n") or "latex" ("\\newline{}\n")
generate_plot_caption <- function(plot_df, line_break = "markdown") {
  n_series <- nrow(plot_df)
  sep_str <- " "

  series_captions <- vapply(
    seq_len(n_series),
    function(i) {
      as.character(describe_series(plot_df[i, ]))
    },
    character(1)
  )

  paste(series_captions, collapse = sep_str)
}

#' Pipeline function to process all plots in the spreadsheet
#'
#' @param csv_file Path to figure_stats_summaries3.csv
generate_all_plot_captions <- function(csv_file) {
  df <- read.csv(csv_file, stringsAsFactors = FALSE)

  captions_df <- df %>%
    group_by(File_Generated) %>%
    group_split() %>%
    map_dfr(function(plot_df) {
      file_path <- plot_df$File_Generated[1]
      indicator_name <- paste(unique(plot_df$Indicator), collapse = ", ")
      region_name <- plot_df$Region[1]
      num_datasets <- nrow(plot_df)
      caption <- generate_plot_caption(plot_df)

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

## run code ----
# # Load data and run caption generation
# captions <- generate_all_plot_captions(
#   here::here("utils/figure_stats_summaries3.csv")
# )
#
# # # Display first few generated plot captions
# cat(captions$Caption[1])
# cat("\n\n-----------------------------------\n\n")
# cat(captions$Caption[2])
# #
# # write.csv(captions, here::here("utils/automated.captions.csv"))
