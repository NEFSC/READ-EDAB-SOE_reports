dat <- stocksmart::stockAssessmentSummary |>
  dplyr::filter(.data$`Science Center` == "NEFSC") |>
  janitor::clean_names() |>
  dplyr::filter(!stringr::str_detect(.data$stock_name, "Eastern Georges")) |>
  dplyr::select(stock_name, jurisdiction, assessment_year, f_fmsy, b_bmsy)

short_name <- ecodata::stock_status |>
  dplyr::select(Stock, Code) |>
  dplyr::distinct()

recent_dat <- dat |>
  tidyr::drop_na() |>
  dplyr::left_join(short_name, by = c("stock_name" = "Stock")) |>
  dplyr::group_by(stock_name) |>
  dplyr::mutate(max_year = max(assessment_year, na.rm = TRUE)) |>
  dplyr::filter(assessment_year == max_year) |>
  dplyr::ungroup() |>
  dplyr::select(-max_year)

# plot with trajectories ----

dat |>
  tidyr::drop_na() |>
  ggplot2::ggplot() +
  ggplot2::geom_vline(xintercept = 1, linetype = "dotted") +
  ggplot2::geom_vline(xintercept = 0.5, linetype = "dashed") +
  ggplot2::geom_hline(yintercept = 1, linetype = "dashed") +
  # ggplot2::geom_point(
  #   ggplot2::aes(
  #     x = b_bmsy,
  #     y = f_fmsy,
  #     shape = jurisdiction,
  #     color = assessment_year
  #   ),
  #   alpha = 0.5
  # ) +
  ggplot2::geom_path(
    ggplot2::aes(
      x = b_bmsy,
      y = f_fmsy,
      group = stock_name
    ),
    color = "gray50",
    alpha = 0.5
  ) +
  ggrepel::geom_text_repel(
    ggplot2::aes(
      x = b_bmsy, #geom_text_repel auto-jitters text around points
      y = f_fmsy,
      label = Code
    ),
    data = recent_dat
  ) +
  ggplot2::geom_point(
    ggplot2::aes(
      x = b_bmsy,
      y = f_fmsy,
      shape = jurisdiction #,
      # color = assessment_year
    ),
    data = recent_dat,
    cex = 2
  ) +
  ggplot2::xlab(expression(~ B / B[msy])) +
  ggplot2::ylab(expression(~ F / F[msy])) +
  ggplot2::scale_color_viridis_c() +
  ecodata::theme_ts() +
  ecodata::theme_title() +
  ggplot2::facet_wrap(~jurisdiction, scales = "free", ncol = 1) +
  ggplot2::theme(legend.position = "bottom", legend.direction = "vertical")

ggplot2::ggsave(
  "images/stock_assessment_trajectories.png",
  width = 6,
  height = 12,
  units = "in",
  dpi = 72
)

# plot with NA values ----

recent_dat2 <- dat |>
  dplyr::left_join(short_name, by = c("stock_name" = "Stock")) |>
  dplyr::group_by(stock_name) |>
  dplyr::mutate(max_year = max(assessment_year, na.rm = TRUE)) |>
  dplyr::filter(assessment_year == max_year) |>
  dplyr::ungroup() |>
  dplyr::select(-max_year)

bbmsy_only <- recent_dat2 |>
  dplyr::filter(is.na(f_fmsy) & !is.na(b_bmsy))

p1 <- recent_dat2 |>
  dplyr::filter(jurisdiction == "NEFMC") |>
  ggplot2::ggplot() +
  ggplot2::geom_vline(xintercept = 1, linetype = "dotted") +
  ggplot2::geom_vline(xintercept = 0.5, linetype = "dashed") +
  ggplot2::geom_hline(yintercept = 1, linetype = "dashed") +
  ggplot2::geom_point(
    ggplot2::aes(
      x = b_bmsy,
      y = f_fmsy
    )
  ) +
  ggrepel::geom_text_repel(
    ggplot2::aes(
      x = b_bmsy, #geom_text_repel auto-jitters text around points
      y = f_fmsy,
      label = Code
    )
  ) +
  ggplot2::xlab(expression(~ B / B[msy])) +
  ggplot2::ylab(expression(~ F / F[msy])) +
  ggplot2::scale_color_viridis_c() +
  ecodata::theme_ts() +
  ecodata::theme_title() +
  # ggplot2::facet_wrap(~jurisdiction, scales = "free", ncol = 1) +
  ggplot2::theme(legend.position = "bottom", legend.direction = "vertical")

p2 <- bbmsy_only |>
  dplyr::mutate(jurisdiction == "NEFMC") |>
  dplyr::mutate(Cat = "No F/Fmsy\nEstimate") |>
  ggplot2::ggplot() +
  ggplot2::geom_point(
    ggplot2::aes(
      x = b_bmsy,
      y = 0
    )
  ) +

  ggplot2::geom_vline(xintercept = 1, linetype = "dotted") +
  ggplot2::geom_vline(xintercept = 0.5, linetype = "dashed") +
  ggrepel::geom_text_repel(
    ggplot2::aes(
      x = b_bmsy, #geom_text_repel auto-jitters text around points
      y = 0,
      label = Code
    ),
    data = bbmsy_only
  ) +
  ggplot2::xlab(expression(~ B / B[msy])) +
  ggplot2::ylab(expression(~ F / F[msy])) +
  ggplot2::scale_color_viridis_c() +
  ecodata::theme_ts() +
  ecodata::theme_title() +
  ggplot2::facet_wrap(~Cat, strip.position = "left") +
  ggplot2::theme(
    legend.position = "bottom",
    legend.direction = "vertical",
    axis.ticks = ggplot2::element_blank(),
    axis.text = ggplot2::element_blank(),
    axis.title = ggplot2::element_blank(),
    strip.background = ggplot2::element_rect(fill = "white")
  ) +
  ggplot2::xlim(c(0, max(recent_dat2$b_bmsy, na.rm = TRUE))) +
  ggplot2::ylim(-1, 1)
p2

ggpubr::ggarrange(
  p2,
  # ggh4x::force_panelsizes(ggplot2::unit(1, "in"), ggplot2::unit(6, "in")),
  p1,
  # ggh4x::force_panelsizes(ggplot2::unit(4.75, "in"), ggplot2::unit(6, "in")),
  ncol = 1,
  heights = c(1, 5)
)

ggplot2::ggsave(
  "images/stock_assessment_bbmsy.png",
  width = 7,
  height = 7,
  units = "in",
  dpi = 72,
  bg = "white"
)

# try faceting ----

recent_dat2 <- dat |>
  dplyr::left_join(short_name, by = c("stock_name" = "Stock")) |>
  dplyr::group_by(stock_name) |>
  dplyr::mutate(max_year = max(assessment_year, na.rm = TRUE)) |>
  dplyr::filter(assessment_year == max_year) |>
  dplyr::ungroup() |>
  dplyr::select(-max_year) |>
  dplyr::mutate(
    f_fmsy = dplyr::case_when(
      is.na(f_fmsy) & !is.na(b_bmsy) ~ 100,
      TRUE ~ f_fmsy
    ),
    Cat = dplyr::case_when(f_fmsy == 100 ~ FALSE, TRUE ~ TRUE),
    Code = dplyr::case_when(
      stock_name == "Atlantic salmon - Gulf of Maine" ~ "Salmon",
      stock_name == "Atlantic wolffish - Gulf of Maine / Georges Bank" ~
        "Wolffish",
      TRUE ~ Code
    )
  )


plt_dat <- recent_dat2 |>
  dplyr::filter(stringr::str_detect(jurisdiction, "NEFMC"))

max_f <- max(plt_dat$f_fmsy[which(plt_dat$f_fmsy < 100)], na.rm = TRUE)

ybreaks <- seq(
  0,
  ifelse(max_f > 1, max_f, 1),
  by = 0.25
)

plt_dat |>
  ggplot2::ggplot() +
  ggplot2::geom_vline(xintercept = 1, linetype = "dotted") +
  ggplot2::geom_vline(xintercept = 0.5, linetype = "dashed") +
  ggplot2::geom_hline(
    ggplot2::aes(yintercept = yval),
    linetype = "dashed",
    data = tibble::tibble(Cat = c(TRUE, FALSE), yval = c(1, NA))
  ) +
  ggplot2::geom_point(
    ggplot2::aes(
      x = b_bmsy,
      y = f_fmsy
    )
  ) +
  ggrepel::geom_text_repel(
    ggplot2::aes(
      x = b_bmsy, #geom_text_repel auto-jitters text around points
      y = f_fmsy,
      label = Code
    ),
    max.overlaps = 50
  ) +
  ggplot2::scale_y_continuous(
    breaks = c(ybreaks, 100),
    labels = c(ybreaks, "No F/Fmsy\nEstimate")
  ) +
  ggplot2::xlab(expression(~ B / B[msy])) +
  ggplot2::ylab(expression(~ F / F[msy])) +
  ggplot2::scale_color_viridis_c() +
  ecodata::theme_ts() +
  ecodata::theme_title() +
  ggplot2::facet_grid(
    rows = ggplot2::vars(Cat),
    scales = "free_y",
    space = "free_y"
  ) +
  ggplot2::theme(
    legend.position = "bottom",
    legend.direction = "vertical",
    strip.text = ggplot2::element_blank(),
    strip.background = ggplot2::element_blank()
  )

ggplot2::ggsave(
  "images/stock_assessment_bbmsy_ne.png",
  width = 6.5,
  height = 5,
  units = "in",
  dpi = 300,
)

# update ecodata function ----

#' plot stock_Status
#'
#' Kobe plots of regional stock status
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#'
#' @return list of 2 items
#'
#' \item{p}{ggplot object}
#' \item{unknown}{data frame listing stocks with unklnown status}
#'
#'
#' @export
#'

plot_stock_status <- function(shadedRegion = NULL, report = "MidAtlantic") {
  # generate plot setup list (same for all plot functions)
  setup <- ecodata::plot_setup(shadedRegion = shadedRegion, report = report)

  # which report? this may be bypassed for some figures
  if (report == "MidAtlantic") {
    councils <- c("MAFMC", "Both")
  } else {
    councils <- c("NEFMC", "Both")
  }

  # optional code to wrangle ecodata object prior to plotting
  # e.g., calculate mean, max or other needed values to join below
  fix <- ecodata::stock_status |>
    dplyr::mutate(
      Code = dplyr::recode(Code, "Dogfish" = "Sp. Dogfish"),
      Code = dplyr::recode(Code, "Mackerel" = "At. Mackerel")
    ) |>
    # add codes for stocks missing them
    dplyr::mutate(
      Code = dplyr::case_when(
        Stock == "Atlantic salmon - Gulf of Maine" ~ "Salmon",
        Stock == "Atlantic wolffish - Gulf of Maine / Georges Bank" ~
          "Wolffish",
        TRUE ~ Code
      )
    )
  fix <- tidyr::pivot_wider(fix, names_from = Var, values_from = Value) |>
    dplyr::filter(Council %in% councils) |>
    (\(.) {
      . ->> unfiltered
    })() |>
    dplyr::group_by(Stock) |>
    dplyr::mutate(
      score = dplyr::case_when(
        (F.Fmsy < 1 & B.Bmsy > 1.0) ~ "a",
        (F.Fmsy < 1 & B.Bmsy > 0.5 & B.Bmsy < 1) ~ "b",
        (F.Fmsy > 1 | B.Bmsy < 0.5) ~ "c",
      )
    ) |>
    dplyr::mutate(Council = dplyr::recode(Council, "Both" = "NEFMC/MAFMC")) |>
    # recode so we can plot stocks with missing ffmsy
    dplyr::mutate(
      F.Fmsy = dplyr::case_when(
        is.na(F.Fmsy) & !is.na(B.Bmsy) ~ 100,
        TRUE ~ F.Fmsy
      ),
      Cat = dplyr::case_when(F.Fmsy == 100 ~ FALSE, TRUE ~ TRUE)
    ) |>
    dplyr::filter(!is.na(B.Bmsy))

  unknown <- unfiltered |>
    dplyr::filter(is.na(F.Fmsy) & is.na(B.Bmsy)) |>
    dplyr::select(Stock, F.Fmsy, B.Bmsy)

  # code for generating plot object p
  # ensure that setup list objects are called as setup$...
  # e.g. fill = setup$shade.fill, alpha = setup$shade.alpha,
  # xmin = setup$x.shade.min , xmax = setup$x.shade.max
  #

  max_f <- max(fix$F.Fmsy[which(fix$F.Fmsy < 100)], na.rm = TRUE)

  ybreaks <- seq(
    0,
    ifelse(max_f > 1, max_f, 1),
    by = 0.25
  )

  offset_y <- fix |>
    dplyr::mutate(
      new_ffmsy = dplyr::case_when(
        F.Fmsy == 100 ~ F.Fmsy + rnorm(n = dplyr::n(), mean = 0, sd = 0.15),
        TRUE ~ F.Fmsy
      )
    )

  ## TODO: add shading to background, don't color by status
  p <- fix |>
    ggplot2::ggplot() +
    ggplot2::geom_vline(xintercept = 1, linetype = "dotted") +
    ggplot2::geom_vline(xintercept = 0.5, linetype = "dashed") +
    # don't plot hline for missing ffmsy
    ggplot2::geom_hline(
      ggplot2::aes(yintercept = yval),
      linetype = "dashed",
      data = tibble::tibble(Cat = c(TRUE, FALSE), yval = c(1, NA))
    ) +
    ggplot2::geom_point(ggplot2::aes(
      x = B.Bmsy,
      y = F.Fmsy,
      shape = Council,
      color = as.factor(Code)
    )) +
    ggrepel::geom_text_repel(
      ggplot2::aes(
        x = B.Bmsy, #geom_text_repel auto-jitters text around points
        y = new_ffmsy, # extra jittering for missing ffmsy points
        label = Code,
        color = as.factor(Code)
      ),
      data = offset_y,
      max.overlaps = 50,
      min.segment.length = 20
      # direction = "y"
      # max.time = 5,
      # max.iter = 10^6#,
      # force = 5,
      # force_pull = 0,
      # box.padding = 1,
    ) +
    ggplot2::scale_y_continuous(
      breaks = c(ybreaks, 100),
      labels = c(ybreaks, "No F/Fmsy\nEstimate")
    ) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(Cat),
      scales = "free_y",
      space = "free_y"
    ) +
    # ggplot2::scale_color_manual(
    #   values = c("c" = "#D95F02", "b" = "#7570B3", "a" = "#1B9E77")
    # ) +
    ggplot2::xlab(expression(~ B / B[msy])) +
    ggplot2::ylab(expression(~ F / F[msy])) +
    ggplot2::guides(color = "none") +
    ggplot2::ggtitle(paste0(report, ": stock status")) +
    ecodata::theme_ts() +
    ecodata::theme_title() +
    ggplot2::theme(
      legend.position = "bottom",
      legend.direction = "vertical",
      strip.text = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank()
    ) +
    ggplot2::guides(color = "none")

  return(list(p = p, unknown = unknown))
}

plot_stock_status(report = "NewEngland")
ggplot2::ggsave(
  here::here("images/ne_stock_status_test2.png"),
  width = 6.5,
  height = 5
)
plot_stock_status()
