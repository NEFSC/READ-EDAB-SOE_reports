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
