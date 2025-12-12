set.seed(12345)
dat <- tibble::tibble(
  year = seq(1985, 2025),
  `Indicator 1` = c(
    rnorm(31, 20, 3) + (1:31) * 0.1,
    rnorm(10, 20, 3) + (32:41) * 0.2
  ),
  `Indicator 2` = c(
    rnorm(31, 20, 2) - (1:31) * 0.3,
    rnorm(10, 8, 1) + (10:19) * 0.5
  )
) |>
  tidyr::pivot_longer(cols = -year, names_to = "variable", values_to = "value")

plt <- dat |>
  ggplot2::ggplot(ggplot2::aes(x = year, y = value)) +
  ggplot2::geom_point() +
  ggplot2::geom_line() +
  ecodata::geom_gls() +
  ecodata::geom_lm(n = 10) +
  ggplot2::facet_grid(~variable) +
  ecodata::theme_ts() +
  ggplot2::labs(x = "Year", y = "Indicator value")

ggplot2::ggsave(
  plot = plt,
  filename = here::here("images/plot_key.png"),
  width = 6,
  height = 4,
  units = "in",
  dpi = 300
)
