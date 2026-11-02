cat_key <- tibble::tibble(
  orig = c(
    "system_level",
    "management",
    "forecasts",
    "system_drivers",
    "functional_group",
    "stock_level",
    "admin"
  ),
  new = c(
    "System level thresholds/ ref pts",
    "Management",
    "Short term forecasts",
    "Multiple system drivers",
    "Functional group level status/ thresholds/ ref pts",
    "Stock level indicators",
    "SOE admin"
  )
)

levels_priority <- c("Unranked", "Lowest", "Low", "Moderate", "High", "Highest")

requests <- read.csv(here::here(
  "utils/SOE Prioritization 2025_2026-03-19.csv"
)) |>
  dplyr::left_join(cat_key, by = c("Category" = "orig")) |>
  dplyr::mutate(Category = new) |>
  dplyr::select(-new)

requests$Priority <- factor(requests$Priority, levels = levels_priority)

create_table <- function(data, cap, widths) {
  tbl <- data |>
    flextable::as_grouped_data(groups = "Category") |>
    flextable::as_flextable(hide_grouplabel = TRUE) |>
    flextable::width(width = widths) |>
    flextable::align(i = ~ !is.na(Category), align = "left") |>
    flextable::bold(i = ~ !is.na(Category), bold = TRUE) |>
    flextable::theme_zebra() |>
    flextable::bg(i = ~ !is.na(Category), bg = "beige", part = "body") |>
    flextable::hline(i = ~ !is.na(Category)) |>
    flextable::set_caption(cap)

  return(tbl)
}

ret_widths <- function(n) {
  out <- rep(2 / n, n)
  return(out)
}

completed <- requests |>
  dplyr::filter(Status == "Done") |>
  dplyr::select(-Status) |>
  dplyr::group_by(Category) |>
  dplyr::mutate(n = dplyr::n()) |>
  dplyr::arrange(n) |>
  dplyr::arrange(n, Category, desc(Priority)) |>
  dplyr::select(-n) |>
  create_table(
    widths = c(4.5, ret_widths(2)),
    cap = "Requests that have been completed as of the 2026 State of the Ecosytem reports."
  )
