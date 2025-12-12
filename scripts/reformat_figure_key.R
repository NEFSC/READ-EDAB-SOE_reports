data <- read.csv(here::here("utils/figure_captions_summary.csv"))


# remove data suffixes

data2 <- data |>
  dplyr::mutate_all(
    ~ stringr::str_remove(
      .,
      "(?<=(NewEngland|MidAtlantic|BothReports))_([[:digit:]]|[[:punct:]])+png$"
    )
  )

write.csv(
  data2,
  here::here("utils/figure_captions_summary.csv"),
  row.names = FALSE
)
