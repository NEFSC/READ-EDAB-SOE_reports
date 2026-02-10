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

return_plot <- function(
  name,
  cap_key = caption_key,
  ... # passes region to return_filepath
) {
  file <- return_filepath(key = cap_key, chunk_name = name, ...) |>
    here::here()

  if (file.exists(file) & file.info(file)$isdir == FALSE) {
    file |>
      knitr::include_graphics(dpi = 300)
  } else if (!file.exists(file)) {
    # key could include the file name without the date or png extension
    new_file <- list.files(
      path = here::here("images"),
      pattern = paste0('^',basename(file)),
      full.names = TRUE,
      recursive = TRUE
    )[1] # pick the first file if there are multiple
    new_file |>
      knitr::include_graphics(dpi = 300)
  } else {
    stop(paste0("Cannot find file specified: ", file))
  }
}
