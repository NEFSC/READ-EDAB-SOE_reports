## functions ----

# A function to create a standardized filename
create_filename <- function(
  indicator,
  file_region,
  dir = out_dir,
  extension = ".png"
) {
  file.path(
    dir,
    paste0(
      "slide_",
      indicator,
      "_",
      file_region,
      "_",
      Sys.Date(),
      extension
    )
  )
}

# A flexible function to generate and save a plot
save_plot <- function(
  plot_expression,
  indicator,
  report = region,
  save_dir = out_dir,
  ...
) {
  # Execute the code to create the plot
  p <- eval(plot_expression)

  # Check if the plot object is valid before saving
  if (inherits(p, "ggplot") || inherits(p, "ggarrange")) {
    message(report)
    message(indicator)
    message(out_dir)
    fname <- create_filename(
      indicator = indicator,
      file_region = report,
      dir = save_dir
    )
    ggplot2::ggsave(
      filename = fname,
      plot = p,
      bg = "white",
      ...
    )
    message("Plot saved to: ", fname)
  } else {
    stop("Plot object is not a valid ggplot or ggarrange object.")
  }
}
