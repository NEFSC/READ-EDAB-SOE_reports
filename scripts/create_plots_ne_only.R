####################################
#' Run NE only report plots
#'
#' This function creates all plots that are solely for the NE report
#'
#' @param region Region for which to create report plots ("NewEngland")
#' 

region <- "NewEngland"
create_plots_ne <- function(region = "NewEngland")
{
  out_dir <- here::here("images", region)
  
  if (!dir.exists(out_dir)) {
    dir.create(out_dir)
  }
  
  region2 <- dplyr::case_when(
    region == "NewEngland" ~ "New England"
  )
  
  full_region <- dplyr::case_when(
    region == "NewEngland" ~ "New England"
  )
  
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
  
  # calfin center of gravity -- NE only
  save_plot(
    plot_expression = {
      ecodata::plot_zooplankton_index(
        report = region,
        varName = 'Calfin',
        plottype = 'cog',
        n = 10
      )+
        ggplot2::theme(legend.position = 'bottom')
    },
    indicator = "calfin_cog",
    width = 6.5,
    height = 4
  )
  
  # mass inshore survey -- NE only
  save_plot(
    plot_expression = {
      ecodata::plot_mass_inshore_survey(report = region, n = 10) +
        ggplot2::geom_point()+
        ggplot2::geom_line()
    },
    indicator = "mass_inshore",
    width = 6,
    height = 6
  )
  
  # seabird productivity -- NE only
  if (region == "NewEngland") {
    save_plot(
      plot_expression = {
        ecodata::plot_seabird_ne(varName = "productivity", n = 10) +
          ggplot2::coord_cartesian(xlim = c(1991, 2025))
      },
      indicator = "seabird_productivity",
      width = 6.5,
      height = 2.5
    )
  }
  
  # salmon -- NE only
  if (region == "NewEngland") {
    save_plot(
      plot_expression = {
        ecodata::plot_gom_salmon(n = 10) +
          ggplot2::facet_wrap(~Var, nrow = 2, scales = "free_y",
                              strip.position = "left",
                              labeller = ggplot2::as_labeller(c(Total = "Number of Salmon", PSAR = "Percent Return Rate")))
      },
      indicator = "salmon",
      width = 6.5,
      height = 4
    )
  }
  
  # WBTS Zoo - NE only
  if (region == "NewEngland") {
    save_plot(
      plot_expression = {
        ecodata::plot_wbts_zoo(report = region, n = 10)
      },
      indicator = "wbts_zoo",
      width = 6.5,
      height = 4
    )
  }
}
create_plots_ne(region = "NewEngland")