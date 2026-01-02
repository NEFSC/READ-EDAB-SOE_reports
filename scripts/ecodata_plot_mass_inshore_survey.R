#' plot massachusetts inshore data
#'
#' Plots mass_inshore_survey
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#' @param n Numeric scalar. Number of years used (from most recent year) to estimate short term trend . Default = 0 (No trend calculated)
#'
#' @return ggplot object
#'
#'
#' @export
#'

plot_mass_inshore_survey <- function(
  shadedRegion = NULL,
  report = "MidAtlantic",
  n = 0
) {
  # generate plot setup list (same for all plot functions)
  setup <- ecodata::plot_setup(shadedRegion = shadedRegion, report = report)

  # which report? this may be bypassed for some figures
  if (report == "MidAtlantic") {
    filterEPUs <- c("MAB")
    stop("Indicator for 'NewEngland' report only")
  } else {
    filterEPUs <- c("GB", "GOM")
  }

  # optional code to wrangle ecodata object prior to plotting
  # e.g., calculate mean, max or other needed values to join below
  fix <- ecodata::mass_inshore_survey |>
    dplyr::filter(EPU %in% filterEPUs, !grepl("Other", Var)) |>
    tidyr::separate(Var, into = c("Var", "Trash"), sep = " - ") |>
    dplyr::select(!Trash) |>
    tidyr::separate(Var, into = c("Var", "Val"), sep = " Biomass ") |>
    tidyr::pivot_wider(names_from = Val, values_from = Value) |>
    dplyr::mutate(Index = as.numeric(Index), SE = as.numeric(SE)) |>
    dplyr::group_by(Var) |>
    dplyr::mutate(
      hline = mean(Index),
      upper = Index + (2 * SE),
      lower = Index - (2 * SE)
    )

  fix$Var <- factor(
    fix$Var,
    levels = c(
      "Piscivore Spring",
      "Piscivore Fall",
      "Benthivore Spring",
      "Benthivore Fall",
      "Planktivore Spring",
      "Planktivore Fall",
      "Benthos Spring",
      "Benthos Fall"
    )
  )

  ## don't think these are needed anymore??
  ymax <- fix |>
    tidyr::separate(Var, into = c("vars", "Trash"), sep = " ") |>
    dplyr::group_by(vars) |>
    dplyr::summarise(max = max(Index, na.rm = T))

  usevars <- fix |>
    dplyr::distinct(Var) |>
    dplyr::pull()

  df2 <- data.frame(Time = 2015, Var = usevars) |>
    dplyr::mutate(vars = stringr::word(Var)) |>
    dplyr::left_join(ymax, by = "vars") |>
    dplyr::select(-vars)

  # Old code
  #fix <- ecodata::mass_inshore_survey |>
  #   dplyr::filter(EPU %in% filterEPUs,
  #                 grepl("Index",Var),
  #                 !grepl("Other",Var)) |>
  #   tidyr::separate(Var,into = c("Var","Trash"),sep =" Biomass") |>
  #   dplyr::select(-Trash) |>
  #   dplyr::mutate(Var = as.factor(Var))  |>
  #   dplyr::group_by(Var) |>
  #   dplyr::mutate(hline = mean(Value))

  #fix$Var <- factor(fix$Var,levels =  c("Piscivore Spring","Piscivore Fall",
  #                                       "Benthivore Spring","Benthivore Fall",
  #                                       "Planktivore Spring","Planktivore Fall",
  #                                       "Benthos Spring","Benthos Fall"))

  # ymax <- fix |>
  #   tidyr::separate(Var, into = c("vars","Trash"),sep=" ") |>
  #   dplyr::group_by(vars) |>
  #   dplyr::summarise(max = max(Value,na.rm=T))

  #usevars <- fix |>
  #  dplyr::distinct(Var) |>
  #  dplyr::pull()

  #df2 <- data.frame(Time = 2015,Var = usevars) |>
  #  dplyr::mutate(vars = stringr::word(Var)) |>
  #  dplyr::left_join(ymax,by="vars") |>
  #  dplyr::select(-vars)

  # code for generating plot object p
  # ensure that setup list objects are called as setup$...
  # e.g. fill = setup$shade.fill, alpha = setup$shade.alpha,
  # xmin = setup$x.shade.min , xmax = setup$x.shade.max
  #

  # Determine the order of your facets
  facet_order <- levels(fix$Var)

  # Create a list of scales in that EXACT order
  my_scales <- lapply(facet_order, function(v) {
    # Define limits based on the name
    lim <- switch(
      v,
      "Piscivore Spring" = c(0, 150),
      "Piscivore Fall" = c(0, 300),
      "Benthivore Spring" = c(0, 250),
      "Benthivore Fall" = c(0, 150),
      "Planktivore Spring" = c(0, 20),
      "Planktivore Fall" = c(0, 20),
      "Benthos Spring" = c(0, 20),
      "Benthos Fall" = c(0, 15),
      c(0, 100) # Default
    )
    return(ggplot2::scale_y_continuous(limits = lim, oob = scales::oob_keep))
  })

  p <- fix |>
    ggplot2::ggplot(ggplot2::aes(x = Time, y = Index)) +
    ggplot2::annotate(
      "rect",
      fill = setup$shade.fill,
      alpha = setup$shade.alpha,
      xmin = setup$x.shade.min,
      xmax = setup$x.shade.max,
      ymin = -Inf,
      ymax = Inf
    ) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    # ggplot2::geom_point(
    #   data = df2,
    #   ggplot2::aes(x = Time, y = max),
    #   alpha = 0
    # ) +
    ggplot2::geom_hline(
      ggplot2::aes(yintercept = hline, group = Var),
      linewidth = setup$hline.size,
      alpha = setup$hline.alpha,
      linetype = setup$hline.lty
    ) +
    ggplot2::geom_ribbon(
      # data = fix,
      ggplot2::aes(x = Time, ymin = pmax(lower, 0), ymax = upper),
      alpha = 0.5,
      fill = "gray"
    ) +
    ggplot2::ggtitle("Massachusetts inshore BTS") +
    ggplot2::ylab(expression("Biomass (kg tow"^-1 * ")")) +
    ggplot2::xlab(ggplot2::element_blank()) +
    ggplot2::facet_wrap(~Var, ncol = 2, scales = "free_y") +
    # this doesn't work
    ggh4x::facetted_pos_scales(
      y = my_scales
    ) +
    ggplot2::coord_cartesian(clip = "on") +
    ecodata::geom_gls() +
    ecodata::geom_lm(n = n) +
    ecodata::theme_ts() +
    ecodata::theme_facet() +
    ecodata::theme_title()

  # optional code for New England specific (2 panel) formatting
  # if (report == "NewEngland") {
  #   p <- p +
  #     ggplot2::theme(legend.position = "bottom",
  #                    legend.title = ggplot2::element_blank())
  #
  # }

  return(p)
}

plot_mass_inshore_survey(report = "NewEngland")

attr(plot_mass_inshore_survey, "report") <- c("MidAtlantic", "NewEngland")

######################
# testing with ggarrange

plot_custom_lims <- function(data, new_max) {
  p <- data |>
    ggplot2::ggplot(ggplot2::aes(x = Time, y = Index)) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::geom_hline(
      ggplot2::aes(yintercept = hline, group = Var)
    ) +
    ggplot2::geom_ribbon(
      # data = fix,
      ggplot2::aes(x = Time, ymin = lower, ymax = upper),
      alpha = 0.5,
      fill = "gray"
    ) +
    #  ggplot2::ggtitle("Massachusetts inshore BTS") +
    ggplot2::ylab(ggplot2::element_blank()) +
    ggplot2::xlab(ggplot2::element_blank()) +
    ggplot2::scale_y_continuous(
      limits = c(0, new_max),
      oob = scales::oob_keep
    ) +
    ggplot2::coord_cartesian(clip = "on") +
    ecodata::geom_gls() +
    ecodata::geom_lm(n = 10) +
    ecodata::theme_ts() +
    ecodata::theme_facet() +
    ecodata::theme_title()
}

p1 <- fix |>
  dplyr::filter(Var == "Piscivore Spring") |>
  plot_custom_lims(new_max = 150) +
  ggplot2::ggtitle("Piscivore Spring")

p2 <- fix |>
  dplyr::filter(Var == "Piscivore Fall") |>
  plot_custom_lims(new_max = 300) +
  ggplot2::ggtitle("Piscivore Fall")

p3 <- fix |>
  dplyr::filter(Var == "Benthivore Spring") |>
  plot_custom_lims(new_max = 250) +
  ggplot2::ggtitle("Benthivore Spring")

p4 <- fix |>
  dplyr::filter(Var == "Benthivore Fall") |>
  plot_custom_lims(new_max = 150) +
  ggplot2::ggtitle("Benthivore Fall")

p5 <- fix |>
  dplyr::filter(Var == "Planktivore Spring") |>
  plot_custom_lims(new_max = 20) +
  ggplot2::ggtitle("Planktivore Spring")

p6 <- fix |>
  dplyr::filter(Var == "Planktivore Fall") |>
  plot_custom_lims(new_max = 20) +
  ggplot2::ggtitle("Planktivore Fall")

p7 <- fix |>
  dplyr::filter(Var == "Benthos Spring") |>
  plot_custom_lims(new_max = 20) +
  ggplot2::ggtitle("Benthos Spring")

p8 <- fix |>
  dplyr::filter(Var == "Benthos Fall") |>
  plot_custom_lims(new_max = 15) +
  ggplot2::ggtitle("Benthos Fall")

p <- ggpubr::ggarrange(p1, p2, p3, p4, p5, p6, p7, p8, ncol = 2, nrow = 4) 

p <- ggpubr::annotate_figure(
  p,
  top = ggpubr::text_grob("Massachusetts inshore BTS", size = 12),
  left = ggpubr::text_grob("Biomass (kg tow ^-1)", rot = 90, size = 16)
)

  return(p)

