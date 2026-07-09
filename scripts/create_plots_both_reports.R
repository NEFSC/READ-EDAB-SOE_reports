##################################
#' Run BothReports plots
#'
#' This function creates all plots that are the same for both the MAB and NE reports
#'
#' @param region Region for which to create report plots ("BothReports")
#'
#'

# region <- "BothReports"

source(here::here("../ecodata/R/summary_functions.R"))
source(here::here("utils/image_functions.R"))

create_plots_both <- function(region = "BothReports") {
  # setup ----

  out_dir <- here::here("images", region)
  if (!dir.exists(out_dir)) {
    dir.create(out_dir)
  }

  # 9. Thermal Habitat Persistence Plot
  save_plot(
    plot_expression = {
      plt <- ecodata::plot_thermal_habitat_gridded(region)
    },
    indicator = "therm_hab_persist",
    width = 6.5,
    height = 4,
    output_summary = FALSE
  )

  # 5. Energy Density Plot
  # THIS IS A STATIC PLOT IN 2026
  # save_plot(
  #   plot_expression = {
  #     # plot is the same even though it takes a region parameter
  #     ecodata::plot_energy_density(report = "NewEngland")
  #   },
  #   indicator = "energy_density",
  #   width = 6.5,
  #   height = 4
  # )

  # GOM ocean acidification
  GOMoa_image <- "https://github.com/NOAA-EDAB/ecodata/raw/dev/data-raw/workshop/images/Hunt_WBD_2024_pCO2_OMa_Weekly_Climatology-ChrisH_2025.pdf"

  img <- magick::image_read_pdf(GOMoa_image) |>
    magick::image_crop("1650x1100+400+1650")
  magick::image_write(
    img,
    path = here::here("images/BothReports/GOMoa_image.png"),
    format = "png"
  )

  # harbor porpoise
  save_plot(
    plot_expression = {
      ecodata::plot_harborporpoise()
    },
    indicator = "harborporpoise",
    width = 6.5,
    height = 3,
    output_summary = FALSE
  )

  # gray seal
  save_plot(
    plot_expression = {
      ecodata::plot_grayseal()
    },
    indicator = "grayseal",
    width = 6.5,
    height = 3,
    output_summary = FALSE
  )

  # narw-abundance
  save_plot(
    plot_expression = {
      ecodata::plot_narw(varName = "adult", n = 10) +
        ggplot2::ggtitle("North Atlantic right whale abundance") +
        ggplot2::scale_x_continuous(limits = c(1980, 2025))
    },
    indicator = "narw_abundance",
    width = 6.5,
    height = 2.5
  )

  # narw calves
  save_plot(
    plot_expression = {
      ecodata::plot_narw(varName = "calf", n = 10) +
        ggplot2::ggtitle("North Atlantic right whale calf abundance")
    },
    indicator = "NARW-calf-abundance",
    width = 6.5,
    height = 2.5
  )

  # seals
  save_plot(
    plot_expression = {
      # for both reports, even though function calls NE
      ecodata::plot_seal_pups(report = "NewEngland") +
        ggplot2::theme(legend.position = 'bottom')
    },
    indicator = "seal_pups",
    width = 6.5,
    height = 4
  )

  # species dist
  save_plot(
    plot_expression = {
      a <- ecodata::plot_species_dist(varName = "along", n = 10)
      b <- ecodata::plot_species_dist(varName = "depth", n = 10)
      ggpubr::ggarrange(a, b, ncol = 1)
    },
    indicator = "species_dist",
    width = 6.5,
    height = 3.5
  )

  # whale and dolphin dist shifts
  save_plot(
    plot_expression = {
      ecodata::plot_cetacean_dist() +
        ggplot2::ggtitle("Whale and Dolphin Distribution Shifts") +
        ggplot2::facet_wrap(~season, nrow = 1) +
        ggplot2::theme(legend.position = "bottom")
    },
    indicator = "cetacean_dist",
    width = 7.5,
    height = 4,
    output_summary = FALSE
  )

  # forage shifts
  save_plot(
    plot_expression = {
      ecodata::plot_forage_index(varName = "cog", n = 10) +
        ggplot2::coord_cartesian(xlim = c(1982, NA))
    },
    indicator = "forage_dist",
    width = 6.5,
    height = 3.5
  )

  # longterm sst
  save_plot(
    plot_expression = {
      ecodata::plot_long_term_sst(n = 10)
    },
    indicator = "long_term_sst",
    width = 6.5,
    height = 2.5
  )

  # gsi
  save_plot(
    plot_expression = {
      ecodata::plot_gsi(varName = "westgsi", n = 10)
    },
    indicator = "west_gsi",
    width = 6.5,
    height = 2.5
  )

  # cold pool size
  save_plot(
    plot_expression = {
      a <- ecodata::plot_cold_pool(varName = "cold_pool", n = 10)
      b <- ecodata::plot_cold_pool(varName = "extent", n = 10)
      ggpubr::ggarrange(a, b, nrow = 2)
    },
    indicator = "cold_pool",
    width = 6.5,
    height = 4
  )

  # cold pool timing
  save_plot(
    plot_expression = {
      ecodata::plot_cold_pool(varName = "persistence", n = 10)
    },
    indicator = "cold_pool_time",
    width = 6.5,
    height = 2.5
  )

  # spawn timing
  save_plot(
    plot_expression = {
      ecodata::plot_spawn_timing(n = 10)
    },
    indicator = "spawn_timing",
    width = 6.5,
    height = 4
  )

  # development speed
  # not used in 2026
  # save_plot(
  #   plot_expression = {
  #     ecodata::plot_wind_dev_speed() +
  #       ggplot2::theme(legend.position = 'bottom')
  #   },
  #   indicator = "wind_dev_speed",
  #   width = 6.5,
  #   height = 4
  # )

  # slopewater
  save_plot(
    plot_expression = {
      # for both reports, even though function calls NE
      ecodata::plot_slopewater(report = "NewEngland", n = 10)
    },
    indicator = "slopewater",
    width = 6,
    height = 2.5
  )

  # small cope center of gravity
  save_plot(
    plot_expression = {
      ecodata::plot_zooplankton_index(
        varName = 'Smallcopeall',
        plottype = 'cog',
        n = 10
      ) +
        ggplot2::ggtitle("Northeast U.S. Small Copepod Distribution") +
        ggplot2::ylab("Center of Gravity, km")
    },
    indicator = "smallcopeall_cog",
    width = 6.5,
    height = 3.5
  )

  # large cope center of gravity
  save_plot(
    plot_expression = {
      ecodata::plot_zooplankton_index(
        varName = 'Lgcopeall',
        plottype = 'cog',
        n = 10
      ) +
        ggplot2::ggtitle("Northeast U.S. Large Copepod Distribution") +
        ggplot2::ylab("Center of Gravity, km")
    },
    indicator = "lgcopeall_cog",
    width = 6.5,
    height = 3.5
  )

  # macrobenthos shifts
  save_plot(
    plot_expression = {
      ecodata::plot_benthos_index(
        plottype = "cog",
        varName = "Macrobenthos",
        n = 10
      ) +
        ggplot2::ggtitle("Northeast U.S. Macrobenthos Distribution") +
        ggplot2::ylab("Center of Gravity, km") +
        ggplot2::theme(legend.position = 'bottom') +
        ggplot2::facet_grid(
          cols = ggplot2::vars(Season),
          rows = ggplot2::vars(Direction),
          scales = "free_y"
        )
    },
    indicator = "macrobenthos_dist",
    width = 6.5,
    height = 3.5
  )

  # megabenthos shifts
  save_plot(
    plot_expression = {
      ecodata::plot_benthos_index(
        plottype = "cog",
        varName = "Megabenthos",
        n = 10
      ) +
        ggplot2::ggtitle("Northeast U.S. Megabenthos Distribution") +
        ggplot2::ylab("Center of Gravity, km") +
        ggplot2::facet_grid(
          cols = ggplot2::vars(Season),
          rows = ggplot2::vars(Direction),
          scales = "free_y"
        ) +
        ggplot2::theme(legend.position = 'bottom')
    },
    indicator = "megabenthos_dist",
    width = 6.5,
    height = 3.5
  )

  # euphausiid center of gravity
  save_plot(
    plot_expression = {
      ecodata::plot_zooplankton_index(
        report = "MidAtlantic",
        varName = 'Euph',
        plottype = 'cog',
        n = 10
      ) +
        ggplot2::ggtitle("Northeast U.S. Euphausiid Distribution") +
        ggplot2::ylab("Center of Gravity, km") +
        ggplot2::theme(legend.position = 'bottom') +
        ggplot2::facet_grid(
          cols = ggplot2::vars(Season),
          rows = ggplot2::vars(Direction),
          scales = "free_y"
        )
    },
    indicator = "euph_cog",
    width = 6.5,
    height = 3.5
  )
}

# create_plots_both(region = "BothReports")
