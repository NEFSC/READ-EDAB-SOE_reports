# setup ----

region <- "BothReports"
out_dir <- here::here("images", region)
if (!dir.exists(out_dir)) {
  dir.create(out_dir)
}

## functions ----

# A function to create a standardized filename
create_filename <- function(
  indicator,
  reg = region,
  dir = out_dir,
  extension = ".png"
) {
  file.path(
    dir,
    paste0(
      indicator,
      "_",
      reg,
      "_",
      Sys.Date(),
      extension
    )
  )
}

# A flexible function to generate and save a plot
save_plot <- function(plot_expression, indicator, ...) {
  # Execute the code to create the plot
  p <- eval(plot_expression)

  # Check if the plot object is valid before saving
  if (inherits(p, "ggplot") || inherits(p, "ggarrange")) {
    fname <- create_filename(indicator)
    ggplot2::ggsave(
      filename = fname,
      plot = p,
      ...
    )
    message("Plot saved to: ", fname)
  } else {
    stop("Plot object is not a valid ggplot or ggarrange object.")
  }
}

# GOM ocean acidification

GOMoa_image <- "https://github.com/NOAA-EDAB/ecodata/raw/dev/data-raw/workshop/images/Hunt_WBD_2024_pCO2_OMa_Weekly_Climatology-ChrisH_2025.pdf"

img <- magick::image_read_pdf(GOMoa_image) |>
  magick::image_crop("1650x1100+400+1650")
magick::image_write(
  img,
  path = here::here("images/BothReports/GOMoa_image.png"),
  format = "png"
)

# zooplankton season
# magick::image_read("https://github.com/NOAA-EDAB/ecodata/blob/dev/data-raw/workshop/images/RUNGE_Fig1_SeasonalandmultiannualabundanceofCalanusfinmarchicus-JeffreyRunge_2025.png?raw=true")|>
#   magick::image_crop("2000x2000+0+1900")

# harbor porpoise
save_plot(
  plot_expression = {
    ecodata::plot_harborporpoise()
  },
  indicator = "harborporpoise",
  width = 6.5,
  height = 4
)

# gray seal
save_plot(
  plot_expression = {
    ecodata::plot_grayseal()
  },
  indicator = "grayseal",
  width = 6.5,
  height = 4
)

# narw-abundance
save_plot(
  plot_expression = {
    ecodata::plot_narw(varName = "adult", n = 10) +
      ggplot2::ggtitle("North Atlantic right whale abundance")
  },
  indicator = "narw_abundance",
  width = 6.5,
  height = 4
)

# narw calves
save_plot(
  plot_expression = {
    ecodata::plot_narw(varName = "calf", n = 10) +
      ggplot2::ggtitle("North Atlantic right whale calf abundance")
  },
  indicator = "narw_calves",
  width = 6.5,
  height = 4
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
## TODO: update xlims?
save_plot(
  plot_expression = {
    a <- ecodata::plot_species_dist(varName = "along", n = 10) +
      ggplot2::coord_cartesian(xlim = c(1969, 2021))
    b <- ecodata::plot_species_dist(varName = "depth", n = 10) +
      ggplot2::coord_cartesian(xlim = c(1969, 2021))
    ggpubr::ggarrange(a, b, ncol = 2)
  },
  indicator = "species_dist",
  width = 6.5,
  height = 4
)

# protectedspp-dist-shifts
save_plot(
  plot_expression = {
    ecodata::plot_cetacean_dist() +
      ggplot2::ggtitle("Whale and Dolphin Distribution Shifts")
  },
  indicator = "cetacean_dist",
  width = 6.5,
  height = 4
)

# forage shifts
save_plot(
  plot_expression = {
    ecodata::plot_forage_index(varName = "cog", n = 10) +
      ggplot2::coord_cartesian(xlim = c(1982, 2023)) +
      ggplot2::ggtitle("Northeast U.S. Forage Fish Distribution") +
      ggplot2::ylab("Center of Gravity, km")
  },
  indicator = "forage_dist",
  width = 6.5,
  height = 4
)

# macrobenthos shifts
save_plot(
  plot_expression = {
    ecodata::plot_benthos_index(
      plottype = "cog",
      varName = "Macrobenthos",
      n = 10
    ) +
      ggplot2::coord_cartesian(xlim = c(1980, 2023)) +
      ggplot2::ggtitle("Northeast U.S. Macrobenthos Distribution") +
      ggplot2::ylab("Center of Gravity, km")
  },
  indicator = "macrobenthos_dist",
  width = 6.5,
  height = 4
)

# longterm sst
save_plot(
  plot_expression = {
    ecodata::plot_long_term_sst(n = 10)
  },
  indicator = "long_term_sst",
  width = 6.5,
  height = 4
)

# gsi
save_plot(
  plot_expression = {
    ecodata::plot_gsi(varName = "westgsi", n = 10)
  },
  indicator = "west_gsi",
  width = 6.5,
  height = 4
)

# cold pool size
save_plot(
  plot_expression = {
    a <- ecodata::plot_cold_pool(varName = "cold_pool", n = 10)
    b <- ecodata::plot_cold_pool(varName = "extent", n = 10)
    ggpubr::ggarrange(a, b, ncol = 2)
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
  height = 4
)

# spawn timing
save_plot(
  plot_expression = {
    ecodata::plot_spawn_timing(n = 10) +
      ggplot2::ggtitle("Spring Resting Maturity Stage")
  },
  indicator = "spawn_timing",
  width = 6.5,
  height = 4
)


# development speed
save_plot(
  plot_expression = {
    ecodata::plot_wind_dev_speed()
  },
  indicator = "wind_dev_speed",
  width = 6.5,
  height = 4
)

save_plot(
  plot_expression = {
    # for both reports, even though function calls NE
    ecodata::plot_slopewater(report = "NewEngland")
  },
  indicator = "slopewater",
  width = 6.5,
  height = 4
)

# for NE only ----

# calfin center of gravity
region <- "NewEngland"
out_dir <- here::here("images", region)

save_plot(
  plot_expression = {
    ecodata::plot_zooplankton_index(
      report = region,
      varName = 'Calfin',
      plottype = 'cog',
      n = 10
    ) +
      theme(legend.position = 'bottom')
  },
  indicator = "calfin_cog",
  width = 6.5,
  height = 4
)

# mass inshore survey

save_plot(
  plot_expression = {
    ecodata::plot_mass_inshore_survey(report = region, n = 10)
  },
  indicator = "mass_inshore",
  width = 6.5,
  height = 4
)

# zooplankton season

# for both reports (region-specific figures) ----
# wind rev
save_plot(
  plot_expression = {
    ecodata::plot_wind_revenue(
      report = region,
      varName = "value",
      plottype = "nofacets"
    )
  },
  indicator = "wea_spp_rev",
  width = 6.5,
  height = 4
)
