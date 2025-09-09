# setup ----

## TODO: identify if any of these plots are exactly the same in both reports
## if so, save in a "both_regions" folder (will have to modify the save function)

## variables ----

region <- "MidAtlantic"

out_dir <- here::here("images", region)
if (!dir.exists(out_dir)) {
  dir.create(out_dir)
}

region2 <- dplyr::case_when(
  region == "MidAtlantic" ~ "Mid-Atlantic",
  region == "NewEngland" ~ "New England"
)

full_region <- dplyr::case_when(
  region == "MidAtlantic" ~ "the Mid-Atlantic Bight",
  region == "NewEngland" ~ "New England"
)

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

# Performance relative to fishery management objectives ----

## Commercial & recreational landings ----

### Indicators ----

# total landings
save_plot(
  plot_expression = {
    plt <- ecodata::plot_comdat(
      report = region,
      varName = "landings",
      n = 10
    )
    if (region == "MidAtlantic") {
      plt
    } else {
      plt +
        ggplot2::ylab(expression("Landings (10"^3 * " metric tons)"))
    }
  },
  indicator = "total_landings",
  width = 6.5,
  height = 4
)

# commercial landings
save_plot(
  plot_expression = {
    ecodata::plot_comdat(
      report = region,
      plottype = "guild",
      n = 10
    )
  },
  indicator = "commercial_landings",
  width = 6,
  height = 6
)

# climate vulnerability landings
save_plot(
  plot_expression = {
    ecodata::plot_community_climate_vulnerability(
      report = region,
      plottype = "regionland",
      n = 100
    )
  },
  indicator = "climatevul_land",
  width = 6.5,
  height = 4
)

# rec landings
save_plot(
  plot_expression = {
    ecodata::plot_recdat(
      report = region,
      varName = "landings",
      n = 10
    )
  },
  indicator = "rec_landings",
  width = 6.5,
  height = 4
)

# rec hms and sharks
save_plot(
  plot_expression = {
    rec_hms_plot <- ecodata::plot_rec_hms(report = region, n = 100) +
      ggplot2::scale_color_discrete(
        limits = c("LargeCoastal", "Prohibited", "SmallCoastal"),
        labels = c("Large Coastal", "Prohibited", "Small Coastal")
      ) +
      ggplot2::ggtitle(paste(region2, "Recreational Shark Landings"))
    rec_lps_sharks_plot <- ecodata::plot_lps_sharks(
      report = region,
      n = 100
    ) +
      ggplot2::ggtitle(paste(region2, "Rec. Pelagic Shark Landings"))
    ggpubr::ggarrange(
      rec_hms_plot,
      rec_lps_sharks_plot,
      ncol = 2,
      common.legend = TRUE,
      legend = "bottom"
    )
  },
  indicator = "rec_hms",
  width = 6.5,
  height = 4
)

### Implications ----

# 1. Stock Status Plot
save_plot(
  plot_expression = {
    stock_status_plot <- ecodata::plot_stock_status(report = region)
    if (region == "MidAtlantic") {
      stock_status_plot$p +
        ggplot2::coord_cartesian(xlim = c(0, 2), ylim = c(0, 2))
    } else {
      stock_status_plot$p +
        ggplot2::theme(legend.position = 'bottom')
    }
  },
  indicator = "stock_status",
  width = 6.5,
  height = 4
)

# 2. ABC/ACL Stacked Plot
save_plot(
  plot_expression = {
    ecodata::plot_abc_acl(
      report = region,
      plottype = "Stacked"
    )
  },
  indicator = "abcacl_stacked",
  width = 6.5,
  height = 4
)

# 3. ABC/ACL Catch Plot
save_plot(
  plot_expression = {
    ecodata::plot_abc_acl(
      report = region,
      plottype = "Catch"
    )
  },
  indicator = "abcacl_catch",
  width = 6.5,
  height = 4
)

# Aggregate biomass
if (region == "MidAtlantic") {
  save_plot(
    plot_expression = {
      ecodata::plot_aggregate_biomass(report = region, EPU = "MAB", n = 10)
    },
    indicator = "aggregate_biomass_mab",
    width = 6.5,
    height = 6
  )
}

if (region == "NewEngland") {
  # georges bank
  save_plot(
    plot_expression = {
      ecodata::plot_aggregate_biomass(
        report = region,
        EPU = "GB",
        n = 10
      ) +
        ggplot2::theme(panel.spacing = grid::unit(0, 'lines'))
    },
    indicator = "aggregate_biomass_gb",
    width = 6.5,
    height = 6
  )
  # gulf of maine
  save_plot(
    plot_expression = {
      ecodata::plot_aggregate_biomass(
        report = region,
        EPU = "GOM",
        n = 10
      )
    },
    indicator = "aggregate_biomass_gom",
    width = 6.5,
    height = 6
  )
}

# mass inshore -- NE only
if (region == "NewEngland") {
  save_plot(
    plot_expression = {
      ecodata::plot_mass_inshore_survey(report = region, n = 10)
    },
    indicator = "mass_inshore",
    width = 6.5,
    height = 4
  )
}

## Commercial profits ----

### Indicators ----

# 1. Commercial Revenue Plot
save_plot(
  plot_expression = {
    comm_revenue_plot <- ecodata::plot_comdat(
      report = region,
      varName = "revenue",
      n = 10
    )
    if (region == "MidAtlantic") {
      comm_revenue_plot +
        ggplot2::theme(
          legend.position = "right",
          legend.title = ggplot2::element_blank()
        )
    } else {
      comm_revenue_plot
    }
  },
  indicator = "comm_revenue",
  width = 6.5,
  height = 4
)

# bennet

save_plot(
  plot_expression = {
    if (region == "MidAtlantic") {
      ecodata::plot_bennet(
        report = region,
        varName = "total"
      )
    } else {
      gb <- ecodata::plot_bennet(
        report = region,
        varName = "total",
        EPU = "GB"
      ) +
        ggplot2::ggtitle("GB revenue components") +
        ggplot2::theme(
          legend.position = "bottom",
          legend.title = ggplot2::element_blank()
        ) +
        ggplot2::ylab("Million USD (2023)")
      gom <- ecodata::plot_bennet(
        report = region,
        varName = "total",
        EPU = "GOM"
      ) +
        ggplot2::ggtitle("GOM revenue components") +
        ggplot2::theme(
          legend.position = "bottom",
          legend.title = ggplot2::element_blank()
        ) +
        ggplot2::ylab("Million USD (2023)")

      # this is using the patchwork library and might break
      # rewrite with ggarrange
      gb +
        gom +
        plot_layout(guides = 'collect') &
        theme(legend.position = 'bottom')
    }
  },
  indicator = "bennet",
  width = 6.5,
  height = 4
)

# bennet all
save_plot(
  plot_expression = {
    if (region == "MidAtlantic") {
      ecodata::plot_bennet(report = region) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle =45, hjust = 1))
    } else {
      gb <- ecodata::plot_bennet(
        report = "NewEngland",
        varName = "guild",
        EPU = "GB"
      ) +
        ggplot2::ylab("Million USD (2023)")
      gom <- ecodata::plot_bennet(
        report = "NewEngland",
        varName = "guild",
        EPU = "GOM"
      ) +
        ggplot2::ylab("Million USD (2023)")

      # this is using the patchwork library and might break
      # rewrite with ggarrange
      gb /
        gom +
        plot_layout(guides = 'collect') &
        theme(legend.position = 'bottom')
    }
  },
  indicator = "bennet_all",
  width = 6.5,
  height = 4
)

# 4. Climate Vulnerability Revenue Plot
save_plot(
  plot_expression = {
    ecodata::plot_community_climate_vulnerability(
      report = region,
      plottype = "regionrev",
      n = 100
    )
  },
  indicator = "climatevul_rev",
  width = 6.5,
  height = 4
)

## Recreational opportunities ----

### Indicators ----

# 1. Recreational Operational Plot
save_plot(
  plot_expression = {
    ecodata::plot_recdat(
      report = region,
      varName = "effort",
      n = 10
    )
  },
  indicator = "rec_op",
  width = 6.5,
  height = 4
)

# 2. Recreational Diversity Plot
save_plot(
  plot_expression = {
    plt <- ecodata::plot_recdat(
      report = region,
      varName = "effortdiversity",
      n = 10
    )
    if (region == "MidAtlantic") {
      plt
    } else {
      plt +
        ggplot2::ylab('Effective Shannon Index')
    }
  },
  indicator = "rec_div",
  width = 6.5,
  height = 4
)

## Stability ----

### Indicators ----

# 1. Commercial Diversity Fleet Plot
save_plot(
  plot_expression = {
    a <- ecodata::plot_commercial_div(
      report = region,
      varName = "Fleet count",
      n = 100
    ) +
      ggplot2::theme(plot.margin = ggplot2::unit(c(0.25, 0.5, 0.25, 0.5), "cm"))
    b <- ecodata::plot_commercial_div(
      report = region,
      varName = "Fleet diversity in revenue",
      n = 100
    ) +
      ggplot2::theme(plot.margin = ggplot2::unit(c(0.25, 0.5, 0.25, 0.5), "cm"))
    ggpubr::ggarrange(a, b, ncol = 2)
  },
  indicator = "comm_div_fleet",
  width = 6.5,
  height = 3
)

# 2. Commercial Diversity Species Diversity Plot
save_plot(
  plot_expression = {
    plt <- ecodata::plot_commercial_div(
      report = region,
      varName = "Permit revenue species diversity",
      n = 100
    )
    if (region == "MidAtlantic") {
      plt
    } else {
      plt + ggplot2::ylab('Effective Shannon Index')
    }
  },
  indicator = "commercial_div_species_div",
  width = 6.5,
  height = 4
)

# 3. Recreational Diversity Catch Plot
save_plot(
  plot_expression = {
    ecodata::plot_recdat(report = region, varName = "catchdiversity", n = 10)
  },
  indicator = "recdat_div_catch",
  width = 6.5,
  height = 4
)

# total primary production
save_plot(
  plot_expression = {
    if (region == "MidAtlantic") {
      ecodata::plot_annual_chl_pp(
        report = region,
        varName = "pp",
        plottype = "total",
        EPU = "MAB"
      ) +
        ggplot2::coord_cartesian(ylim = c(1e+07, 4e+07), xlim = c(1998, 2023)) +
        ggplot2::ggtitle("MAB Primary Production") +
        ggplot2::ylab("Carbon (mt)")
    } else {
      a <- ecodata::plot_annual_chl_pp(
        report = region,
        varName = "pp",
        plottype = "total",
        EPU = "GB",
        n = 27
      ) +
        ggplot2::ggtitle('Georges Bank total PP')
      b <- ecodata::plot_annual_chl_pp(
        report = region,
        varName = "pp",
        plottype = "total",
        EPU = "GOM",
        n = 27
      ) +
        ggplot2::ggtitle('Gulf of Maine total PP')
      # rewrite with ggarrange
      a + b
    }
  },
  indicator = "totpp",
  width = 6.5,
  height = 4
)

# 4. Zooplankton Diversity Plot
save_plot(
  plot_expression = {
    zoo_diversity_plot <- ecodata::plot_zoo_diversity(report = region, n = 10)
    if (region == "MidAtlantic") {
      zoo_diversity_plot +
        ggplot2::ggtitle("Zooplankton Diversity") +
        ggplot2::theme(plot.title = ggplot2::element_text(vjust = -5)) +
        ggplot2::ylab("Shannon Index")
    } else {
      zoo_diversity_plot
    }
  },
  indicator = "zoo_diversity",
  width = 6.5,
  height = 4
)

# 5. Expected N Plot
save_plot(
  plot_expression = {
    ecodata::plot_exp_n(report = region, varName = "fall", n = 10) +
      ggplot2::scale_x_continuous(breaks = seq(1968, 2018, by = 10), expand = c(0.01, 0.01))
  },
  indicator = "exp_n",
  width = 6.5,
  height = 4
)

# finfish traits
save_plot(
  plot_expression = {
    if (region == "MidAtlantic") {
      ecodata::plot_finfish_traits(report = region, varName = "length_maturity")
    } else {
      ecodata::plot_finfish_traits(
        report = region,
        varName = 'fecundity',
        n = 10
      ) +
        ggplot2::ylab('Fecundity (number of \noffspring per mature female)')
    }
  },
  indicator = "traits",
  width = 6.5,
  height = 4
)

## Community social and climate vulnerability ----

# 1. Commercial Engagement Plot
save_plot(
  plot_expression = {
    commercial_engagement_plot <- ecodata::plot_engagement(
      report = region,
      varName = "Commercial"
    )
    if (region == "MidAtlantic") {
      commercial_engagement_plot +
        ggplot2::theme(plot.title = ggplot2::element_text(vjust = 0))
    } else {
      commercial_engagement_plot
    }
  },
  indicator = "commercial_engagement",
  width = 6.5,
  height = 4
)

# 2. Recreational Engagement Plot
save_plot(
  plot_expression = {
    recreational_engagement_plot <- ecodata::plot_engagement(
      report = region,
      varName = "Recreational"
    )
    if (region == "MidAtlantic") {
      recreational_engagement_plot +
        ggplot2::theme(plot.title = ggplot2::element_text(vjust = 0))
    } else {
      recreational_engagement_plot
    }
  },
  indicator = "recreational_engagement",
  width = 6.5,
  height = 4
)

# 3. Community Climate Vulnerability Exposure Plot
save_plot(
  plot_expression = {
    ecodata::plot_community_climate_vulnerability(
      report = region,
      n = 100
    )
  },
  indicator = "commvulex",
  width = 6.5,
  height = 4
)

## Protected Species ----

### Indicators ----

# harbor porpoise
save_plot(
  plot_expression = {
    ecodata::plot_harborporpoise()
  },
  indicator = "harbor_porpoise",
  width = 6.5,
  height = 4
)

# gray seal
save_plot(
  plot_expression = {
    ecodata::plot_grayseal()
  },
  indicator = "gray_seal",
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

### Implications ----

# seals
save_plot(
  plot_expression = {
    ecodata::plot_seal_pups(report = region) +
      ggplot2::theme(legend.position = 'bottom')
  },
  indicator = "seal_pups",
  width = 6.5,
  height = 4
)

# Risks to meeting fishery management objectives ----

## Climte and ecosystem change ----

### Risks to managing spatially ----

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

### Risks to managing seasonally ----

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

# transition date
save_plot(
  plot_expression = {
    # TODO: move aesthetics into ecodata function
    ecodata::plot_trans_dates(report = region, varName = "length", n = 10) +
      ggplot2::ggtitle(paste(
        "Time between spring and fall transition in",
        full_region
      )) +
      ggplot2::theme(
        strip.background = ggplot2::element_blank(),
        strip.text.x = ggplot2::element_blank()
      )
  },
  indicator = "transition_date",
  width = 6.5,
  height = 4
)

# 1. Cold Pool Time Plot
save_plot(
  plot_expression = {
    ecodata::plot_cold_pool(varName = "persistence", n = 10)
  },
  indicator = "cold_pool_time",
  width = 6.5,
  height = 4
)

# 2. Monthly Chlorophyll Plot
save_plot(
  plot_expression = {
    ecodata::plot_chl_pp(
      report = region,
      plottype = "monthly",
      n = 100
    ) +
      ggplot2::facet_wrap(EPU~Month~., ncol = 3)
                          #, scales = "free_y")
  },
  indicator = "monthly_chl",
  width = 6.5,
  height = 8
)

### Risks to setting catch limits ----
# productivity + recruitment anomalies
save_plot(
  plot_expression = {
    # TODO: move aesthetics into ecodata function
    productivity_anomaly_plot <- ecodata::plot_productivity_anomaly(
      report = region,
      EPU = dplyr::case_when(
        region == "NewEngland" ~ "GOM",
        region == "MidAtlantic" ~ "MAB"
      )
    ) +
      ggplot2::guides(
        fill = ggplot2::guide_legend(
          ncol = dplyr::case_when(
            region == "NewEngland" ~ 3,
            region == "MidAtlantic" ~ 2,
            TRUE ~ 2
          )
        )
      ) +
      ggplot2::theme(
        legend.position = "bottom",
        legend.title = ggplot2::element_blank(),
        legend.text = ggplot2::element_text(size = 8),
        plot.title = ggplot2::element_text(size = 11)
      )
    # TODO: move aesthetics into ecodata function
    recruit_anomaly_plot <- ecodata::plot_productivity_anomaly(
      report = region,
      varName = "assessment"
    ) +
      ggplot2::guides(fill = ggplot2::guide_legend(ncol = 2)) +
      ggplot2::theme(
        legend.position = "bottom",
        legend.title = ggplot2::element_blank(),
        legend.text = ggplot2::element_text(size = 10),
        plot.title = ggplot2::element_text(size = 11)
      )
    # combined anomaly plot
    ggpubr::ggarrange(
      productivity_anomaly_plot,
      recruit_anomaly_plot,
      ncol = ifelse(region == "MidAtlantic", 2, 1)
    )
  },
  indicator = "productivity_anomaly",
  width = 6.5,
  height = 4
)

# seabird productivity -- NE only
if (region == "NewEngland") {
  save_plot(
    plot_expression = {
      ecodata::plot_seabird_ne(varName = "productivity", n = 10) +
        ## TODO: update these xlim
        ggplot2::coord_cartesian(xlim = c(1992, 2023))
    },
    indicator = "seabird_productivity",
    width = 6.5,
    height = 4
  )
}

# salmon -- NE only
if (region == "NewEngland") {
  save_plot(
    plot_expression = {
      ecodata::plot_gom_salmon(n = 10) +
        ggplot2::ylab('returning proportion')
    },
    indicator = "salmon",
    width = 6.5,
    height = 4
  )
}

# condition factor
save_plot(
  plot_expression = {
    if (region == "MidAtlantic") {
      ecodata::plot_condition(report = region) +
        ggplot2::theme(
          legend.text = ggplot2::element_text(size = 10),
          legend.title = ggplot2::element_text(size = 11),
          axis.text.x = ggplot2::element_text(size = 12),
          axis.text.y = ggplot2::element_text(size = 8),
          plot.title = ggplot2::element_text(size = 12)
        )
    } else {
      gb <- ecodata::plot_condition(report = region, EPU = "GB")

      gom <- ecodata::plot_condition(report = region, EPU = "GOM")

      # change to ggarrange
      gb /
        gom +
        plot_layout(guides = 'collect') &
        theme(
          legend.position = 'bottom',
          legend.text = element_text(size = 10),
          legend.title = element_text(size = 11),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          plot.title = element_text(size = 12)
        )
    }
  },
  indicator = "condition",
  width = 6.5,
  height = 4
)

# 5. Energy Density Plot
save_plot(
  plot_expression = {
    ecodata::plot_energy_density(report = region)
  },
  indicator = "energy_density",
  width = 6.5,
  height = 4
)

# 6. Forage Index Plot
save_plot(
  plot_expression = {
    ecodata::plot_forage_index(report = region, n = 10)
  },
  indicator = "foragebio",
  width = 6.5,
  height = 4
)

# 7. Benthos Plot
save_plot(
  plot_expression = {
    megabenthos_plot <- ecodata::plot_benthos_index(
      report = region,
      varName = "Megabenthos",
      n = 10
    ) +
      ggplot2::theme(legend.position = "none")
    macrobenthos_plot <- ecodata::plot_benthos_index(
      report = region,
      varName = "Macrobenthos",
      n = 10
    ) +
      ggplot2::theme(legend.position = "right")
    ggpubr::ggarrange(
      megabenthos_plot,
      macrobenthos_plot,
      ncol = ifelse(region == "MidAtlantic", 2, 1)
    )
  },
  indicator = "benthos",
  width = 6.5,
  height = 4
)

# 8. Zooplankton Anomaly Plot
save_plot(
  plot_expression = {
    large_copepod_plot <- ecodata::plot_zooplankton_index(
      report = region,
      varName = "Lgcopeall",
      n = 10
    )
    small_copepod_plot <- ecodata::plot_zooplankton_index(
      report = region,
      varName = "Smallcopeall",
      n = 10
    )
    euphausiid_plot <- ecodata::plot_zooplankton_index(
      report = region,
      varName = "Euph",
      n = 10
    )
    ggpubr::ggarrange(
      large_copepod_plot,
      small_copepod_plot,
      euphausiid_plot,
      ncol = ifelse(region == "MidAtlantic", 3, 1),
      common.legend = TRUE,
      legend = "bottom"
    )
  },
  indicator = "zooplankton_anomaly",
  width = 6.5,
  height = 4
)

# 9. Thermal Habitat Persistence Plot
save_plot(
  plot_expression = {
    plt <- ecodata::plot_thermal_habitat_gridded(region)
    if (region == "MidAtlantic") {
      plt
    } else {
      plt + ggplot2::theme(plot.margin = grid::unit(c(0, 0, 0, 0), "cm"))
    }
  },
  indicator = "therm_hab_persist",
  width = 6.5,
  height = 4
)

# Other ocean uses: offshore wind ----

# development speed
save_plot(
  plot_expression = {
    ecodata::plot_wind_dev_speed()
  },
  indicator = "wind_dev_speed",
  width = 6.5,
  height = 4
)

# 1. Wind Species Revenue Plot
save_plot(
  plot_expression = {
    ecodata::plot_wind_revenue(
      report = region,
      varName = "value",
      plottype = "nofacets"
    )
  },
  indicator = "wind_revenue",
  width = 6.5,
  height = 4
)

# 3. Wind Port Revenue Plot
save_plot(
  plot_expression = {
    ecodata::plot_wind_port(report = region)
  },
  indicator = "wea_port_rev",
  width = 6.5,
  height = 4
)

# Highlights ----

save_plot(
  plot_expression = {
    # for both reports, even though function calls NE
    ecodata::plot_slopewater(report = "NewEngland")
  },
  indicator = "slopewater",
  width = 6.5,
  height = 4
)
