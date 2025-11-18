### INDICATORS WITH A '# FUNCITON ADJUSTED IN ECODATA' LINE HAVE HAD THE ADDITIONS MADE HERE ADDED TO THE ECODATA PLOT CODE
### THE LINES OF CODE ADDED TO ECODATA FUNCTIONS HAVE BEEN REMOVED FROM THE FUNCTIONS IN THIS CLEANED SCRIPT
### THE ORIGINAL MODS WITH COMMENTED OUT LINES OF CODE CAN BE FOUND IN 'scripts/create_plots.R'
### NOTE THAT THE CHANGES TO ECODATA PLOTTING CODE WILL NOT BE VISIBLE BY CALLING `ECODATA::` IN THIS SCRIPT UNTIL THE CHANGES HAVE BEEN MERGED IN ECODATA AND THE PACKAGE RELOADED

# setup ----

## variables ----

region <- "MidAtlantic" #or
region <- "NewEngland"

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

####### PLOTS THAT DIFFER FOR MAB/NE#######

# Performance relative to fishery management objectives ----

## Commercial & recreational landings ----

### Indicators ----

# total landings
save_plot(
  plot_expression = {
    ecodata::plot_comdat(
      report = region,
      varName = "landings",
      n = 10
    )
  },
  indicator = "total_landings",
  width = 6.5,
  height = ifelse(region == "NewEngland", 5, 4),
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
  width = 6.5,
  height = 7
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
  height = 2.5
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
  height = 2.5
)

# rec hms and sharks
save_plot(
  plot_expression = {
    rec_hms_plot <- ecodata::plot_rec_hms(report = region, 
                                          n = 100
                                          )
    rec_lps_sharks_plot <- ecodata::plot_lps_sharks(
      report = region,
      n = 100
    ) 
    ggpubr::ggarrange(
      rec_lps_sharks_plot,
      rec_hms_plot,
      ncol = 1,
      common.legend = TRUE,
      legend = "bottom"
    ) 
  },
  indicator = "rec_hms",
  width = 6.5,
  height = 8
)

### Implications ----

# 1. Stock Status Plot
# output of ecodata::plot_Stock_status is a list with plot in $p. 
# Need a better way to write this here other than the if/else, but function returns correct plot.
save_plot(
  plot_expression = {
    stock_status_plot <- plot_stock_status(report = region)
    if (region == "MidAtlantic") {
      stock_status_plot$p
      } else {
   stock_status_plot$p 
    }
  },
  indicator = "stock_status",
  width = 6.5,
  height = 4
)


# 2. ABC/ACL Stacked Plot NEW 
# uses the function plot_abc_acl in 'scripts' folder (abc_acl_rewrite.R), not current ecodata function
save_plot(
  plot_expression = {
    plot_abc_acl(
      report = region,
      plottype = "Stacked"
    )
  },
  indicator = "abcacl_stacked_new",
  width = 6.5,
  height = 7
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
  height = 2.5
)

# Aggregate biomass
# LEAVING FOR NOW, STILL NEED TO MAKE EXTENSIVE MODS
if (region == "MidAtlantic") {
  save_plot(
    plot_expression = {
      ecodata::plot_aggregate_biomass(report = region, EPU = "MAB", n = 10) 
    },
    indicator = "aggregate_biomass_mab",
    width = 6.5,
    height = 7
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
    height = 7
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
    height = 7
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
  },
  indicator = "comm_revenue",
  width = 6.5,
  height = ifelse(region == "NewEngland", 5, 2.5)
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
      ) 
      gom <- ecodata::plot_bennet(
        report = region,
        varName = "total",
        EPU = "GOM"
      ) 

        ggpubr::ggarrange(gb, gom, nrow = 2)
    }
  },
  indicator = "bennet",
  width = 6.5,
  height = ifelse(region == "NewEngland", 8, 4)
  ) 

# bennet all
save_plot(
  plot_expression = {
    if (region == "MidAtlantic") {
      ecodata::plot_bennet(report = region) 
    } else {
      gb <- ecodata::plot_bennet(
        report = "NewEngland",
        varName = "guild",
        EPU = "GB"
      ) 
      gom <- ecodata::plot_bennet(
        report = "NewEngland",
        varName = "guild",
        EPU = "GOM"
      ) 
      
      ggpubr::ggarrange(gb, gom, nrow = 2, common.legend = TRUE, legend = "bottom")  
    }
  },
  indicator = "bennet_all",
  width = 6.5,
  height = 5
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
  height = 2.5
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
  height = 2.5
)

# 2. Recreational Diversity Plot
save_plot(
  plot_expression = {
    ecodata::plot_recdat(
      report = region,
      varName = "effortdiversity",
      n = 10
    )
  },
  indicator = "rec_div",
  width = 6.5,
  height = 2.5
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
    ) 
    b <- ecodata::plot_commercial_div(
      report = region,
      varName = "Fleet diversity in revenue",
      n = 100
    ) 
    ggpubr::ggarrange(a, b, nrow = 2)
  },
  indicator = "comm_div_fleet",
  width = 6.5,
  height = 5
)

# 2. Commercial Diversity Species Diversity Plot
save_plot(
  plot_expression = {
    ecodata::plot_commercial_div(
      report = region,
      varName = "Permit revenue species diversity",
      n = 100
    )
  },
  indicator = "commercial_div_species_div",
  width = 6.5,
  height = 2.5
)

# 3. Recreational Diversity Catch Plot
save_plot(
  plot_expression = {
    ecodata::plot_recdat(report = region, varName = "catchdiversity", n = 10)
  },
  indicator = "recdat_div_catch",
  width = 6.5,
  height = 2.5
)

# total primary production
# KEPT YLAB HERE FOR GB/GOM
save_plot(
  plot_expression = {
    if (region == "MidAtlantic") {
      ecodata::plot_annual_chl_pp(
        report = region,
        varName = "pp",
        plottype = "total",
        EPU = "MAB"
      ) 
    } else {
      a <- ecodata::plot_annual_chl_pp(
        report = region,
        varName = "pp",
        plottype = "total",
        EPU = "GB",
        n = 27
      ) + ggplot2::ggtitle('Georges Bank total PP')
      b <- ecodata::plot_annual_chl_pp(
        report = region,
        varName = "pp",
        plottype = "total",
        EPU = "GOM",
        n = 27
      ) + ggplot2::ggtitle('Gulf of Maine total PP')
      
      ggpubr::ggarrange(a, b, nrow = 2)
    }
  },
  indicator = "totpp",
  width = 6.5,
  height = ifelse(region == "NewEngland", 5, 2.5)
)

# 4. Zooplankton Diversity Plot

save_plot(
  plot_expression = {
    zoo_diversity_plot <- ecodata::plot_zoo_diversity(report = region, n = 10)
  },
  indicator = "zoo_diversity",
  width = 6.5,
  height = ifelse(region == "NewEngland", 5, 2.5)
)

# 5. Expected N Plot
save_plot(
  plot_expression = {
    exp_n_plot <- ecodata::plot_exp_n(report = region, varName = "fall", n = 10)
  },
  indicator = "exp_n",
  width = 6.5,
  height = ifelse(region == "NewEngland", 5, 2.5)
)

# finfish traits
save_plot(
  plot_expression = {
    if (region == "MidAtlantic") {
      ecodata::plot_finfish_traits(report = region, 
                                   varName = "length_maturity") 
    } else {
      ecodata::plot_finfish_traits(
        report = region,
        varName = 'fecundity',
        n = 10
      ) 
    }
  },
  indicator = "traits",
  width = 6.5,
  height = ifelse(region == "NewEngland", 5, 2.5)
)

## Community social and climate vulnerability ----

# 1. Commercial Engagement Plot
save_plot(
  plot_expression = {
    commercial_engagement_plot <- ecodata::plot_engagement(
      report = region,
      varName = "Commercial"
    )
  },
  indicator = "commercial_engagement",
  width = 6.5,
  height = 4
)

# 2. Recreational Engagement Plot
save_plot(
  plot_expression = {
    recreational_engagement_plot <- plot_engagement(
      report = region,
      varName = "Recreational"
    )
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

### Risks to managing seasonally ----

# transition dates
save_plot(
  plot_expression = {
    ecodata::plot_trans_dates(report = region, 
                              varName = "length", n = 10) 
  },
  indicator = "transition_date",
  width = 6.5,
  height = ifelse(region == "NewEngland", 5, 2.5)
)


# Monthly Chlorophyll Plot
save_plot(
  plot_expression = {
ecodata::plot_chl_pp(
  report = region,
  plottype = "monthly", n = 10
) 
  },
indicator = "monthly_chl",
width = 6.5,
height = ifelse(region == "NewEngland", 7, 4)
)

### Risks to setting catch limits ----
# productivity + recruitment anomalies
save_plot(
  plot_expression = {
    productivity_anomaly_plot <- ecodata::plot_productivity_anomaly(
      report = region,
      EPU = dplyr::case_when(
        region == "NewEngland" ~ "GOM",
        region == "MidAtlantic" ~ "MAB"
      )
    ) 
    recruit_anomaly_plot <- ecodata::plot_productivity_anomaly(
      report = region,
      varName = "assessment"
    ) 
    # combined anomaly plot
    ggpubr::ggarrange(
      recruit_anomaly_plot,
      productivity_anomaly_plot,
      ncol = ifelse(region == "MidAtlantic", 1, 1)
    )
  },
  indicator = "productivity_anomaly",
  width = 6.5,
  height = 8
)

# condition factor
save_plot(
  plot_expression = {
    if (region == "MidAtlantic") {
      plot_condition(report = region) 
    } else {
      gb <- plot_condition(report = region, EPU = "GB") 

      gom <- plot_condition(report = region, EPU = "GOM") 

      ggpubr::ggarrange(gb, gom, ncol = 1, common.legend = TRUE, legend = "bottom") +
        patchwork::plot_layout(guides = 'collect') 
    }
  },
  indicator = "condition",
  width = 6.5,
  height = 6.5
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
    ecodata::plot_forage_index(report = region, 
                                      n = 10)
  },
  indicator = "foragebio",
  width = 6.5,
  height = ifelse(region == "NewEngland", 5, 2.5)
)

# 7. Benthos Plot
save_plot(
  plot_expression = {
    megabenthos_plot <- ecodata::plot_benthos_index(
      report = region,
      varName = "Megabenthos",
      n = 10
    ) 
    macrobenthos_plot <- ecodata::plot_benthos_index(
      report = region,
      varName = "Macrobenthos",
      n = 10
    ) 
    ggpubr::ggarrange(
      megabenthos_plot,
      macrobenthos_plot,
      common.legend = TRUE, legend = "bottom",
      nrow = 2
    ) 
  },
  indicator = "benthos",
  width = 6.5,
  height = 8
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
      nrow = 3,
      common.legend = TRUE,
      legend = "bottom"
    ) 
  },
  indicator = "zooplankton_anomaly",
  width = 6.5,
  height = 7.5
)

# 9. Thermal Habitat Persistence Plot
save_plot(
  plot_expression = {
    ecodata::plot_thermal_habitat_gridded(region) 
  },
  indicator = "therm_hab_persist",
  width = 6.5,
  height = 4
)

# Other ocean uses: offshore wind ----

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
### NEW PLOT FUNCTION
### THE FILE 'all_data' is in '//nefscdata/SOE_ESP_Data/ej_indicator/2026_SOE/output'
### THE NEW PLOTTING FUNCTION 'PLOT_WIND_PORT' IS IN PLOT-UPDATES BRANCH OF ECODATA.
      ### R/plot_wind_port.R
### NOT ON GITHUB AS IT CONTAINS CONFIDENTAL DATA

save_plot(
  plot_expression = {
      plot_wind_port(report=region,
                     data = all_data) 
  },
  indicator = "wea_port_rev",
  width = 6.5,
  height = 7
)

####### SAME PLOTS FOR BOTH REPORTS ######
# setup ----

region <- "BothReports"

out_dir <- here::here("images", region)
if (!dir.exists(out_dir)) {
  dir.create(out_dir)
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
      ecodata::plot_narw(varName = "adult", n = 10)
  },
  indicator = "narw_abundance",
  width = 6.5,
  height = 2.5
)

# narw calves
save_plot(
  plot_expression = {
    ecodata::plot_narw(varName = "calf", n = 10) 
  },
  indicator = "narw_calves",
  width = 6.5,
  height = 2.5
)

# seals
save_plot(
  plot_expression = {
    # for both reports, even though function calls NE
    ecodata::plot_seal_pups(report = "NewEngland")
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
    ggpubr::ggarrange(a, b, nrow = 2)
  },
  indicator = "species_dist",
  width = 6.5,
  height = 5
)

# whale and dolphin dist shifts
save_plot(
  plot_expression = {
    ecodata::plot_cetacean_dist() 
  },
  indicator = "cetacean_dist",
  width = 7.5,
  height = 4
)

# forage shifts
save_plot(
  plot_expression = {
    ecodata::plot_forage_index(varName = "cog", n = 10) 
  },
  indicator = "forage_dist",
  width = 6.5,
  height = 5
)

# macrobenthos shifts
save_plot(
  plot_expression = {
    ecodata::plot_benthos_index(
      plottype = "cog",
      varName = "Macrobenthos",
      n = 10
    ) 
  },
  indicator = "macrobenthos_dist",
  width = 6.5,
  height = 5
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
  height = 5
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
save_plot(
  plot_expression = {
    plot_wind_dev_speed() 
  },
  indicator = "wind_dev_speed",
  width = 6.5,
  height = 4
)

# slopewater
save_plot(
  plot_expression = {
    # for both reports, even though function calls NE
    ecodata::plot_slopewater(report = "NewEngland")
  },
  indicator = "slopewater",
  width = 6,
  height = 4
)

# for NE only ----

region <- "NewEngland"
out_dir <- here::here("images", region)

# calfin center of gravity -- NE only
save_plot(
  plot_expression = {
    ecodata::plot_zooplankton_index(
      report = region,
      varName = 'Calfin',
      plottype = 'cog',
      n = 10
    )
  },
  indicator = "calfin_cog",
  width = 6.5,
  height = 4
)

# mass inshore survey -- NE only
save_plot(
  plot_expression = {
    ecodata::plot_mass_inshore_survey(report = region, n = 10) 
  },
  indicator = "mass_inshore",
  width = 6,
  height = 6
)

# seabird productivity -- NE only
if (region == "NewEngland") {
  save_plot(
    plot_expression = {
      plot_seabird_ne(varName = "productivity", n = 10) 
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
      ecodata::plot_gom_salmon(n = 10)
    },
    indicator = "salmon",
    width = 6.5,
    height = 5
  )
}

