# setup ----

region <- "MidAtlantic"

out_dir <- here::here("images")
if (!dir.exists(out_dir)) {
  dir.create(out_dir)
}

## TODO: name so that figures that are the same in both reports save in a separate directory
create_filename <- function(indicator, reg = region, dir = out_dir) {
  new_out_dir <- paste(out_dir, reg, sep = "/")
  if (!dir.exists(new_out_dir)) {
    dir.create(new_out_dir)
  }

  fname <- paste0(
    new_out_dir,
    "/",
    indicator,
    "_",
    reg,
    "_",
    Sys.Date(),
    ".png"
  )
  message(fname)
  return(fname)
}

region2 <- dplyr::case_when(
  region == "MidAtlantic" ~ "Mid-Atlantic",
  region == "NewEngland" ~ "New England"
)

full_region <- dplyr::case_when(
  region == "MidAtlantic" ~ "the Mid-Atlantic Bight",
  region == "NewEngland" ~ "New England"
)

# Performance relative to fishery management objectives ----

## Commercial & recreational landings ----

### Indicators ----

# total landings
ecodata::plot_comdat(
  report = region,
  varName = "landings",
  n = 10
)
ggplot2::ggsave(
  create_filename("total_landings"),
  width = 6.5,
  height = 4
)

# commercial landings
ecodata::plot_comdat(report = region, plottype = "guild", n = 10)
ggplot2::ggsave(
  create_filename("commercial_landings"),
  width = 6.5,
  height = 4
)

# climate vulnerability landings
ecodata::plot_community_climate_vulnerability(
  report = region,
  plottype = "regionland",
  n = 100
)
ggplot2::ggsave(
  create_filename("climatevul_land"),
  width = 6.5,
  height = 4
)

# rec landings
ecodata::plot_recdat(report = region, varName = "landings", n = 10)
ggplot2::ggsave(
  create_filename("rec_landings"),
  width = 6.5,
  height = 4
)

# rec hms and sharks
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
ggplot2::ggsave(
  create_filename("rec_hms"),
  width = 6.5,
  height = 4
)

### Implications ----

# 1. Stock Status Plot
stock_status_plot <- ecodata::plot_stock_status(report = region)

if (region == "MidAtlantic") {
  stock_status_plot$p +
    ggplot2::coord_cartesian(xlim = c(0, 2), ylim = c(0, 2))
} else {
  # new england code here
}

ggplot2::ggsave(
  create_filename("stock_status"),
  width = 6.5,
  height = 4
)

# 2. ABC/ACL Stacked Plot
ecodata::plot_abc_acl(report = region, plottype = "Stacked")
ggplot2::ggsave(
  create_filename("abcacl_stacked"),
  width = 6.5,
  height = 4
)

# 3. ABC/ACL Catch Plot
ecodata::plot_abc_acl(report = region, plottype = "Catch")
ggplot2::ggsave(
  create_filename("abcacl_catch"),
  width = 6.5,
  height = 4
)

# aggregate biomass
if (region == "MidAtlantic") {
  ecodata::plot_aggregate_biomass(report = region, EPU = "MAB", n = 10)
} else {
  # new england code here
}

ggplot2::ggsave(
  create_filename("aggregate_biomass"),
  width = 6.5,
  height = 4
)

## Commercial profits ----

### Indicators ----

# 1. Commercial Revenue Plot
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
  # new england code here
}

ggplot2::ggsave(
  create_filename("comm_revenue"),
  width = 6.5,
  height = 4
)

# bennet
ecodata::plot_bennet(report = region, varName = "total")
ggplot2::ggsave(
  create_filename("bennet"),
  width = 6.5,
  height = 4
)

# bennet all
ecodata::plot_bennet(report = region)
ggplot2::ggsave(
  create_filename("bennet_all"),
  width = 6.5,
  height = 4
)

# 4. Climate Vulnerability Revenue Plot
climatevul_rev_plot <- ecodata::plot_community_climate_vulnerability(
  report = region,
  plottype = "regionrev",
  n = 100
)
ggplot2::ggsave(
  create_filename("climatevul_rev"),
  width = 6.5,
  height = 4
)

## Recreational opportunities ----

### Indicators ----

# 1. Recreational Operational Plot
ecodata::plot_recdat(report = region, varName = "effort", n = 10)
ggplot2::ggsave(
  create_filename("rec_op"),
  width = 6.5,
  height = 4
)

# 2. Recreational Diversity Plot
ecodata::plot_recdat(report = region, varName = "effortdiversity", n = 10)
ggplot2::ggsave(
  create_filename("rec_div"),
  width = 6.5,
  height = 4
)

## Stability ----

### Indicators ----

# 1. Commercial Diversity Fleet Plot
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
ggpubr::ggarrange(a, b, ncol = 2)
ggplot2::ggsave(
  create_filename("comm_div_fleet"),
  width = 6.5,
  height = 4
)

# 2. Commercial Diversity Species Diversity Plot
ecodata::plot_commercial_div(
  report = region,
  varName = "Permit revenue species diversity",
  n = 100
)
ggplot2::ggsave(
  create_filename("commercial_div_species_div"),
  width = 6.5,
  height = 4
)

# 3. Recreational Diversity Catch Plot
ecodata::plot_recdat(report = region, varName = "catchdiversity", n = 10)
ggplot2::ggsave(
  create_filename("recdat_div_catch"),
  width = 6.5,
  height = 4
)

# total primary production
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
  # new england code here
}

ggplot2::ggsave(
  create_filename("totpp"),
  width = 6.5,
  height = 4
)

# 4. Zooplankton Diversity Plot
zoo_diversity_plot <- ecodata::plot_zoo_diversity(
  report = region,
  n = 10
)

if (region == "MidAtlantic") {
  zoo_diversity_plot +
    ggplot2::ggtitle("Zooplankton Diversity") +
    ggplot2::theme(plot.title = ggplot2::element_text(vjust = -5)) +
    ggplot2::ylab("Shannon Index")
} else {
  # new england code here
}
ggplot2::ggsave(
  create_filename("zoo_diversity"),
  width = 6.5,
  height = 4
)

# 5. Expeditions N Plot
exp_n_plot <- ecodata::plot_exp_n(
  report = region,
  varName = "fall",
  n = 10
)

ggplot2::ggsave(
  create_filename("exp_n"),
  width = 6.5,
  height = 4
)

# finfish traits
if (region == "MidAtlantic") {
  ecodata::plot_finfish_traits(
    report = region,
    varName = "length_maturity"
  )
} else {
  # new england code here
}
ggplot2::ggsave(
  create_filename("traits"),
  width = 6.5,
  height = 4
)

## Community social and climate vulnerability ----

# 1. Commercial Engagement Plot
commercial_engagement_plot <- ecodata::plot_engagement(
  report = region,
  varName = "Commercial"
)
if (region == "MidAtlantic") {
  commercial_engagement_plot +
    ggplot2::theme(plot.title = ggplot2::element_text(vjust = 0))
} else {
  # new england code here
}
ggplot2::ggsave(
  create_filename("commercial_engagement"),
  width = 6.5,
  height = 4
)

# 2. Recreational Engagement Plot
recreational_engagement_plot <- ecodata::plot_engagement(
  report = region,
  varName = "Recreational"
)
if (region == "MidAtlantic") {
  recreational_engagement_plot +
    ggplot2::theme(plot.title = ggplot2::element_text(vjust = 0))
} else {
  # new england code here
}
ggplot2::ggsave(
  create_filename("recreational_engagement"),
  width = 6.5,
  height = 4
)

# 3. Community Climate Vulnerability Exposure Plot
ecodata::plot_community_climate_vulnerability(report = region, n = 100)
ggplot2::ggsave(
  create_filename("commvulex"),
  width = 6.5,
  height = 4
)

## Protected Species ----

### Indicators ----

# harbor porpoise
ecodata::plot_harborporpoise()
ggplot2::ggsave(
  create_filename("harbor_porpoise"),
  width = 6.5,
  height = 4
)

# gray seal
ecodata::plot_grayseal()
ggplot2::ggsave(
  create_filename("gray_seal"),
  width = 6.5,
  height = 4
)

# narw-abundance
ecodata::plot_narw(varName = "adult", n = 10) +
  ggplot2::ggtitle("North Atlantic right whale abundance")
ggplot2::ggsave(
  create_filename("narw_abundance"),
  width = 6.5,
  height = 4
)

# narw calves
ecodata::plot_narw(varName = "calf", n = 10) +
  ggplot2::ggtitle("North Atlantic right whale calf abundance")
ggplot2::ggsave(
  create_filename("narw_calves"),
  width = 6.5,
  height = 4
)

### Implications ----

# seals
ecodata::plot_seal_pups(report = region) +
  ggplot2::theme(legend.position = 'bottom')
ggplot2::ggsave(
  create_filename("seal_pups"),
  width = 6.5,
  height = 4
)

# Risks to meeting fishery management objectives ----

## Climte and ecosystem change ----

### Risks to managing spatially ----

# species dist
# TODO: these xlims are outdated?
a <- ecodata::plot_species_dist(varName = "along", n = 10) +
  ggplot2::coord_cartesian(xlim = c(1969, 2021))
b <- ecodata::plot_species_dist(varName = "depth", n = 10) +
  ggplot2::coord_cartesian(xlim = c(1969, 2021))

ggpubr::ggarrange(a, b, ncol = 2)

ggplot2::ggsave(
  create_filename("species_dist"),
  width = 6.5,
  height = 4
)

# protectedspp-dist-shifts
ecodata::plot_cetacean_dist() +
  ggplot2::ggtitle("Whale and Dolphin Distribution Shifts")
ggplot2::ggsave(
  create_filename("cetacean_dist"),
  width = 6.5,
  height = 4
)

# forage shifts
## TODO: move these aesthetic updates into ecodata function
ecodata::plot_forage_index(varName = "cog", n = 10) +
  ggplot2::coord_cartesian(xlim = c(1982, 2023)) +
  ggplot2::ggtitle("Northeast U.S. Forage Fish Distribution") +
  ggplot2::ylab("Center of Gravity, km")
ggplot2::ggsave(
  create_filename("forage_dist"),
  width = 6.5,
  height = 4
)

# macrobenthos shifts
## TODO: move these aesthetic updates into ecodata function
ecodata::plot_benthos_index(
  plottype = "cog",
  varName = "Macrobenthos",
  n = 10
) +
  ggplot2::coord_cartesian(xlim = c(1980, 2023)) +
  ggplot2::ggtitle("Northeast U.S. Macrobenthos Distribution") +
  ggplot2::ylab("Center of Gravity, km")
ggplot2::ggsave(
  create_filename("macrobenthos_dist"),
  width = 6.5,
  height = 4
)

# longterm sst
ecodata::plot_long_term_sst(n = 10)
ggplot2::ggsave(
  create_filename("long_term_sst"),
  width = 6.5,
  height = 4
)

# gsi
ecodata::plot_gsi(varName = "westgsi", n = 10)
ggplot2::ggsave(
  create_filename("west_gsi"),
  width = 6.5,
  height = 4
)

# cold pool size
a <- ecodata::plot_cold_pool(varName = "cold_pool", n = 10)
b <- ecodata::plot_cold_pool(varName = "extent", n = 10)

ggpubr::ggarrange(a, b, ncol = 2)
ggplot2::ggsave(
  create_filename("cold_pool"),
  width = 6.5,
  height = 4
)

### Risks to managing seasonally ----

# spawn timing
ecodata::plot_spawn_timing(n = 10) +
  ggplot2::ggtitle("Spring Resting Maturity Stage")
ggplot2::ggsave(
  create_filename("spawn_timing"),
  width = 6.5,
  height = 4
)

# transition date
## TODO: move aesthetics into ecodata function
ecodata::plot_trans_dates(report = region, varName = "length", n = 10) +
  ggplot2::ggtitle(paste(
    "Time between spring and fall transition in",
    full_region
  )) +
  ggplot2::theme(
    strip.background = ggplot2::element_blank(),
    strip.text.x = ggplot2::element_blank()
  )
ggplot2::ggsave(
  create_filename("transition_date"),
  width = 6.5,
  height = 4
)

# 1. Cold Pool Time Plot
ecodata::plot_cold_pool(varName = "persistence", n = 10)
ggplot2::ggsave(
  create_filename("cold_pool_time"),
  width = 6.5,
  height = 4
)
ggplot2::ggsave(
  create_filename("cold_pool_time"),
  width = 6.5,
  height = 4
)

# 2. Monthly Chlorophyll Plot
ecodata::plot_chl_pp(report = region, plottype = "monthly", n = 100)
ggplot2::ggsave(
  create_filename("monthly_chl"),
  width = 6.5,
  height = 4
)

### Risks to setting catch limits ----
# productivity + recruitment anomalies
## productivity
## TODO: move aesthetics into ecodata function
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

## recruitment
## TODO: move aesthetics into ecodata function
recruit_anomaly_plot <- recruit_anomaly_plot <- ecodata::plot_productivity_anomaly(
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

## combined anomaly plot
ggpubr::ggarrange(
  productivity_anomaly_plot,
  recruit_anomaly_plot,
  ncol = ifelse(region == "MidAtlantic", 2, 1)
)

ggplot2::ggsave(
  create_filename("productivity_anomaly"),
  width = 6.5,
  height = 4
)

# condition factor
ecodata::plot_condition(report = region) +
  ggplot2::theme(
    legend.text = ggplot2::element_text(size = 10),
    legend.title = ggplot2::element_text(size = 11),
    axis.text.x = ggplot2::element_text(size = 12),
    axis.text.y = ggplot2::element_text(size = 10),
    plot.title = ggplot2::element_text(size = 12)
  )
ggplot2::ggsave(
  create_filename("condition"),
  width = 6.5,
  height = 4
)

# 5. Energy Density Plot
ecodata::plot_energy_density(report = region)
ggplot2::ggsave(
  create_filename("energy_density"),
  width = 6.5,
  height = 4
)

# 6. Forage Index Plot
ecodata::plot_forage_index(report = region, n = 10)
ggplot2::ggsave(
  create_filename("foragebio"),
  width = 6.5,
  height = 4
)

# 7. Benthos Plot
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
ggplot2::ggsave(
  create_filename("benthos"),
  width = 6.5,
  height = 4
)

# 8. Zooplankton Anomaly Plot
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
ggplot2::ggsave(
  create_filename("zooplankton_anomaly"),
  width = 6.5,
  height = 4
)

# 9. Thermal Habitat Persistence Plot
ecodata::plot_thermal_habitat_gridded(region)
ggplot2::ggsave(
  create_filename("therm_hab_persist"),
  width = 6.5,
  height = 4
)

# Other ocean uses: offshore wind ----

# development speed
ecodata::plot_wind_dev_speed()
ggplot2::ggsave(
  create_filename("wind_dev_speed"),
  width = 6.5,
  height = 4
)

# 1. Wind Species Revenue Plot
wea_spp_rev_plot <- ecodata::plot_wind_revenue(
  report = region,
  varName = "value",
  plottype = "nofacets"
)
ggplot2::ggsave(
  create_filename("wind_revenue"),
  width = 6.5,
  height = 4
)

# 3. Wind Port Revenue Plot
wea_port_rev_plot <- ecodata::plot_wind_port(report = region)
ggplot2::ggsave(
  create_filename("wea_port_rev"),
  width = 6.5,
  height = 4
)

# Highlights ----

ecodata::plot_slopewater(report = "NewEngland")
ggplot2::ggsave(
  create_filename("slopewater"),
  width = 6.5,
  height = 4
)
