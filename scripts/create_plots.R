### INDICATORS WITH A '# FUNCITON ADJUSTED IN ECODATA' LINE HAVE HAD THE ADDITIONS MADE HERE ADDED TO THE ECODATA PLOT CODE
### THE LINES OF CODE ADDED TO ECODATA FUNCTIONS HAVE BEEN COMMENTED OUT BUT NOT REMOVED FROM THE FUNCTIONS IN THIS SCRIPT
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
# FUNCTION ADJUSTED IN ECODATA
save_plot(
  plot_expression = {
    plt <- ecodata::plot_comdat(
      report = region,
      varName = "landings",
      n = 10
    )
#    if (region == "MidAtlantic") {
 #     plt +
  #      ggplot2::theme(strip.text.x = ggplot2::element_blank())
   # } else {
    #  plt +
     #   ggplot2::ylab(expression("Landings (10"^3 * " metric tons)")) +
      #  ggplot2::facet_wrap(~EPU,
       #                     nrow = 2)
  #  }
  },
  indicator = "total_landings",
  width = 6.5,
  height = ifelse(region == "NewEngland", 5, 4),
)

# commercial landings
# FUNCTION ADJUSTED IN ECODATA
save_plot(
  plot_expression = {
    ecodata::plot_comdat(
      report = region,
      plottype = "guild",
      n = 10
    ) 
  #  +
   #  ggplot2::geom_point(size = 0.05) +
    #  ggplot2::geom_line(size = 0.05)
  },
  indicator = "commercial_landings",
  width = 6.5,
  height = 7
)

# climate vulnerability landings
# FUNCTION ADJUSTED IN ECODATA
save_plot(
  plot_expression = {
    plot_community_climate_vulnerability(
      report = region,
      plottype = "regionland",
      n = 100
    ) 
  #+
   #   ggplot2::ylab("Total Climate Vulnerability \n (Regional Landings)") +
    #  ggplot2::theme(legend.position = 'bottom')
  },
  indicator = "climatevul_land",
  width = 6.5,
  height = 2.5
)

# rec landings
# NO CHANGE TO ECODATA FUNCTION NEEDED
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
# FUNCTIONS (REC_HMS AND LPS_SHARKS) ADJUSTED IN ECODATA
save_plot(
  plot_expression = {
    rec_hms_plot <- ecodata::plot_rec_hms(report = region, n = 100)
    #+
     # ggplot2::scale_color_discrete(
      #  limits = c("LargeCoastal", "Prohibited", "SmallCoastal"),
       # labels = c("Large Coastal", "Prohibited", "Small Coastal")
    #  ) +
     # ggplot2::ggtitle(paste(region2, "Marine Recreational Information Program (MRIP) Rec. Shark Landings"))
    rec_lps_sharks_plot <- ecodata::plot_lps_sharks(
      report = region,
      n = 100
    ) 
    #+
     # ggplot2::ggtitle(paste(region2, "Large Pelagics Survey Rec. Shark Landings")) +
      #ggplot2::theme(legend.background = ggplot2::element_rect(fill = "white"))
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
# NEED SOME WAY TO SAVE AS $p OBJECT FROM ecodata FUNCTION, OTHERWISE, FUNCTION WORKS 
save_plot(
  plot_expression = {
    plot_stock_status(report = region)
   # if (region == "MidAtlantic") {
    #  stock_status_plot$p +
     #   ggplot2::coord_cartesian(xlim = c(0, 2), ylim = c(0, 2)) +
      #  ggplot2::theme(legend.position = 'bottom')
#    } else {
 #     stock_status_plot$p +
  #      ggplot2::theme(legend.position = 'bottom')
   # }
  },
  indicator = "stock_status",
  width = 6.5,
  height = 4
)

# 2. ABC/ACL Stacked Plot OLD VERSION DO NOT USE
#save_plot(
 # plot_expression = {
  #  ecodata::plot_abc_acl(
   #   report = region,
    #  plottype = "Stacked"
#    )
 # },
  #indicator = "abcacl_stacked",
  #width = 7,
  #height = 4
#)

# 2. ABC/ACL Stacked Plot NEW 
# uses the function plot_abc_acl in 'scripts' folder, not current ecodata function

#FOR MAB
# FUNCTION ADJUSTED IN ECODATA CODE
save_plot(
  plot_expression = {
    plot_abc_acl(
      report = "MidAtlantic",
      plottype = "Stacked"
    )
  },
  indicator = "abcacl_stacked_new",
  width = 6.5,
  height = 7
)

#FOR NE
# NEEDS ADJUSTMENTS, CANNOT SEE COMBINED AND YLABS ARE TOO BIG 
save_plot(
  plot_expression = {
    plot_abc_acl(
      report = "NewEngland",
      plottype = "Stacked"
    )
  },
  indicator = "abcacl_stacked_new",
  width = 6.5,
  height = 7
)

# 3. ABC/ACL Catch Plot
# NO CHANGE TO ECODATA FUNCTION NEEDED
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
# FUNCTION ADJUSTED IN ECODATA, CHECK FOR MAB
save_plot(
  plot_expression = {
    comm_revenue_plot <- plot_comdat(
      report = region,
      varName = "revenue",
      n = 10
    )
#    if (region == "MidAtlantic") {
 #     comm_revenue_plot +
  #      ggplot2::theme(
   #       legend.position = "right",
    #      legend.title = ggplot2::element_blank()
     #   )
#    } else {
 #     comm_revenue_plot + 
  #      ggplot2::facet_wrap(~EPU,
   #                         nrow = 2)
    #}
  },
  indicator = "comm_revenue",
  width = 6.5,
  height = ifelse(region == "NewEngland", 5, 2.5)
)

# bennet
# FUNCTION PARTIALLY ADJUSTED IN ECODATA. FOR NE, GB FIXES AREN'T REFLECTED IN PLOT. NEED TO SEPARATE GB AND GOM IN ECODATA CODE.
save_plot(
  plot_expression = {
    if (region == "MidAtlantic") {
      plot_bennet(
        report = region,
        varName = "total"
      ) 
      #+
      #  ggplot2::theme(text = ggplot2::element_text(size = 14)) +
       # ggplot2::theme(
        #  legend.position = "bottom")
    } else {
      gb <- plot_bennet(
        report = region,
        varName = "total",
        EPU = "GB"
      ) 
#      +
 #      ggplot2::ggtitle("GB revenue components") +
  #      ggplot2::theme(
   #      legend.position = "none",
    #      legend.title = ggplot2::element_blank())
        # +
      #  ggplot2::ylab("Million USD (2023)") +
       # ggplot2::theme(text = ggplot2::element_text(size = 12)) 
      gom <- plot_bennet(
        report = region,
        varName = "total",
        EPU = "GOM"
      ) 
   # +
   #   ggplot2::ggtitle("GOM revenue components") 
  #      ggplot2::theme(
   #       legend.position = "bottom",
    #      legend.title = ggplot2::element_blank()
     #   ) +
      #  ggplot2::ylab("Million USD (2023)") +
       # ggplot2::theme(text = ggplot2::element_text(size = 12)) 

        ggpubr::ggarrange(gb, gom, nrow = 2)
    }
  },
  indicator = "bennet",
  width = 6.5,
  height = ifelse(region == "NewEngland", 8, 4)
)

# bennet all
# FUNCTION ADJUSTED IN ECODATA
save_plot(
  plot_expression = {
    if (region == "MidAtlantic") {
      ecodata::plot_bennet(report = region) 
 #     +
  #      ggplot2::theme(legend.position = "bottom") +
   #     ggplot2::facet_wrap(~Var,
    #                        nrow = 2)
    } else {
      gb <- ecodata::plot_bennet(
        report = "NewEngland",
        varName = "guild",
        EPU = "GB"
      ) 
#      +
 #       ggplot2::ylab("Million USD (2023)") +
  #      ggplot2::theme(
   #       legend.position = "none",
    #      legend.title = ggplot2::element_blank()) +
     #   ggplot2::theme(strip.text.y = ggplot2::element_blank())
      gom <- ecodata::plot_bennet(
        report = "NewEngland",
        varName = "guild",
        EPU = "GOM"
      ) 
 #     +
  #      ggplot2::ylab("Million USD (2023)") +
   #     ggplot2::theme(legend.position = 'bottom') +
    #    ggplot2::theme(strip.text.y = ggplot2::element_blank())
      
      ggpubr::ggarrange(gb, gom, nrow = 2, common.legend = TRUE, legend = "bottom")  
    }
  },
  indicator = "bennet_all",
  width = 6.5,
  height = 5
)

# 4. Climate Vulnerability Revenue Plot
# FUNCTION ADJUSTED IN ECODATA 
save_plot(
  plot_expression = {
    plot_community_climate_vulnerability(
      report = region,
      plottype = "regionrev",
      n = 100
    ) 
#    +
 #     ggplot2::theme(legend.position = "bottom") +
  #    ggplot2::ylab("Total Climate Vulnerability \n (Regional Revenue)") 
  },
  indicator = "climatevul_rev",
  width = 6.5,
  height = 2.5
)

## Recreational opportunities ----

### Indicators ----

# 1. Recreational Operational Plot
# NO CHANGE TO ECODATA FUNCTION NEEDED
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
# FUNCTION ADJUSTED IN ECODATA
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
  height = 2.5
)

## Stability ----

### Indicators ----

# 1. Commercial Diversity Fleet Plot
# FUNCTION ADJUSTED IN ECODATA
save_plot(
  plot_expression = {
    a <- ecodata::plot_commercial_div(
      report = region,
      varName = "Fleet count",
      n = 100
    ) 
#    +
 #     ggplot2::theme(plot.margin = ggplot2::unit(c(0.25, 0.5, 0.25, 0.5), "cm"))
    b <- ecodata::plot_commercial_div(
      report = region,
      varName = "Fleet diversity in revenue",
      n = 100
    ) 
#    +
 #     ggplot2::theme(plot.margin = ggplot2::unit(c(0.25, 0.5, 0.25, 0.5), "cm"))
    ggpubr::ggarrange(a, b, nrow = 2)
  },
  indicator = "comm_div_fleet",
  width = 6.5,
  height = 5
)

# 2. Commercial Diversity Species Diversity Plot
# FUNCTION ADJUSTED IN ECODATA
save_plot(
  plot_expression = {
    plt <- ecodata::plot_commercial_div(
      report = region,
      varName = "Permit revenue species diversity",
      n = 100
    )
 #   if (region == "MidAtlantic") {
  #    plt
   # } else {
    #  plt + ggplot2::ylab('Effective Shannon Index')
#    }
  },
  indicator = "commercial_div_species_div",
  width = 6.5,
  height = 2.5
)

# 3. Recreational Diversity Catch Plot
# NO CHANGE TO ECODATA FUNCTION NEEDED
save_plot(
  plot_expression = {
    ecodata::plot_recdat(report = region, varName = "catchdiversity", n = 10)
  },
  indicator = "recdat_div_catch",
  width = 6.5,
  height = 2.5
)

# total primary production
# FUNCTION ADJUSTED IN ECODATA, KEPT YLAB HERE FOR GB/GOM
save_plot(
  plot_expression = {
    if (region == "MidAtlantic") {
      ecodata::plot_annual_chl_pp(
        report = region,
        varName = "pp",
        plottype = "total",
        EPU = "MAB"
      ) 
#      +
 #       ggplot2::coord_cartesian(ylim = c(2e+07, 4e+07), xlim = c(1998, 2023)) +
  #      ggplot2::ggtitle("MAB Primary Production") +
   #     ggplot2::ylab("Carbon (mt)")
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
# FUNCTION ADJUSTED IN ECODATA
save_plot(
  plot_expression = {
    zoo_diversity_plot <- ecodata::plot_zoo_diversity(report = region, n = 10)
    if (region == "MidAtlantic") {
      zoo_diversity_plot 
 #     +
  #      ggplot2::ggtitle("Zooplankton Diversity") +
   #     ggplot2::theme(plot.title = ggplot2::element_text(vjust = -5)) +
    #    ggplot2::ylab("Shannon Index")
    } else {
      zoo_diversity_plot 
#      +
 #       ggplot2::ggtitle("Zooplankton Diversity") +
  #      ggplot2::facet_wrap(~EPU, nrow = 2)
    }
  },
  indicator = "zoo_diversity",
  width = 6.5,
  height = ifelse(region == "NewEngland", 5, 2.5)
)

# 5. Expected N Plot
# FUNCTION ADJUSTED IN ECODATA
save_plot(
  plot_expression = {
    exp_n_plot <- ecodata::plot_exp_n(report = region, varName = "fall", n = 10)
    if (region == "MidAtlantic") {
      exp_n_plot
#      +
 #     ggplot2::scale_x_continuous(breaks = seq(1968, 2018, by = 10), expand = c(0.01, 0.01)) +
  #    ggplot2::ylab("Number of species / 1000 Individuals") +
   #   ggplot2::theme(axis.title.y = ggplot2::element_text(size = 8),
    #                 legend.position = 'bottom') 
    } else {
      exp_n_plot 
#      +
 #     ggplot2::scale_x_continuous(breaks = seq(1968, 2018, by = 10), expand = c(0.01, 0.01)) +
  #      ggplot2::ylab("Number of species / 1000 Individuals") +
   #     ggplot2::theme(axis.title.y = ggplot2::element_text(size = 8),
    #                   legend.position = 'bottom') +
     #   ggplot2::facet_wrap(~EPU, nrow = 2)
    }
  },
  indicator = "exp_n",
  width = 6.5,
  height = ifelse(region == "NewEngland", 5, 2.5)
)

# finfish traits
# FUNCTION ADJUSTED IN ECODATA
save_plot(
  plot_expression = {
    if (region == "MidAtlantic") {
      plot_finfish_traits(report = region, varName = "length_maturity") 
#      +
 #       ggplot2::theme(legend.position = 'bottom')
    } else {
      plot_finfish_traits(
        report = region,
        varName = 'fecundity',
        n = 10
      ) 
#      +
 #       ggplot2::ylab('Fecundity (number of \noffspring per mature female)') +
  #      ggplot2::theme(legend.position = 'bottom') +
   #     ggplot2::facet_wrap(~EPU, nrow = 2)
    }
  },
  indicator = "traits",
  width = 6.5,
  height = ifelse(region == "NewEngland", 5, 2.5)
)

## Community social and climate vulnerability ----

# 1. Commercial Engagement Plot
# FUNCTION ADJUSTED IN ECODATA
save_plot(
  plot_expression = {
    commercial_engagement_plot <- ecodata::plot_engagement(
      report = region,
      varName = "Commercial"
    )
    if (region == "MidAtlantic") {
      commercial_engagement_plot 
#      +
 #       ggplot2::theme(plot.title = ggplot2::element_text(vjust = 0))
    } else {
      commercial_engagement_plot 
#      +
 #       ggplot2::theme(plot.title = ggplot2::element_text(vjust = 0))
    }
  },
  indicator = "commercial_engagement",
  width = 6.5,
  height = 4
)

# 2. Recreational Engagement Plot
# FUNCTION ADJUSTED IN ECODATA
save_plot(
  plot_expression = {
    recreational_engagement_plot <- ecodata::plot_engagement(
      report = region,
      varName = "Recreational"
    )
    if (region == "MidAtlantic") {
      recreational_engagement_plot 
#      +
 #       ggplot2::theme(plot.title = ggplot2::element_text(vjust = 0))
    } else {
      recreational_engagement_plot 
 #     +
  #      ggplot2::theme(plot.title = ggplot2::element_text(vjust = 0))
    }
  },
  indicator = "recreational_engagement",
  width = 6.5,
  height = 4
)

# 3. Community Climate Vulnerability Exposure Plot
# FUNCTION ADJUSTED IN ECODATA
save_plot(
  plot_expression = {
    ecodata::plot_community_climate_vulnerability(
      report = region,
      n = 100
    ) 
#    +
 #     ggplot2::theme(legend.position = 'bottom')
  },
  indicator = "commvulex",
  width = 6.5,
  height = 4
)

### Risks to managing seasonally ----

# transition date
# FUNCTION ADJUSTED IN ECODATA
save_plot(
  plot_expression = {
    if (region == "MidAtlantic") {
    # TODO: move aesthetics into ecodata function
    ecodata::plot_trans_dates(report = region, varName = "length", n = 10) 
#      +
 #     ggplot2::ggtitle(paste(
  #      "Time between spring and fall transition in",
   #     full_region
    #  )) 
#      +
 #     ggplot2::theme(
  #      strip.background = ggplot2::element_blank(),
   #     strip.text.x = ggplot2::element_blank()
    #  )
    } else {
      ecodata::plot_trans_dates(report = region, varName = "length", n = 10) 
#      +
 #       ggplot2::ggtitle(paste(
  #        "Time between spring and fall transition in",
   #       full_region
    #    )) 
 #     +
  #      ggplot2::theme(
   #       strip.background = ggplot2::element_blank(),
    #      strip.text.x = ggplot2::element_blank()) +
     # ggplot2::facet_wrap(~EPU, nrow = 2)
   }
  },
  indicator = "transition_date",
  width = 6.5,
  height = ifelse(region == "NewEngland", 5, 2.5)
)


# Monthly Chlorophyll Plot
# FUNCTION ADJUSTED IN ECODATA
save_plot(
  plot_expression = {
ecodata::plot_chl_pp(
  report = region,
  plottype = "monthly", n = 10
) 
#    + ggplot2::facet_grid(rows = ggplot2::vars(EPU), cols = ggplot2::vars(Month)) +
 #     ggplot2::theme(text = ggplot2::element_text(size = 16)) +
  #     ggplot2::geom_point(color = "white") + ggplot2::geom_line() +
   #    ggplot2::scale_x_discrete(breaks = scales::pretty_breaks(n = 1)) + 
    #  ggplot2::theme(axis.text.x = ggplot2::element_text(size = 8), panel.border = ggplot2::element_rect(color = "gray80"))
  },
indicator = "monthly_chl",
width = 6.5,
height = ifelse(region == "NewEngland", 7, 4)
)

### Risks to setting catch limits ----
# productivity + recruitment anomalies
# FUNCTION ADJUSTED IN ECODATA
save_plot(
  plot_expression = {
    # TODO: move aesthetics into ecodata function
    productivity_anomaly_plot <- ecodata::plot_productivity_anomaly(
      report = region,
      EPU = dplyr::case_when(
        region == "NewEngland" ~ "GOM",
        region == "MidAtlantic" ~ "MAB"
      )
    ) 
#    +
 #     ggplot2::guides(
  #      fill = ggplot2::guide_legend(
   #       ncol = dplyr::case_when(
    #        region == "NewEngland" ~ 3,
     #       region == "MidAtlantic" ~ 2,
      #      TRUE ~ 2
#          )
 #       )
  #    ) +
   #   ggplot2::theme(
    #    legend.position = "bottom",
     #   legend.title = ggplot2::element_blank(),
      #  legend.text = ggplot2::element_text(size = 8),
       # plot.title = ggplot2::element_text(size = 11),
#        axis.text = ggplot2::element_text(size = 11),
 #       axis.title.y = ggplot2::element_text(vjust = 0, size = 10)
  #    )
    # TODO: move aesthetics into ecodata function
    recruit_anomaly_plot <- ecodata::plot_productivity_anomaly(
      report = region,
      varName = "assessment"
    ) 
#    +
 #     ggplot2::guides(fill = ggplot2::guide_legend(ncol = 2)) +
  #    ggplot2::theme(
   #     legend.position = "bottom",
    #    legend.title = ggplot2::element_blank(),
     #   legend.text = ggplot2::element_text(size = 8),
      #  plot.title = ggplot2::element_text(size = 11),
       # axis.text = ggplot2::element_text(size = 11),
  #      axis.title.y = ggplot2::element_text(vjust = 0, size = 10)
   #   )
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
# FUNCTION ADJUSTED IN ECODATA
save_plot(
  plot_expression = {
    if (region == "MidAtlantic") {
      ecodata::plot_condition(report = region) 
#      +
 #       ggplot2::theme(
  #        legend.text = ggplot2::element_text(size = 10),
   #       legend.title = ggplot2::element_text(size = 10),
    #     axis.text.x = ggplot2::element_text(size = 12),
     #    axis.text.y = ggplot2::element_text(size = 8),
      #    plot.title = ggplot2::element_text(size = 12),
       #   legend.position = "bottom",
        #) +
       #ggplot2::guides(fill= ggplot2::guide_legend(nrow=2,byrow=TRUE))
    } else {
      gb <- ecodata::plot_condition(report = region, EPU = "GB") 

      gom <- ecodata::plot_condition(report = region, EPU = "GOM") 

      # change to ggarrange
      ggpubr::ggarrange(gb, gom, ncol = 1, common.legend = TRUE, legend = "bottom") +
        patchwork::plot_layout(guides = 'collect') 
#        ggplot2::theme(
 #         legend.text = ggplot2::element_text(size = 10),
  #        legend.title = ggplot2::element_text(size = 11),
   #       axis.text.x = ggplot2::element_text(size = 12),
    #      axis.text.y = ggplot2::element_text(size = 12),
     #     plot.title = ggplot2::element_text(size = 12)
      #  )
    }
  },
  indicator = "condition",
  width = 6.5,
  height = 7
 # height = ifelse(region == "NewEngland", 7, 6)
)

# 5. Energy Density Plot
# FUNCTION ADJUSTED IN ECODATA
save_plot(
  plot_expression = {
    ecodata::plot_energy_density(report = region)
#    +
 #     ggplot2::theme(legend.position = 'bottom')
  },
  indicator = "energy_density",
  width = 6.5,
  height = 4
)

# 6. Forage Index Plot
# FUNCTION ADJUSTED IN ECODATA
save_plot(
  plot_expression = {
    plt <- ecodata::plot_forage_index(report = region, n = 10)
    if (region == "MidAtlantic") {
      plt
    } else {
      plt 
  #    + ggplot2::facet_wrap(~EPU, nrow = 2)
    }
  },
  indicator = "foragebio",
  width = 6.5,
  height = ifelse(region == "NewEngland", 5, 2.5)
)

# 7. Benthos Plot
# fUNCTION ADJUSTED IN ECODATA
save_plot(
  plot_expression = {
    megabenthos_plot <- ecodata::plot_benthos_index(
      report = region,
      varName = "Megabenthos",
      n = 10
    ) 
#    +
 #     ggplot2::theme(legend.position = "none") +
  #    ggplot2::geom_point(ggplot2::aes(color = .data$Season)) + 
   #   ggplot2::geom_line(ggplot2::aes(color = .data$Season)) 
    macrobenthos_plot <- ecodata::plot_benthos_index(
      report = region,
      varName = "Macrobenthos",
      n = 10
    ) 
 #   +
  #    ggplot2::geom_point(ggplot2::aes(color = .data$Season)) + 
   #   ggplot2::geom_line(ggplot2::aes(color = .data$Season)) 
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
# FUNCTION ADJUSTED IN ECODATA
save_plot(
  plot_expression = {
    large_copepod_plot <- ecodata::plot_zooplankton_index(
      report = region,
      varName = "Lgcopeall",
      n = 10
    ) 
#    +
 #     ggplot2::ylab("Relative Biomass") +
  #    ggplot2::labs(title = "Large Copepods") +
   #   ggplot2::theme(strip.text.x = ggplot2::element_blank()) +
    #  ggplot2::geom_point(ggplot2::aes(color = .data$Season)) + 
     # ggplot2::geom_line(ggplot2::aes(color = .data$Season)) 
    small_copepod_plot <- ecodata::plot_zooplankton_index(
      report = region,
      varName = "Smallcopeall",
      n = 10
    ) 
#    +
 #     ggplot2::ylab("Relative Biomass") +
  #    ggplot2::labs(title = "Small Copepods") +
   #   ggplot2::theme(strip.text.x = ggplot2::element_blank()) +
    #  ggplot2::geom_point(ggplot2::aes(color = .data$Season)) + 
     # ggplot2::geom_line(ggplot2::aes(color = .data$Season)) 
    euphausiid_plot <- ecodata::plot_zooplankton_index(
      report = region,
      varName = "Euph",
      n = 10
    ) 
#    +
 #     ggplot2::theme(legend.background = ggplot2::element_rect(fill = "white")) +
  #    ggplot2::ylab("Relative Biomass") +
   #   ggplot2::labs(title = "Euphasiids") +
    #  ggplot2::theme(strip.text.x = ggplot2::element_blank()) +
     # ggplot2::geom_point(ggplot2::aes(color = .data$Season)) + 
      #ggplot2::geom_line(ggplot2::aes(color = .data$Season)) 
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
# FUNCTION ADJUSTED IN ECODATA
save_plot(
  plot_expression = {
    plt <- ecodata::plot_thermal_habitat_gridded(region) 
#    +
 #     ggplot2::geom_tile(ggplot2::aes(x=Longitude,y = Latitude, color = Value, width = 0.0025, height = 0.0025)) 
    if (region == "MidAtlantic") {
      plt
    } else {
      plt
 #     + ggplot2::theme(plot.margin = grid::unit(c(0, 0, 0, 0), "cm"))
    }
  },
  indicator = "therm_hab_persist",
  width = 6.5,
  height = 4
)

# Other ocean uses: offshore wind ----

# 1. Wind Species Revenue Plot
# FUNCTION ADJUSTED IN ECODATA
save_plot(
  plot_expression = {
    ecodata::plot_wind_revenue(
      report = region,
      varName = "value",
      plottype = "nofacets"
    )
#    +
 #     ggplot2::theme(legend.position = "bottom")
  },
  indicator = "wind_revenue",
  width = 6.5,
  height = 4
)

# 3. Wind Port Revenue Plot
#save_plot(
 # plot_expression = {
  #  ecodata::plot_wind_port(report = region) +
   # ggplot2::theme(axis.text.y = ggplot2::element_text(size = 6))
#  },
 # indicator = "wea_port_rev",
  #width = 7.5,
#  height = 4
#)

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
# NO CHANGE TO ECODATA FUNCTION NEEDED
save_plot(
  plot_expression = {
    ecodata::plot_harborporpoise()
  },
  indicator = "harborporpoise",
  width = 6.5,
  height = 4
)

# gray seal
# NO CHANGE TO ECODATA FUNCTION NEEDED
save_plot(
  plot_expression = {
    ecodata::plot_grayseal()
  },
  indicator = "grayseal",
  width = 6.5,
  height = 4
)

# narw-abundance
# FUNCTION ADJUSTED IN ECODATA
save_plot(
  plot_expression = {
      ecodata::plot_narw(varName = "adult", n = 10)
    # + ggplot2::ggtitle("North Atlantic right whale abundance")
  },
  indicator = "narw_abundance",
  width = 6.5,
  height = 2.5
)

# narw calves
# FUNCTION ADJUSTED IN ECODATA
save_plot(
  plot_expression = {
    ecodata::plot_narw(varName = "calf", n = 10) 
  #  + ggplot2::ggtitle("North Atlantic right whale calf abundance")
  },
  indicator = "narw_calves",
  width = 6.5,
  height = 2.5
)

# seals
# FUNCTION ADJUSTED IN ECODATA
save_plot(
  plot_expression = {
    # for both reports, even though function calls NE
    ecodata::plot_seal_pups(report = "NewEngland")
 #   + ggplot2::theme(legend.position = 'bottom')
  },
  indicator = "seal_pups",
  width = 6.5,
  height = 4
)

# species dist
# FUNCTION ADJUSTED IN ECODATA
save_plot(
  plot_expression = {
    a <- ecodata::plot_species_dist(varName = "along", n = 10) 
    # + ggplot2::coord_cartesian(xlim = c(1969, 2021))
    b <- ecodata::plot_species_dist(varName = "depth", n = 10) 
    # + ggplot2::coord_cartesian(xlim = c(1969, 2021))
    ggpubr::ggarrange(a, b, nrow = 2)
  },
  indicator = "species_dist",
  width = 6.5,
  height = 5
)

# whale and dolphin dist shifts
# FUNCTION ADJUSTED IN ECODATA
save_plot(
  plot_expression = {
    ecodata::plot_cetacean_dist() 
   # + ggplot2::ggtitle("Whale and Dolphin Distribution Shifts") +
    #  ggplot2::facet_wrap(~season, nrow = 1) +
     # ggplot2::theme(legend.position = "bottom") 
  },
  indicator = "cetacean_dist",
  width = 7.5,
  height = 4
)

# forage shifts
# FUNCTION ADJUSTED IN ECODATA
save_plot(
  plot_expression = {
    ecodata::plot_forage_index(varName = "cog", n = 10) 
#     + ggplot2::coord_cartesian(xlim = c(1982, 2023)) +
 #     ggplot2::ggtitle("Northeast U.S. Forage Fish Distribution") +
  #    ggplot2::ylab("Center of Gravity, km") + 
   # ggplot2::geom_point(ggplot2::aes(color = .data$Season)) + 
#      ggplot2::geom_line(ggplot2::aes(color = .data$Season)) +
 #     ggplot2::facet_wrap(~Var, nrow = 2) +
  #  ggplot2::theme(legend.position = "bottom") +
   #   ggplot2::facet_grid(cols = ggplot2::vars(Season), rows = ggplot2::vars(Direction), scales = "free_y")
  },
  indicator = "forage_dist",
  width = 6.5,
  height = 5
)

# macrobenthos shifts
# FUNCTION ADJUSTED IN ECODATA
save_plot(
  plot_expression = {
    ecodata::plot_benthos_index(
      plottype = "cog",
      varName = "Macrobenthos",
      n = 10
    ) 
#    +
 #     ggplot2::coord_cartesian(xlim = c(1980, 2023)) +
  #    ggplot2::ggtitle("Northeast U.S. Macrobenthos Distribution") +
   #   ggplot2::ylab("Center of Gravity, km") +
    #  ggplot2::geom_point(ggplot2::aes(color = .data$Season)) + 
     # ggplot2::geom_line(ggplot2::aes(color = .data$Season)) +
#      ggplot2::theme(legend.position = 'bottom') +
 #     ggplot2::facet_grid(cols = ggplot2::vars(Season), rows = ggplot2::vars(Direction), scales = "free_y")
  },
  indicator = "macrobenthos_dist",
  width = 6.5,
  height = 5
)

# longterm sst
# NO CHANGE TO ECODATA FUNCTION NEEDED
save_plot(
  plot_expression = {
    ecodata::plot_long_term_sst(n = 10)
  },
  indicator = "long_term_sst",
  width = 6.5,
  height = 2.5
)

# gsi
# NO CHANGE TO ECODATA FUNCTION NEEDED
save_plot(
  plot_expression = {
    ecodata::plot_gsi(varName = "westgsi", n = 10)
  },
  indicator = "west_gsi",
  width = 6.5,
  height = 2.5
)

# cold pool size
# NO CHANGE TO ECODATA FUNCTION NEEDED 
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
# NO CHANGE TO ECODATA FUNCTION NEEDED
save_plot(
  plot_expression = {
    ecodata::plot_cold_pool(varName = "persistence", n = 10)
  },
  indicator = "cold_pool_time",
  width = 6.5,
  height = 2.5
)

# spawn timing
# FUNCTION ADJUSTED IN ECODATA
save_plot(
  plot_expression = {
    ecodata::plot_spawn_timing(n = 10)
   # +  ggplot2::ggtitle("Spring Resting Maturity Stage")
  },
  indicator = "spawn_timing",
  width = 6.5,
  height = 4
)


# development speed
# FUNCTION ADJUSTED IN ECODATA
save_plot(
  plot_expression = {
    plot_wind_dev_speed() 
  #  + ggplot2::theme(legend.position = 'bottom')
  },
  indicator = "wind_dev_speed",
  width = 6.5,
  height = 4
)

# slopewater
# NO CHANGE TO ECODATA FUNCTION NEEDED
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
######### KEEP THESE???? 

region <- "NewEngland"
out_dir <- here::here("images", region)

# calfin center of gravity -- NE only
# FUNCTION ADJUSTED IN ECODATA
save_plot(
  plot_expression = {
    ecodata::plot_zooplankton_index(
      report = region,
      varName = 'Calfin',
      plottype = 'cog',
      n = 10
    )
    #+
     # ggplot2::theme(legend.position = 'bottom')
  },
  indicator = "calfin_cog",
  width = 6.5,
  height = 4
)

# mass inshore survey -- NE only
# FUNCTION ADJUSTED IN ECODATA
save_plot(
  plot_expression = {
    ecodata::plot_mass_inshore_survey(report = region, n = 10) 
    #+
    #  ggplot2::geom_point()+
     # ggplot2::geom_line()
  },
  indicator = "mass_inshore",
  width = 6,
  height = 6
)

# seabird productivity -- NE only
# FUNCTION ADJUSTED IN ECODATA
if (region == "NewEngland") {
  save_plot(
    plot_expression = {
      plot_seabird_ne(varName = "productivity", n = 10) 
      #+
        ## TODO: update these xlim
       # ggplot2::coord_cartesian(xlim = c(1992, 2023))
    },
    indicator = "seabird_productivity",
    width = 6.5,
    height = 2.5
  )
}

# salmon -- NE only
# FUNCTION ADJUSTED IN ECODATA
if (region == "NewEngland") {
  save_plot(
    plot_expression = {
      ecodata::plot_gom_salmon(n = 10)
      #+
       # ggplot2::ylab('Returning proportion') +
        #ggplot2::facet_wrap(~Var, nrow = 2, scales = "free_y") 
    },
    indicator = "salmon",
    width = 6.5,
    height = 5
  )
}

