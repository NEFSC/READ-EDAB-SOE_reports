# reinstall ecodata
devtools::install_github("NOAA-EDAB/ecodata", ref = "a66530e")

# setup ----

## variables ----

region <- "NewEngland" #change to NewEngland to run for NE
region <- "MidAtlantic" #change to NewEngland to run for NE

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

####### PLOTS THAT DIFFER FOR MAB/NE####### ----

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
      plt +
           ggplot2::theme(strip.text.x = ggplot2::element_blank())
    } else {
      plt +
         ggplot2::ylab(expression("Landings (10"^3 * " metric tons)")) +
        ggplot2::facet_wrap(~EPU,
                           nrow = 2)
    }
  },
  indicator = "total_landings",
  width = 6.5,
  height = ifelse(region == "NewEngland", 5, 3),
)

# commercial landings
save_plot(
  plot_expression = {
    ecodata::plot_comdat(
      report = region,
      plottype = "guild",
      n = 10
    ) +
      ggplot2::geom_point(size = 0.05) +
      ggplot2::geom_line(size = 0.05)
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
      n = 24
    ) +
      ggplot2::ylab("Total Climate Vulnerability \n (Regional Landings)") +
      ggplot2::theme(legend.position = 'bottom')
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
# rec hms and sharks
save_plot(
  plot_expression = {
    rec_hms_plot <- ecodata::plot_rec_hms(report = region, n = 10) +
      ggplot2::scale_color_discrete(
        limits = c("LargeCoastal", "SmallCoastal", "Prohibited"),
        labels = c("Large Coastal", "Small Coastal", "Prohibited")
      ) +
      ggplot2::ggtitle(paste(
        region2,
        "Marine Recreational Information Program (MRIP) Rec. Shark Landings"
      )) +
      ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white"))
    rec_lps_sharks_plot <- ecodata::plot_lps_sharks(
      report = region,
      n = 10
    ) +
      ggplot2::ggtitle(paste(
        region2,
        "Large Pelagics Survey Rec. Shark Landings"
      )) +
      ggplot2::theme(legend.background = ggplot2::element_rect(fill = "white"))
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
  height = 5
)

#NE only - rec_hms from LPS only, no MRIP
save_plot(
  plot_expression = {
    rec_lps_sharks_plot <- ecodata::plot_lps_sharks(
      report = region,
      n = 10
    )  +
      ggplot2::ggtitle(paste(region2, "Large Pelagics Survey Rec. Shark Landings")) +
      ggplot2::theme(legend.background = ggplot2::element_rect(fill = "white"))
  },
  indicator = "rec_hms_NE",
  width = 6.5,
  height = 4.5
)

### Implications ----

# 1. Stock Status Plot
save_plot(
  plot_expression = {
    stock_status_plot <- ecodata::plot_stock_status(report = region)
    if (region == "MidAtlantic") {
      stock_status_plot$p 
    } else {
      stock_status_plot$p 
    }
  },
  indicator = "stock_status",
  width = 6.5,
  height = 6
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
  height = 4.5
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
if (region == "MidAtlantic") {
  save_plot(
    plot_expression = {
      plot_aggregate_biomass(report = region, EPU = "MAB", n = 10)
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
      plot_aggregate_biomass(
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
      plot_aggregate_biomass(
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
            ggplot2::scale_x_continuous(
              breaks = seq(1980, 2020, by = 5),
              expand = c(0.01, 0.01)
            ) +
          ggplot2::theme(
           legend.position = "bottom",
          legend.title = ggplot2::element_blank()
       )
        } else {
         comm_revenue_plot + 
          ggplot2::facet_wrap(~EPU,
                             nrow = 2)
    }
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
      ) +
        ggplot2::theme(text = ggplot2::element_text(size = 14)) +
       ggplot2::theme(
        legend.position = "bottom")
    } else {
      gb <- ecodata::plot_bennet(
        report = region,
        varName = "total",
        EPU = "GB"
      )  +
            ggplot2::ggtitle("GB revenue components") +
            ggplot2::theme(
            legend.position = "none",
            legend.title = ggplot2::element_blank()) +
       ggplot2::ylab("Million USD (2023)") +
       ggplot2::theme(text = ggplot2::element_text(size = 12)) 
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
        ggplot2::ylab("Million USD (2023)") +
       ggplot2::theme(text = ggplot2::element_text(size = 12)) 
      
      ggpubr::ggarrange(gb, gom, nrow = 2)
    }
  },
  indicator = "bennet",
  width = 6.5,
  height = ifelse(region == "NewEngland", 6, 3)
) 

# bennet all
save_plot(
  plot_expression = {
    if (region == "MidAtlantic") {
      ecodata::plot_bennet(report = region) +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
            legend.position = "bottom") +
        ggplot2::facet_wrap(~Var, nrow = 2)
    } else {
      gb <- ecodata::plot_bennet(
        report = "NewEngland",
        varName = "guild",
        EPU = "GB"
      ) +
        ggplot2::ylab("Million USD (2023)") +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
      gom <- ecodata::plot_bennet(
        report = "NewEngland",
        varName = "guild",
        EPU = "GOM"
      ) +
        ggplot2::ylab("Million USD (2023)") +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
          legend.position = "bottom")
      ggpubr::ggarrange(gb, gom, ncol = 1,
                        common.legend = TRUE,
                        legend = "bottom") 
    }
  },
  indicator = "bennet_all",
  width = ifelse(region == "NewEngland", 9, 6.5),
  height = 6.5
)

# 4. Climate Vulnerability Revenue Plot
save_plot(
  plot_expression = {
    ecodata::plot_community_climate_vulnerability(
      report = region,
      plottype = "regionrev",
      n = 24
    ) +
         ggplot2::theme(legend.position = "bottom") +
        ggplot2::ylab("Total Climate Vulnerability \n (Regional Revenue)") 
  },
  indicator = "climatevul_rev",
  width = 6.5,
  height = 2.5
)

# Geret's profitability indices (comdat profit) (EPU = MAB)
save_plot(
  plot_expression = {
    if (region == "MidAtlantic") {
      ecodata::plot_comdat_profit(
        report = region,
        n = 23
      ) +
        ggplot2::scale_color_discrete(
          limits = c("cost_index", "profit_index", "revenue_index"),
          labels = c("Cost Index", "Profit Index", "Revenue Index")
        ) +
        ggplot2::theme(legend.position = "bottom")
    }
    
  else {  
   gb <- ecodata::plot_comdat_profit(
      report = region,
      EPU = "GB",
      n = 23
    ) +
      ggplot2::scale_color_discrete(
        limits = c("cost_index", "profit_index", "revenue_index"),
        labels = c("Cost Index", "Profit Index", "Revenue Index")
      ) +
      ggplot2::theme(legend.position = "none") 
   
   gom <- ecodata::plot_comdat_profit(
     report = region,
     EPU = "GOM",
     n = 23
   ) +
     ggplot2::scale_color_discrete(
       limits = c("cost_index", "profit_index", "revenue_index"),
       labels = c("Cost Index", "Profit Index", "Revenue Index")
     ) +
     ggplot2::theme(legend.position = "bottom")
   ggpubr::ggarrange(gb, gom, nrow = 2)
  }
  },
  indicator = "comdat_profit",
  width = 6.5,
  height = ifelse (region == "NewEngland", 8, 4.5)
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
    plt <- ecodata::plot_recdat(
      report = region,
      varName = "effortdiversity",
      n = 10
    )
    if (region == "MidAtlantic") {
      plt
    } else {
      plt   +
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
save_plot(
  plot_expression = {
    a <- ecodata::plot_commercial_div(
      report = region,
      varName = "Fleet count",
      n = 22
    ) +
         ggplot2::theme(plot.margin = ggplot2::unit(c(0.25, 0.5, 0.25, 0.5), "cm"))
    b <- ecodata::plot_commercial_div(
      report = region,
      varName = "Fleet diversity in revenue",
      n = 22
    ) +
         ggplot2::theme(plot.margin = ggplot2::unit(c(0.25, 0.5, 0.25, 0.5), "cm"))
    ggpubr::ggarrange(a, b, nrow = 2)
  },
  indicator = "comm_div_fleet",
  width = 6.5,
  height = 5
)

# 2. Commercial Diversity Species Diversity Plot
save_plot(
  plot_expression = {
    plt <- ecodata::plot_commercial_div(
      report = region,
      varName = "Permit revenue species diversity",
      n = 22
    )
       if (region == "MidAtlantic") {
        plt
     } else {
      plt + ggplot2::ylab('Effective Shannon Index')
        }
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
save_plot(
  plot_expression = {
    if (region == "MidAtlantic") {
      ecodata::plot_annual_chl_pp(
        report = region,
        varName = "pp",
        plottype = "total",
        EPU = "MAB",
        n = 27
      )  +
             ggplot2::coord_cartesian(ylim = c(2e+07, 4e+07), xlim = c(1998, 2023)) +
            ggplot2::ggtitle("MAB Primary Production") +
           ggplot2::ylab("Carbon (mt)")
    } else {
      a <- ecodata::plot_annual_chl_pp(
        report = region,
        varName = "pp",
        plottype = "total",
        EPU = "GB",
        n = 27
      ) + ggplot2::ggtitle('Georges Bank total Primary Production') +
        ggplot2::ylab("Carbon (mt)")
      b <- ecodata::plot_annual_chl_pp(
        report = region,
        varName = "pp",
        plottype = "total",
        EPU = "GOM",
        n = 27
      ) + ggplot2::ggtitle('Gulf of Maine total Primary Production') +
        ggplot2::ylab("Carbon (mt)")
      
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
    if (region == "MidAtlantic") {
      zoo_diversity_plot +
            ggplot2::ggtitle("Zooplankton Diversity") +
           ggplot2::theme(plot.title = ggplot2::element_text(vjust = -5)) +
          ggplot2::ylab("Shannon Index")
    } else {
      zoo_diversity_plot +
             ggplot2::ggtitle("Zooplankton Diversity") +
            ggplot2::facet_wrap(~EPU, nrow = 1, scales = "free_y")
    }
  },
  indicator = "zoo_diversity",
  width = 6.5,
  height = ifelse(region == "NewEngland", 4, 2.5)
)

# 5. Expected N Plot
save_plot(
  plot_expression = {
    exp_n_plot <- ecodata::plot_exp_n(report = region, varName = "fall", n = 10)
    if (region == "MidAtlantic") {
      exp_n_plot  +
           ggplot2::scale_x_continuous(breaks = seq(1968, 2018, by = 10), expand = c(0.01, 0.01)) +
          ggplot2::ylab("Number of species / 1000 Individuals") +
         ggplot2::theme(axis.title.y = ggplot2::element_text(size = 8),
                       legend.position = 'bottom') 
    } else {
      exp_n_plot  +
           ggplot2::scale_x_continuous(breaks = seq(1968, 2018, by = 10), expand = c(0.01, 0.01)) +
            ggplot2::ylab("Number of species / 1000 Individuals") +
           ggplot2::theme(axis.title.y = ggplot2::element_text(size = 8),
                         legend.position = 'bottom') +
         ggplot2::facet_wrap(~EPU, nrow = 2)
    }
  },
  indicator = "exp_n",
  width = 6.5,
  height = ifelse(region == "NewEngland", 5, 3.5)
)

#zooplankton community PCA
save_plot(
  plot_expression = {
    if (region == "MidAtlantic") {
      ecodata::plot_zoo_community(report = region, n = 10)  +
        ggplot2::theme(legend.position = 'bottom')
    } else {
      ecodata::plot_zoo_community(
        report = region,
        n = 10
      )   +
        ggplot2::facet_wrap(~EPU, nrow = 2)
    }
  },
  indicator = "zoo_community",
  width = 6.5,
  height = ifelse(region == "NewEngland", 5, 3.5)
)


# finfish traits
save_plot(
  plot_expression = {
    if (region == "MidAtlantic") {
      ecodata::plot_finfish_traits(report = region, varName = "length_maturity", n = 10)  +
             ggplot2::theme(legend.position = 'bottom')
    } else {
      ecodata::plot_finfish_traits(
        report = region,
        varName = 'fecundity',
        n = 10
      )   +
             ggplot2::ylab('Fecundity (number of \noffspring per mature female)') +
            ggplot2::theme(legend.position = 'bottom') +
           ggplot2::facet_wrap(~EPU, nrow = 2)
    }
  },
  indicator = "traits",
  width = 6.5,
  height = ifelse(region == "NewEngland", 5, 3.5)
)

#finfish traits - trophic level
save_plot(
  plot_expression = {
    if (region == "MidAtlantic") {
    ecodata::plot_finfish_traits(report = region, varName = "trophic_level", n = 10)  +
         ggplot2::theme(legend.position = 'bottom') +
         ggplot2::ylab('Trophic Level')
    } else {
      ecodata::plot_finfish_traits(report = region, varName = "trophic_level", n = 10)  +
        ggplot2::theme(legend.position = 'bottom') +
        ggplot2::ylab('Trophic Level') +
        ggplot2::facet_wrap(~EPU, nrow = 2)
    }
  },
  indicator = "traits_trophic_level",
  width = 6.5,
  height = 4.5
)

#finfish traits - growth rate
save_plot(
  plot_expression = {
    if (region == "MidAtlantic") {
    ecodata::plot_finfish_traits(report = region, varName = "k", n = 10)  +
         ggplot2::theme(legend.position = 'bottom') +
         ggplot2::ylab('Growth coefficient (k)')
    } else {
      ecodata::plot_finfish_traits(report = region, varName = "k", n = 10)  +
        ggplot2::theme(legend.position = 'bottom') +
        ggplot2::ylab('Growth coefficient (k)') +
        ggplot2::facet_wrap(~EPU, nrow = 2)
    }
  },
  indicator = "traits_growth_rate",
  width = 6.5,
  height = 4.5
)

## Community social and climate vulnerability ----

# 1. Commercial Engagement Plot
save_plot(
  plot_expression = {
    commercial_engagement_plot <- plot_engagement(
      report = region,
      varName = "Commercial"
    ) +
      ggplot2::theme(plot.title = ggplot2::element_text(vjust = 0)) 
  },
  indicator = "commercial_engagement",
  width = 7,
  height = 5
)

# 2. Recreational Engagement Plot
save_plot(
  plot_expression = {
    recreational_engagement_plot <- ecodata::plot_engagement(
      report = region,
      varName = "Recreational"
    ) +
      ggplot2::theme(plot.title = ggplot2::element_text(vjust = 0))
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
      n = 24
    ) +
        ggplot2::theme(legend.position = 'bottom')
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
                                varName = "length",
                                n = 10)
  },
  indicator = "transition_date",
  width = 6.5,
  height = ifelse(region == "NewEngland", 4, 2.5)
)


# Monthly Chlorophyll Plot
save_plot(
  plot_expression = {
    ecodata::plot_chl_pp(
      report = region,
      plottype = "monthly",
      n = 30)
  },
  indicator = "monthly_chl",
  width = 6.5,
  height = ifelse(region == "NewEngland", 7, 4)
)

### Risks to setting catch limits ----
# productivity anomaly
save_plot(
  plot_expression = {
      anomaly <- plot_productivity_anomaly(report = region, 
                                varName = "anomaly", 
                                plottype = "council") 

      assessment <- plot_productivity_anomaly(report = region, 
                                              varName = "assessment", 
                                              plottype = "council") 
      if (region == "MidAtlantic") {
          ggpubr::ggarrange(
            anomaly,
            assessment,
            ncol = 1)
      } else {
        ggpubr::ggarrange(
          anomaly,
          assessment,
          ncol = 1,
          common.legend = TRUE,
          legend = "bottom") 
      }
      },
      indicator = "productivity_anomaly",
      width = 6.5,
      height = ifelse(region == "NewEngland", 9.5, 8.5)
    )

# condition factor
save_plot(
  plot_expression = {
    if (region == "MidAtlantic") {
      ecodata::plot_condition(report = region) +
             ggplot2::theme(
              legend.text = ggplot2::element_text(size = 10),
             legend.title = ggplot2::element_text(size = 10),
           axis.text.x = ggplot2::element_text(size = 12),
          axis.text.y = ggplot2::element_text(size = 8),
          plot.title = ggplot2::element_text(size = 12),
         legend.position = "bottom",
      ) +
      ggplot2::guides(fill= ggplot2::guide_legend(nrow=2,byrow=TRUE))
    } else {
      gb <- ecodata::plot_condition(report = region, EPU = "GB") 
      
      gom <- ecodata::plot_condition(report = region, EPU = "GOM") 
    
      ggpubr::ggarrange(gb, gom, ncol = 1, common.legend = TRUE, legend = "bottom") +
              ggplot2::theme(
               legend.text = ggplot2::element_text(size = 10),
              legend.title = ggplot2::element_text(size = 11),
             axis.text.x = ggplot2::element_text(size = 12),
            axis.text.y = ggplot2::element_text(size = 12),
           plot.title = ggplot2::element_text(size = 12)
        )
    }
  },
  indicator = "condition",
  width = 6.5,
 #  height = 7
  height = ifelse(region == "NewEngland", 10, 6)
)

# 5. Energy Density Plot
save_plot(
  plot_expression = {
    plot_energy_density(report = region) +
         ggplot2::theme(legend.position = 'bottom')
  },
  indicator = "energy_density",
  width = 6.5,
  height = 4
)

# 6. Forage Index Plot
save_plot(
  plot_expression = {
    plt <- ecodata::plot_forage_index(report = region, n = 10)
    if (region == "MidAtlantic") {
      plt
    } else {
      plt + ggplot2::facet_wrap(~EPU, nrow = 2)
    }
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
    ) +
      ggplot2::theme(legend.position = "none") 
    macrobenthos_plot <- ecodata::plot_benthos_index(
      report = region,
      varName = "Macrobenthos",
      n = 10)
    ggpubr::ggarrange(
      megabenthos_plot,
      macrobenthos_plot,
      common.legend = TRUE,
      legend = "bottom",
      nrow = 2
    )
  },
  indicator = "benthos",
  width = 6.5,
  height = 6
)

# 8. Zooplankton Anomaly Plot
save_plot(
  plot_expression = {
    large_copepod_plot <- ecodata::plot_zooplankton_index(
      report = region,
      varName = "Lgcopeall",
      n = 10
    ) +
      ggplot2::ylab("Relative Biomass") +
      ggplot2::labs(title = "Large Copepods") +
      ggplot2::theme(strip.text.x = ggplot2::element_text(size = 10))
    small_copepod_plot <- ecodata::plot_zooplankton_index(
      report = region,
      varName = "Smallcopeall",
      n = 10
    ) +
      ggplot2::ylab("Relative Biomass") +
      ggplot2::labs(title = "Small Copepods") +
      ggplot2::theme(strip.text.x = ggplot2::element_text(size = 10)) 
    euphausiid_plot <- ecodata::plot_zooplankton_index(
      report = region,
      varName = "Euph",
      n = 10
    ) +
      ggplot2::theme(
        legend.background = ggplot2::element_rect(fill = "white")
      ) +
      ggplot2::ylab("Relative Biomass") +
      ggplot2::labs(title = "Euphasiids") +
      ggplot2::theme(strip.text.x = ggplot2::element_text(size = 10)) 
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

# Advection Index
save_plot(
  plot_expression = {
    ecodata::plot_advection(report = region, n = 10, varName = 6) +
      ggplot2::theme(legend.position = 'bottom')
  },
  indicator = "advection_index",
  width = 6.5,
  height = 4.5
)

# Seasonal OISST Anomaly - MAB ONLY
save_plot(
  plot_expression = {
    ecodata::plot_seasonal_oisst_anom(report = region, n = 10) 
  },
  indicator = "seasonal_oisst_anom",
  width = 6.5,
  height = 4.5
)

# Seasonal Bottom Temp Anomaly - MAB ONLY
save_plot(
  plot_expression = {
    ecodata::plot_bottom_temp_model_anom(report = region, 
                                      n =10, 
                                      varName = "seasonal", 
                                      EPU = "MAB", 
                                      plottype = "GLORYS") +
      ggplot2::theme(legend.position = 'bottom')
  },
  indicator = "bottom_temp_anom",
  width = 6.5,
  height = 4.5
)
# Other ocean uses: offshore wind ----

# 1. Wind Species Revenue Plot
save_plot(
  plot_expression = {
    ecodata::plot_wind_revenue(
      report = region,
      varName = "value",
      plottype = "nofacets",
      n = 16
    )    +
         ggplot2::theme(legend.position = "bottom") +
      if (region == "MidAtlantic") {
        ggplot2::ggtitle("Mid Atlantic: Fishery Revenue in Active Projects") 
      } else {
        ggplot2::ggtitle("New England: Fishery Revenue in Active Projects") 
      }
  },
  indicator = "wind_revenue",
  width = 6.5,
  height = 4
)


### NEW PLOT FUNCTION
### THE FILE 'all_data' is in '//nefscdata/SOE_ESP_Data/ej_indicator/2026_SOE/output'
### THE NEW PLOTTING FUNCTION 'PLOT_WIND_PORT' IS IN PLOT-UPDATES BRANCH OF ECODATA.
### R/plot_wind_port.R

save_plot(
  plot_expression = {
    plot_wind_port(report=region,
                   data = all_data) 
  },
  indicator = "wea_port_rev",
  width = 6.5,
  height = 7
)

# ## Mid plot -- NE ports landing majority Mid species
# ## currently under MidAtlantic/newengland_mafmc
# save_plot(
#   plot_expression = {
#     plot_wind_port(port_list = c("BARNSTABLE, MA",
#                       "DAVISVILLE/NORTH KINGSTOWN, RI",
#                       "EAST HAVEN, CT",
#                       "NEW LONDON, CT",
#                                  "POINT JUDITH, RI",
#                       "STONINGTON,CT",
#                                  "TIVERTON,RI"))
#   },
#   indicator = "wind_rev",
#   width = 6.5,
#   height = 4.5
# )
# 
# ## NE plot - MAB ports landing majority NE species
# ## currently under NewEngland/midatlantic_nefmc
# save_plot(
#   plot_expression = {
#     plot_wind_port(port_list = c("CAPE MAY, NJ",
#                                  "NEWPORT NEWS, VA",
#                                  "LONG BEACH (TOWN OF), NJ",
#                                  "POINT PLEASANT, NJ",
#                                  "BARNEGAT LIGHT, NJ",
#                                  "HAMPTON, VA",
#                                  "WILDWOOD, NJ",
#                                  "POINT LOOKOUT, NY",
#                                  "BRIELLE, NJ")) +
#       ggplot2::ggtitle("Port Revenue from Lease Areas, Majority NEFMC Species")
#   },
#   indicator = "wind-rev",
#   width = 6.5,
#   height = 4
# )

####### SAME PLOTS FOR BOTH REPORTS ######
# setup ----

region <- "BothReports"

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
  height = 4
)

# 5. Energy Density Plot
save_plot(
  plot_expression = {
    # plot is the same even though it takes a region parameter
    ecodata::plot_energy_density(report = "NewEngland")
  },
  indicator = "energy_density",
  width = 6.5,
  height = 4
)

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
  height = 3
)

# gray seal
save_plot(
  plot_expression = {
    ecodata::plot_grayseal()
  },
  indicator = "grayseal",
  width = 6.5,
  height = 3
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
  indicator = "narw_calves",
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
    a <- ecodata::plot_species_dist(varName = "along", n = 10) +
      ggplot2::coord_cartesian(xlim = c(1969, 2021))
    b <- ecodata::plot_species_dist(varName = "depth", n = 10) +
      ggplot2::coord_cartesian(xlim = c(1969, 2021))
    ggpubr::ggarrange(a, b, ncol = 2)
  },
  indicator = "species_dist",
  width = 6.5,
  height = 5
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
  height = 4
)

# forage shifts
save_plot(
  plot_expression = {
    ecodata::plot_forage_index(varName = "cog", n = 10) +
      ggplot2::coord_cartesian(xlim = c(1982, 2023)) 
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
    )  +
         ggplot2::coord_cartesian(xlim = c(1980, 2023)) +
        ggplot2::ggtitle("Northeast U.S. Macrobenthos Distribution") +
       ggplot2::ylab("Center of Gravity, km") +
      ggplot2::geom_point(ggplot2::aes(color = .data$Season)) + 
     ggplot2::geom_line(ggplot2::aes(color = .data$Season)) +
          ggplot2::theme(legend.position = 'bottom') +
         ggplot2::facet_grid(cols = ggplot2::vars(Season), rows = ggplot2::vars(Direction), scales = "free_y")
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
    ecodata::plot_wind_dev_speed() +
      ggplot2::theme(legend.position = 'bottom')
  },
  indicator = "wind_dev_speed",
  width = 6.5,
  height = 4
)

# slopewater
save_plot(
  plot_expression = {
    # for both reports, even though function calls NE
    ecodata::plot_slopewater(report = "NewEngland", n = 10)
  },
  indicator = "slopewater",
  width = 6,
  height = 4
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
  height = 4
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
      # ggplot2::coord_cartesian(xlim = c(1980, 2023)) +
      ggplot2::ggtitle("Northeast U.S. Megabenthos Distribution") +
      ggplot2::ylab("Center of Gravity, km") +
      ggplot2::facet_grid(
          cols = ggplot2::vars(Season),
          rows = ggplot2::vars(Direction),
          scales = "free_y") +
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
