# SELECT REGION
# region <- "NewEngland"
# region <- "MidAtlantic"

source(here::here("utils/plot_functions.R"))

###########################################
#' Run all slide plots
#'
#' This function creates all plots that are included in both the NE and MAB Slides
#'
#' @param region Region for which to create slide plots ("MidAtlantic" or "NewEngland")
create_plots_slides_mab_and_ne <- function(region) {
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
          ggplot2::facet_wrap(~EPU, nrow = 2)
      }
    },
    indicator = "total-landings",
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
      ) +
        ggplot2::geom_point(size = 0.05) +
        ggplot2::geom_line(size = 0.05)
    },
    indicator = "comm-landings",
    width = 6.5,
    height = ifelse(region == "NewEngland", 6, 5)
  )

  # climate vulnerability landings
  save_plot(
    plot_expression = {
      ecodata::plot_community_risks(
        report = region,
        plottype = "regionland",
        n = 30
      ) +
        ggplot2::ylab("Total Vulnerability \n (Regional Landings)") +
        ggplot2::theme(legend.position = 'bottom')
    },
    indicator = "climatevul-land",
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
    indicator = "rec-landings",
    width = 6.5,
    height = 2
  )

  # rec hms and sharks - MAB ONLY
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
        ggplot2::theme(
          legend.background = ggplot2::element_rect(fill = "white")
        )
      if (region == 'MidAtlantic') {
        ggpubr::ggarrange(
          rec_lps_sharks_plot,
          rec_hms_plot,
          ncol = 1,
          common.legend = TRUE,
          legend = "bottom"
        )
      } else {
        rec_lps_sharks_plot
      }
    },
    indicator = "rec-hms",
    width = 6.5,
    height = ifelse(region == 'MidAtlantic', 5, 2)
  )

  ### Implications ----

  # 1. Stock Status Plot
  save_plot(
    plot_expression = {
      stock_status_plot <- ecodata::plot_stock_status(report = region)
      p = stock_status_plot$p +
        ggplot2::scale_y_continuous(breaks = c(0, 0.5, 1, 2, 8)) +
        ggplot2::theme(legend.direction = 'horizontal')
      if (region == "MidAtlantic") {
        p
      } else {
        p
      }
    },
    indicator = "stock-status",
    width = 6.5,
    height = ifelse(region == "NewEngland", 6, 5)
  )

  # 2. ABC/ACL Stacked Plot
  save_plot(
    plot_expression = {
      ecodata::plot_abc_acl(
        report = region,
        plottype = "Stacked"
      )
    },
    indicator = "abcacl-stacked",
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
    indicator = "abcacl-catch",
    width = 6.5,
    height = 2.5
  )

  # Aggregate biomass
  if (region == "MidAtlantic") {
    save_plot(
      plot_expression = {
        custom_legend_grob <- gridtext::richtext_grob(
          paste(
            "<span style='color:black;'>NEFSC Bottom Trawl</span>",
            "<span style='color:red;'>NEAMAP Bottom Trawl</span>",
            sep = "<br>"
          ),
          halign = 0,
          gp = grid::gpar(fontsize = 10)
        )

        ecodata::plot_aggregate_biomass(report = region, EPU = "MAB", n = 10) +
          ggplot2::theme(legend.position = "bottom") +
          ggplot2::guides(
            custom_legend = ggplot2::guide_custom(
              grob = custom_legend_grob
            )
          )
      },
      indicator = "nefsc-biomass-mab",
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
      indicator = "nefsc-biomass-gb",
      width = 6,
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
      indicator = "nefsc-biomass-gom",
      width = 6,
      height = 7
    )
  }

  ## Commercial profits ----
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
      } else {
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
    indicator = "comdat-profit",
    width = 6.5,
    height = ifelse(region == "NewEngland", 5.5, 4)
  )
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
          ggplot2::facet_wrap(~EPU, nrow = 2)
      }
    },
    indicator = "comm-revenue",
    width = 6.5,
    height = ifelse(region == "NewEngland", 4.5, 4.5)
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
            legend.position = "bottom"
          )
      } else {
        gb <- ecodata::plot_bennet(
          report = region,
          varName = "total",
          EPU = "GB"
        ) +
          ggplot2::ggtitle("GB revenue components") +
          ggplot2::theme(
            legend.position = "none",
            legend.title = ggplot2::element_blank()
          ) +
          ggplot2::ylab("Million USD (2024)") +
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
          ggplot2::ylab("Million USD (2024)") +
          ggplot2::theme(text = ggplot2::element_text(size = 12))

        ggpubr::ggarrange(gb, gom, nrow = 2)
      }
    },
    indicator = "bennet",
    width = 6.5,
    height = ifelse(region == "NewEngland", 8, 6)
  )

  # bennet all
  save_plot(
    plot_expression = {
      if (region == "MidAtlantic") {
        ecodata::plot_bennet(report = region) +
          ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
            legend.position = "bottom"
          ) +
          ggplot2::facet_wrap(~Var, nrow = 2)
      } else {
        gb <- ecodata::plot_bennet(
          report = "NewEngland",
          varName = "guild",
          EPU = "GB"
        ) +
          ggplot2::ylab("Million USD (2023)") +
          ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
          )
        gom <- ecodata::plot_bennet(
          report = "NewEngland",
          varName = "guild",
          EPU = "GOM"
        ) +
          ggplot2::ylab("Million USD (2023)") +
          ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
            legend.position = "bottom"
          )
        ggpubr::ggarrange(
          gb,
          gom,
          ncol = 1,
          common.legend = TRUE,
          legend = "bottom"
        )
      }
    },
    indicator = "bennet-all",
    width = 9,
    height = ifelse(region == "NewEngland", 6.5, 9)
  )

  # 4. Climate Vulnerability Revenue Plot
  save_plot(
    plot_expression = {
      ecodata::plot_community_risks(
        report = region,
        plottype = "regionrev",
        n = 30
      ) +
        ggplot2::theme(legend.position = "bottom") +
        ggplot2::ylab("Total Vulnerability \n (Regional Revenue)")
    },
    indicator = "climatevul-rev",
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
    indicator = "rec-op",
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
        plt +
          ggplot2::ylab('Effective Shannon Index')
      }
    },
    indicator = "rec-div",
    width = 6.5,
    height = 2.5
  )

  ## Stability ----

  ### Indicators ----

  # 1. Commercial Diversity Fleet Plot
  save_plot(
    plot_expression = {
      if (region == "NewEngland") {
        a <- ecodata::plot_commercial_div(
          report = region,
          varName = "Fleet count",
          n = 22
        ) +
          ggplot2::theme(
            plot.margin = ggplot2::unit(c(0.25, 0.5, 0.25, 0.5), "cm")
          )
      } else {
        a <- ecodata::plot_commercial_div(
          report = region,
          varName = "Fleet count",
          n = 22
        ) +
          ggplot2::theme(
            plot.margin = ggplot2::unit(c(0.25, 0.5, 0.25, 0.5), "cm")
          )
        b <- ecodata::plot_commercial_div(
          report = region,
          varName = "Fleet diversity in revenue",
          n = 22
        ) +
          ggplot2::theme(
            plot.margin = ggplot2::unit(c(0.25, 0.5, 0.25, 0.5), "cm")
          )
        ggpubr::ggarrange(a, b, nrow = 2)
      }
    },
    indicator = "comm-div-fleet",
    width = 6.5,
    height = 4
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
    indicator = "commercial-div-species-div",
    width = 6.5,
    height = 2.5
  )

  # 3. Recreational Diversity Catch Plot
  save_plot(
    plot_expression = {
      ecodata::plot_recdat(report = region, varName = "catchdiversity", n = 10)
    },
    indicator = "recdat-div-catch",
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
        ) +
          ggplot2::coord_cartesian(
            ylim = c(2e+07, 4e+07),
            xlim = c(1998, 2025)
          ) +
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
          ggplot2::ggtitle('Georges Bank total Primary Production') +
          ggplot2::ylab("Carbon (mt)")
        b <- ecodata::plot_annual_chl_pp(
          report = region,
          varName = "pp",
          plottype = "total",
          EPU = "GOM",
          n = 27
        ) +
          ggplot2::ggtitle('Gulf of Maine total Primary Production') +
          ggplot2::ylab("Carbon (mt)")

        ggpubr::ggarrange(a, b, nrow = 2)
      }
    },
    indicator = "totpp",
    width = 6.5,
    height = ifelse(region == "NewEngland", 4, 2.5)
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
    indicator = "zoo-diversity",
    width = 6.5,
    height = ifelse(region == "NewEngland", 4, 2.5)
  )

  # 5. Expected N Plot
  save_plot(
    plot_expression = {
      exp_n_plot <- ecodata::plot_exp_n(
        report = region,
        varName = "fall",
        n = 10
      )
      if (region == "MidAtlantic") {
        exp_n_plot +
          ggplot2::scale_x_continuous(
            breaks = seq(1968, 2018, by = 10),
            expand = c(0.01, 0.01)
          ) +
          ggplot2::ylab("Number of species / 1000 Individuals") +
          ggplot2::theme(
            axis.title.y = ggplot2::element_text(size = 8),
            legend.position = 'bottom'
          )
      } else {
        exp_n_plot +
          ggplot2::scale_x_continuous(
            breaks = seq(1968, 2018, by = 10),
            expand = c(0.01, 0.01)
          ) +
          ggplot2::ylab("Number of species / 1000 Individuals") +
          ggplot2::theme(
            axis.title.y = ggplot2::element_text(size = 8),
            legend.position = 'bottom'
          ) +
          ggplot2::facet_wrap(~EPU, nrow = 2)
      }
    },
    indicator = "exp-n",
    width = 6.5,
    height = ifelse(region == "NewEngland", 3.5, 4)
  )

  #zooplankton community PCA
  save_plot(
    plot_expression = {
      if (region == "MidAtlantic") {
        ecodata::plot_zoo_community(report = region, n = 10) +
          ggplot2::theme(legend.position = 'bottom')
      } else {
        ecodata::plot_zoo_community(
          report = region,
          n = 10
        ) +
          ggplot2::facet_wrap(~EPU, nrow = 2)
      }
    },
    indicator = "zoo-community",
    width = 6.5,
    height = ifelse(region == "NewEngland", 5, 3.5)
  )

  # finfish traits -- original for pdfs
  save_plot(
    plot_expression = {
      if (region == "MidAtlantic") {
        ecodata::plot_finfish_traits(
          report = region,
          varName = "length_maturity",
          n = 10
        ) +
          ggplot2::theme(legend.position = 'bottom')
      } else {
        ecodata::plot_finfish_traits(
          report = region,
          varName = 'fecundity',
          n = 10
        ) +
          ggplot2::ylab('Fecundity (number of \noffspring per mature female)') +
          ggplot2::theme(legend.position = 'bottom') +
          ggplot2::facet_wrap(~EPU, nrow = 2)
      }
    },
    indicator = "traits",
    width = 6.5,
    height = ifelse(region == "NewEngland", 3.5, 4)
  )

  #finfish traits - growth rate
  save_plot(
    plot_expression = {
      if (region == "MidAtlantic") {
        ecodata::plot_finfish_traits(report = region, varName = "k", n = 10) +
          ggplot2::theme(legend.position = 'bottom') +
          ggplot2::ylab('Growth coefficient (k)')
      } else {
        ecodata::plot_finfish_traits(report = region, varName = "k", n = 10) +
          ggplot2::theme(legend.position = 'bottom') +
          ggplot2::ylab('Growth coefficient (k)') +
          ggplot2::facet_wrap(~EPU, nrow = 2)
      }
    },
    indicator = "traits-k",
    width = 6.5,
    height = ifelse(region == "NewEngland", 4.5, 4)
  )

  # finfish traits -- trophic level
  save_plot(
    plot_expression = {
      if (region == "MidAtlantic") {
        ecodata::plot_finfish_traits(
          report = region,
          varName = "trophic_level",
          n = 10
        ) +
          ggplot2::theme(legend.position = 'bottom') +
          ggplot2::ylab('Trophic Level')
      } else {
        ecodata::plot_finfish_traits(
          report = region,
          varName = "trophic_level",
          n = 10
        ) +
          ggplot2::theme(legend.position = 'bottom') +
          ggplot2::ylab('Trophic Level') +
          ggplot2::facet_wrap(~EPU, nrow = 2)
      }
    },
    indicator = "traits-tl",
    width = 6.5,
    height = ifelse(region == "NewEngland", 4.5, 4)
  )

  ## Community social and climate vulnerability ----

  # 1. Commercial Engagement Plot
  save_plot(
    plot_expression = {
      commercial_engagement_plot <- ecodata::plot_engagement(
        report = region,
        varName = "Commercial"
      ) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(vjust = 0),
          legend.title = ggplot2::element_blank()
        )
    },
    indicator = "commercial-engagement",
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
    indicator = "recreational-engagement",
    width = 6.5,
    height = 3.5
  )

  # 3. Community Climate Vulnerability Exposure Plot
  save_plot(
    plot_expression = {
      ecodata::plot_community_risks(
        report = region,
        n = 30
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
      ecodata::plot_trans_dates(report = region, varName = "length", n = 10)
    },
    indicator = "transition",
    width = 6.5,
    height = ifelse(region == "NewEngland", 5, 2.5)
  )

  # Monthly Chlorophyll Plot
  save_plot(
    plot_expression = {
      ecodata::plot_chl_pp(
        report = region,
        plottype = "monthly",
        n = 30
      )
    },
    indicator = "chl-month",
    width = 6.5,
    height = ifelse(region == "NewEngland", 5.5, 4)
  )

  ### Risks to setting catch limits ----
  # productivity + recruitment anomalies
  save_plot(
    plot_expression = {
      anomaly <- ecodata::plot_productivity_anomaly(
        report = region,
        varName = "anomaly",
        plottype = "council"
      ) +
        ggplot2::labs(
          title = paste0(region, " Productivity Anomaly from Survey Data")
        ) +
        if (region == "MidAtlantic") {
          ggplot2::labs(subtitle = "MAFMC managed species")
        } else {
          ggplot2::labs(subtitle = "NEFMC managed species")
        }

      assessment <- ecodata::plot_productivity_anomaly(
        report = region,
        varName = "assessment",
        plottype = "council"
      )
      if (region == "MidAtlantic") {
        ggpubr::ggarrange(
          anomaly,
          assessment,
          ncol = 1
        )
      } else {
        ggpubr::ggarrange(
          anomaly,
          assessment,
          ncol = 1,
          common.legend = TRUE,
          legend = "bottom"
        )
      }
    },
    indicator = "productivity-anomaly",
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
          ggplot2::guides(fill = ggplot2::guide_legend(nrow = 2, byrow = TRUE))
      } else {
        gb <- ecodata::plot_condition(report = region, EPU = "GB")

        gom <- ecodata::plot_condition(report = region, EPU = "GOM")

        ggpubr::ggarrange(
          gb,
          gom,
          ncol = 2,
          common.legend = TRUE,
          legend = "bottom"
        ) +
          ggplot2::theme(
            legend.text = ggplot2::element_text(size = 10),
            legend.title = ggplot2::element_text(size = 11),
            axis.text.x = ggplot2::element_text(size = 12),
            axis.text.y = ggplot2::element_text(size = 12),
            plot.title = ggplot2::element_text(size = 12)
          )
      }
    },
    indicator = ifelse(region == "NewEngland", "ne-cf", "mab-cf"),
    width = ifelse(region == "NewEngland", 13, 6.5),
    # width = 6.5,
    height = 7
    # height = ifelse(region == "NewEngland", 7, 6)
  )

  # # 5. Energy Density Plot
  # save_plot(
  #   plot_expression = {
  #     ecodata::plot_energy_density(report = region) +
  #       ggplot2::theme(legend.position = 'bottom')
  #   },
  #   indicator = "energy-density",
  #   width = 6.5,
  #   height = 4
  # )

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
        n = 10
      )
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
    height = ifelse(region == "NewEngland", 8, 6)
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
    indicator = "zoopanom",
    width = 6.5,
    height = 7.5
  )

  # Advection Index
  save_plot(
    plot_expression = {
      ecodata::plot_advection(report = region, n = 10, varName = 6) +
        ggplot2::theme(legend.position = 'bottom')
    },
    indicator = "advection-index",
    width = 6.5,
    height = 3
  )

  # Seasonal OISST Anomaly - MAB ONLY
  if (region == "MidAtlantic") {
    save_plot(
      plot_expression = {
        ecodata::plot_seasonal_oisst_anom(report = region, n = 10)
      },
      indicator = "seasonal-oisst-anom",
      width = 6.5,
      height = 4.5
    )
  }

  # Seasonal Bottom Temp Anomaly - MAB ONLY
  if (region == "MidAtlantic") {
    save_plot(
      plot_expression = {
        ecodata::plot_bottom_temp_model_anom(
          report = region,
          n = 10,
          varName = "seasonal",
          EPU = "MAB",
          plottype = "GLORYS"
        ) +
          ggplot2::theme(legend.position = 'bottom')
      },
      indicator = "bottom-temp-anom",
      width = 6.5,
      height = 4.5
    )
  }

  # In situ bottom temperature
  save_plot(
    plot_expression = {
      if (region == "MidAtlantic") {
        ecodata::plot_bottom_temp_insitu(report = region, n = 10)
      } else {
        ecodata::plot_bottom_temp_insitu(
          report = region,
          n = 10
        ) +
          ggplot2::facet_wrap(~EPU, nrow = 2)
      }
    },
    indicator = "bottom-temp-insitu",
    width = 6.5,
    height = ifelse(region == "NewEngland", 5, 2.5)
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
      ) +
        ggplot2::theme(legend.position = "bottom") +
        if (region == "MidAtlantic") {
          ggplot2::ggtitle("Mid Atlantic: Fishery Revenue in Active Projects")
        } else {
          ggplot2::ggtitle("New England: Fishery Revenue in Active Projects")
        }
    },
    indicator = "wea-spp-rev",
    width = 6.5,
    height = 4
  )
}
