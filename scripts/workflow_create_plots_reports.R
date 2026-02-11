#' Create SOE Plots
#'
#' @description
#' Creates all SOE plots for MidAtlantic, NewEngland, and BothReports
#'
#'
#' This script runs each of the "create_plots" files
#' "create_plots_mab_ne.R" - all SOE plots that are included in both the Mid Atlantic and New England reports
#' "create_plots_ne_only.R" - SOE plots that are produced for New England only
#' "create_plots_both_reports.R" - SOE plots that are the same for both reports 
#'
#' Each function contains the setup to set the output directory, and functions 'create_filename' and 'save_plot'
#' All plots in the function will run and be saved to the appropriate directory 'images/<region>/'
#' Plots will save with the date attached. 
#' The report will automatically take the first plot alphabetically, old plots may need to be deleted by hand.
#'
#'Run the line of code for the region you want to create plots for prior to running the 'create_plots_' functions.
#' 
#' The wind port revenue plot needs to be run separately. The input data for this plot is confidential and not in ecodata.
#' The input to plot_wind_port(data = 'all_data') 'all_data' is in '//nefscdata/SOE_ESP_Data/ej_indicator/2026_SOE/output'
#' 
#' It is recommended to install the latest version of ecodata prior to running this script. 
#' @example devtools::install_github("NOAA-EDAB/ecodata", ref = "ab03f61")

region <- "NewEngland" 
region <- "MidAtlantic"
region <- "BothReports"





## Run all MAB plots
source(here::here('scripts','create_plots_mab_ne.R'))
create_plots_mab_and_ne(region = "MidAtlantic")

## Run all NE plots
create_plots_mab_and_ne(region = "NewEngland")

                                                                                                                                                                                                                           ## Run NE only plots
source(here::here('scripts','create_plots_ne_only.R'))
create_plots_ne(region = "NewEngland")

## Run plots for Both Reports
source(here::here('scripts','create_plots_both_reports.R'))
create_plots_both(region = "BothReports")

