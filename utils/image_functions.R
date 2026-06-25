#' Create a Standardized Filename
#'
#' Constructs a standardized file path by combining an indicator name, a geographical region,
#' the current system date, and a file extension.
#'
#' @param indicator A character string specifying the metric or indicator name.
#' @param file_region A character string specifying the geographical region or strata.
#' @param dir A character string specifying the target directory path. Defaults to
#'   the variable \code{out_dir} evaluated from the calling environment.
#' @param extension A character string specifying the file extension (including the period).
#'   Defaults to \code{".png"}.
#'
#' @return A character string containing the concatenated file path structured as:
#'   \code{dir/indicator_file_region_YYYY-MM-DD.extension}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Uses out_dir from environment
#' out_dir <- "output/plots"
#' create_filename("benthic_biomass", "georges_bank")
#'
#' # Overriding defaults
#' create_filename("sst", "mid_atlantic", dir = "data/raw", extension = ".csv")
#' }
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

#' Add Ecodata Source Attribution Caption to ggplot
#'
#' Appends a standardized data provenance caption to an existing \code{ggplot2} object.
#' The caption dynamically extracts tracking metadata (branch name, abbreviated commit SHA,
#' and packaging date) directly from the installed \code{ecodata} package description.
#'
#' @param p A \code{ggplot2} plot object to be annotated.
#' @param plt_indicator A character string matching a \code{chunkName} key within the reference CSV file.
#' @param plt_key A character string mapping the file path to a figure caption summary CSV file.
#'   Defaults to a file named \code{"figure_captions_summary.csv"} located inside a \code{utils/}
#'   directory relative to the project root (via \code{here::here()}).
#'
#' @return An updated \code{ggplot2} plot object incorporating the wrapped, small-font source caption.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(mpg, wt)) + geom_point()
#' # Appends ecodata package provenance info based on matching chunk ID
#' add_ecodata_name(p, plt_indicator = "sst_anomaly_chunk")
#' }

add_ecodata_name <- function(
  p,
  plt_indicator,
  plt_key = here::here("utils/figure_captions_summary.csv")
) {
  key <- read.csv(plt_key)
  name <- key$ecodata_object[key$chunkName == plt_indicator]
  p <- p +
    ggplot2::labs(
      caption = paste0(
        "Source: ecodata::",
        name,
        " (",
        packageDescription("ecodata")$RemoteRef,
        " branch, commit ",
        packageDescription("ecodata")$RemoteSha |> substr(1, 7),
        ", packaged ",
        packageDescription("ecodata")$Packaged |> substr(1, 10),
        ")"
      ) |>
        stringr::str_wrap(width = 50)
    ) +
    ggplot2::theme(plot.caption = ggplot2::element_text(size = 8))

  return(p)
}

#' Extract Data from a ggplot Point Layer
#'
#' Iterates through all geometric layers of a built or unbuilt \code{ggplot2} object
#' to identify and return the data matrix driving a scatter or point geometry layer
#' (such as \code{geom_point}), using the structural presence of the \code{shape}
#' column as the layer identifier.
#'
#' @details
#' The function relies on \code{\link[ggplot2]{layer_data}} to extract evaluated
#' aesthetics. If a plot contains multiple point layers, this function will overwrite
#' the \code{plt_data} object sequentially and return data exclusively from the
#' \emph{last} identified point layer. If no layers contain a \code{shape} column,
#' the function will trigger an error due to an unassigned return variable.
#'
#' @param p A \code{ggplot2} plot object containing at least one point layer.
#'
#' @return A data frame containing the evaluated aesthetic mapping data (e.g.,
#'   \code{x}, \code{y}, \code{shape}, \code{colour}, \code{size}) corresponding
#'   to the isolated point layer.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' # Construct a multi-layered plot
#' p <- ggplot(mtcars, aes(mpg, wt)) +
#'   geom_line() +
#'   geom_point(shape = 21, fill = "blue")
#'
#' # Extract structural coordinate matrix for the geom_point layer
#' pt_data <- return_point_data(p)
#' head(pt_data)
#' }

return_point_data <- function(p) {
  n_layers <- length(p$layers)

  for (i in 1:n_layers) {
    dat <- ggplot2::layer_data(p, i)
    # print(head(dat))
    # shape is only for points, so this will grab data from the point layer
    if ("shape" %in% names(dat)) {
      plt_data <- dat
    }
  }

  return(plt_data)
}

#' Extract Data with Facet and Color Group Mapping
#'
#' Evaluates a built plot layer and conditionally appends literal labeling variables
#' by checking for the presence of multiple structural groupings (facets via \code{PANEL}
#' and mapping color hex-codes back to categorical scale breaks).
#'
#' @details
#' This function dynamically handles data frames from plots that are simultaneously
#' faceted and colored. It automatically filters out unneeded structural aesthetics like
#' internal hex hashes (\code{colour}) and index indicators (\code{PANEL}) before returning.
#'
#' \strong{Warning regarding natural joins:} The function utilizes \code{\link[dplyr]{left_join}}
#' without an explicit \code{by} specification when binding \code{panel_key} and \code{color_key}
#' back to the baseline data frame. This defaults to a natural join matching on common columns
#' (\code{x} and \code{y}), which assumes coordinate pairings are unique identifiers across groups.
#'
#' @param p A \code{ggplot2} plot object.
#' @param p_dat A data frame representing a built plot layer (typically extracted from
#'   \code{ggplot2::ggplot_build(p)$data[[i]]}) containing coordinates and group variables.
#'
#' @return A data frame or tibble matching the dimensions of \code{p_dat} with raw
#'   coordinates (\code{x}, \code{y}) combined with any active layout labels and scale variables
#'   (\code{Var}).
#'
#' @importFrom dplyr select left_join
#' @importFrom tibble tibble
#' @importFrom ggplot2 ggplot_build
#' @export
#'
#' @seealso
#' \code{\link{return_faceted_plt_data}}, \code{\link{return_color_plt_data}}
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' # Complex plot with both facets and color aesthetics
#' p <- ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
#'   geom_point() +
#'   facet_wrap(~Species)
#'
#' layer_dat <- ggplot_build(p)$data[[1]]
#'
#' # Extract clean dataset with layout and color categories decoded
#' return_grouped_data(p, layer_dat)
#' }

return_grouped_data <- function(p, p_dat) {
  output <- p_dat |>
    dplyr::select(x, y)

  if ("PANEL" %in% colnames(p_dat)) {
    plt_data <- p_dat |>
      dplyr::select(x, y, PANEL)

    # get facet data to join
    metadata <- ggplot2::ggplot_build(p)
    facet_labels <- metadata$layout$layout |>
      dplyr::select(-c(ROW, COL, SCALE_X, SCALE_Y, COORD))

    panel_key <- dplyr::left_join(plt_data, facet_labels, by = "PANEL") |>
      dplyr::select(-PANEL)

    output <- dplyr::left_join(output, panel_key)
  }

  if (length(unique(p_dat$colour)) > 1) {
    plt_data <- p_dat |>
      # might need to keep more of the columns if the plot is faceted?
      dplyr::select(x, y, colour)

    # get color data to join
    metadata <- ggplot2::ggplot_build(p)
    color_scale <- metadata$plot$scales$get_scales("colour")

    # Map the labels to their hex colors
    color_key <- dplyr::left_join(
      plt_data,
      tibble::tibble(
        colour = color_scale$map(color_scale$get_breaks()),
        Var = color_scale$get_labels()
      )
    )

    output <- dplyr::left_join(output, color_key) |>
      dplyr::select(-colour)
  }

  return(output)
}

#' Map Faceted Plot Points to Layout Metadata
#'
#' Extracts built layer data from a ggplot object and joins it back to the
#' plot's structural layout matrix to explicitly decode which panel corresponds
#' to which facet variables.
#'
#' @param p A \code{ggplot2} plot object containing facet layers (e.g., \code{facet_wrap}
#'   or \code{facet_grid}).
#' @param p_dat A data frame representing a built plot layer (typically extracted from
#'   \code{ggplot2::ggplot_build(p)$data[[i]]}) containing columns \code{x}, \code{y}, and \code{PANEL}.
#'
#' @return A data frame/tibble containing the raw coordinates (\code{x} and \code{y})
#'   bound to their explicit facet mapping variables, with the internal \code{PANEL} column removed.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' p <- ggplot(mpg, aes(displ, hwy)) + geom_point() + facet_wrap(~class)
#' built_p <- ggplot_build(p)
#'
#' # Extract layer 1 data and map panels to 'class' labels
#' facet_data <- return_faceted_plt_data(p, built_p$data[[1]])
#' }

#' Evaluate, Save, and Log Plot Trend Metrics
#'
#' Evaluates a plot expression, annotates it with data source information, exports the
#' image to disk, and conditionally decomposes the plot's point layers to calculate
#' and append descriptive summary metrics and trend indicators to a shared log file.
#'
#' @details
#' The function uses conditional logic based on the structural properties of the parsed
#' plot (such as facet definitions via \code{FacetNull} or unique aesthetic hexadecimal values)
#' to isolate stratification layers. It relies heavily on parent environment variables if
#' default parameters are left unassigned.
#'
#' @param plot_expression An unevaluated R expression or language object that returns a
#'   \code{ggplot} or \code{ggarrange} object when evaluated (e.g., \code{quote(my_plot_function())}).
#' @param indicator A character string defining the target metric code name. Used both for
#'   file naming and log tracking.
#' @param report A character string specifying the geographic region or report strata.
#'   Defaults to the variable \code{region} looked up in the parent calling environment.
#' @param save_dir A character string tracking the plot storage path. Defaults to the variable
#'   \code{out_dir} looked up in the parent calling environment.
#' @param output_summary A logical value indicating whether the internal data matrix should
#'   be reverse-engineered to yield statistical and trend records. Defaults to \code{TRUE}.
#' @param summary_file A character string defining the storage path for the aggregated project
#'   statistics summary spreadsheet. Defaults to \code{"figure_stats_summaries.csv"} inside
#'   \code{utils/} via \code{here::here()}.
#' @param key A character string mapping the location of the figure captions indexing CSV.
#'   Defaults to \code{"figure_captions_summary.csv"} inside \code{utils/} via \code{here::here()}.
#' @param ... Additional keyword arguments passed directly to \code{\link[ggplot2]{ggsave}}
#'   (such as \code{width}, \code{height}, or \code{dpi}).
#'
#' @return No return value. The function acts entirely through structural side effects:
#'   writing an image file to disk and appending tabular summary logs to an external file.
#'
#' @export
#'
#' @seealso
#' \code{\link{create_filename}}, \code{\link{add_ecodata_name}},
#' \code{\link{return_point_data}}, \code{\link{summary_stats}}, \code{\link{trend_summaries}}
#'
#' @examples
#' \dontrun{
#' # Assign necessary environment tokens assumed by function defaults
#' region <- "mid_atlantic"
#' out_dir <- "./outputs"
#'
#' # Define an expression generating a point plot
#' expr <- quote({
#'   ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) + ggplot2::geom_point()
#' })
#'
#' # Run pipeline
#' save_plot(
#'   plot_expression = expr,
#'   indicator = "vehicle_weight_metrics",
#'   output_summary = TRUE
#' )
#' }

save_plot <- function(
  plot_expression,
  indicator,
  report = region,
  save_dir = out_dir,
  output_summary = TRUE,
  summary_file = here::here("utils/figure_stats_summaries.csv"),
  key = here::here("utils/figure_captions_summary.csv"),
  ...
) {
  # Execute the code to create the plot
  p <- eval(plot_expression) |>
    add_ecodata_name(
      plt_indicator = indicator,
      plt_key = key
    )

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

    # extract data for summary table ----
    message("Extracting plot data...")
    if (output_summary) {
      target_plot <- if (inherits(p, "ggarrange")) p$plots[[1]] else p

      raw_dat <- return_point_data(target_plot)
      # print(head(raw_dat))

      dat <- return_grouped_data(target_plot, p_dat = raw_dat)

      message("Running summary functions...")

      # 5. Group by our new literal labels and execute summary functions
      group <- colnames(dat)[-which(colnames(dat) %in% c("x", "y"))]
      # message(group)

      if (length(group) == 0) {
        summary_rows <- dplyr::bind_cols(
          dat |> summary_stats(),
          dat |> trend_summaries()
        ) |>
          dplyr::mutate(Group1 = "Unit", Group2 = NA, Group3 = NA, .before = 1)
      } else {
        summary_rows <- dat |>
          dplyr::group_by(dplyr::across(dplyr::all_of(group))) |>
          dplyr::reframe(
            stats = summary_stats(dplyr::pick(dplyr::everything())),
            trends = trend_summaries(dplyr::pick(dplyr::everything()))
          )
      }

      # pad out group columns if needed
      if (length(group) == 1) {
        summary_rows <- summary_rows |>
          dplyr::mutate(Group2 = NA, Group3 = NA, .after = 1)
      }

      if (length(group) == 2) {
        summary_rows <- summary_rows |>
          dplyr::mutate(Group3 = NA, .after = 1)
      }

      output <- summary_rows |>
        dplyr::mutate(
          Indicator = indicator,
          Region = report,
          File_Generated = file.path(fname),
          .before = 1
        )

      # 6. Append to the shared summary CSV file

      summary_path <- file.path(summary_file)
      append_mode <- file.exists(summary_path)

      write.table(
        output,
        file = summary_path,
        append = append_mode,
        sep = ",",
        row.names = FALSE,
        col.names = !append_mode
      )

      message(
        "Grouped summary metrics appended to: ",
        summary_path
      )
    }
  } else {
    stop("Plot object is not a valid ggplot or ggarrange object.")
  }
}
