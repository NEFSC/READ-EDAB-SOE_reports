#' Analyze Time Series Objects for Terminal Year and Missing Years
#'
#' This function iterates through a list of time series object names,
#' extracts the years, identifies the terminal year, and finds any
#' missing years (gaps) in the sequence.
#'
#' @param ts_names A character vector of the names of the time series
#'        objects in the current environment to analyze.
#' @param envir The environment where the objects are located (defaults
#'        to the global environment).
#' @return A data frame containing the name, terminal year, and a
#'         string of all missing years for each series.
analyze_time_series <- function(ts_names) {
  # A helper function to process a single time series object
  process_single_ts <- function(ts_name) {
    # Safely get the object by its name
    ts_object <- tryCatch(
      {
        e <- new.env()
        name <- data(list = ts_name, package = "ecodata", envir = e)[1]
        get(name, envir = e)
      },
      error = function(e) {
        warning(paste(
          "Object",
          ts_name,
          "not found or inaccessible:",
          e$message
        ))
        return(NULL)
      }
    )

    if (is.null(ts_object)) {
      return(data.frame(
        Series = ts_name,
        Terminal_Year = NA,
        Missing_Years = "Error/Not Found"
      ))
    }

    # 1. Extract years: Convert the index (if it exists) to a numeric year
    years <- as.numeric(ts_object$Time)

    # Ensure years are unique and sorted
    unique_years <- sort(unique(years))

    if (length(unique_years) == 0) {
      return(data.frame(
        Series = ts_name,
        Terminal_Year = NA,
        Missing_Years = "No years found"
      ))
    }

    # 2. Determine Terminal Year
    terminal_year <- max(unique_years)

    # 3. Find Missing Years
    # Create a full sequence of years from the start year to the end year
    expected_years <- seq(min(unique_years), max(unique_years), by = 1)

    # Identify the years in the expected sequence that are NOT in the unique_years
    missing_years <- setdiff(expected_years, unique_years)

    # Format the missing years for output
    if (length(missing_years) == 0) {
      missing_years_str <- "None"
    } else {
      missing_years_str <- paste(missing_years, collapse = ", ")
    }

    # 4. Compile Results
    return(data.frame(
      Series = ts_name,
      Terminal_Year = terminal_year,
      Missing_Years = missing_years_str,
      stringsAsFactors = FALSE
    ))
  }

  # Apply the processing function to all names and combine the results
  results_list <- lapply(ts_names, process_single_ts)

  # Combine the list of data frames into one result data frame
  final_results <- do.call(rbind, results_list)

  return(final_results)
}

analyze_time_series("aggregate_biomass")

objs <- data(package = "ecodata")$results[, "Item"]
ts_analysis_results <- analyze_time_series(objs)
