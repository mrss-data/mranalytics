#' Add a Gift Range Column to a Data Frame TEST
#'
#' Categorizes gift amounts in a data frame into predefined ranges and adds
#' this categorization as a new column. The new column is placed immediately
#' after the specified amount column.
#'
#' @param data A data frame containing the gift amount data.
#' @param amount_column_name A character string specifying the name of the column
#'   in `data` that contains the numeric gift amounts.
#' @param breaks_config A list containing the configuration for the breaks.
#'   Must include `upper_limits` (a numeric vector) and `lower_bound` (a single
#'   numeric value). Can optionally include `label_prefixes` (a character vector).
#' @param new_column_name A character string for the name of the new column
#'   to be created (defaults to "Gift_Range").
#'
#' @return A data frame with the new gift range column added and appropriately positioned.
#' @export
#' @importFrom dplyr %>% mutate relocate
#' @importFrom rlang sym !!


add_gift_range_column <- function(data, 
                                 amount_column_name, # Expecting a character string for the column name
                                 breaks_config, 
                                 new_column_name = "Gift_Range") {

  # --- Input Validation ---
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a data frame.")
  }
  if (!is.character(amount_column_name) || length(amount_column_name) != 1) {
    stop("amount_column_name must be a single character string.")
  }
  if (!amount_column_name %in% names(data)) {
    stop(paste("Column '", amount_column_name, "' not found in data.", sep=""))
  }
  if (!is.list(breaks_config) || 
      !all(c("upper_limits", "lower_bound") %in% names(breaks_config))) {
    stop("breaks_config must be a list with 'upper_limits' and 'lower_bound'.")
  }
  if (!is.character(new_column_name) || length(new_column_name) != 1) {
    stop("new_column_name must be a single character string.")
  }

  # --- Generate Labels and Breaks ---
  dynamic_labels <- generate_gift_range_labels(
    upper_limits = breaks_config$upper_limits,
    lower_bound = breaks_config$lower_bound,
    prefixes = breaks_config$label_prefixes
  )

  dynamic_breaks <- c(breaks_config$lower_bound, breaks_config$upper_limits)

  if (length(dynamic_labels) != (length(dynamic_breaks) - 1)) {
    stop("Generated labels count does not match the number of break intervals. Check configuration.")
  }

  # --- Add the new column using dplyr::mutate and cut ---
  # Use .data[[string_column_name]] to access the column
  # Use := for dynamic assignment of new column name
  data_with_ranges <- data %>%
    mutate(
      !!new_column_name := cut( # Use !! (bang-bang) to unquote the string variable new_column_name
        .data[[amount_column_name]],
        breaks = dynamic_breaks,
        labels = dynamic_labels,
        right = FALSE,
        include.lowest = TRUE,
        ordered_result = TRUE
      )
    )

  # Move the column so it comes immediately after the Gift_Amount column for easier reading
 data_with_ranges <- data_with_ranges %>% relocate(!!sym(new_column_name), .after = !!sym(amount_column_name))
  
  return(data_with_ranges)
}