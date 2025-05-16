#' Add Gift Range Column to a Data Frame
#'
#' Parses a gift amount column, categorizes amounts into specified gift ranges,
#' and adds this as a new column to the data frame.
#'
#' @param data A data.frame.
#' @param amount_col_name A character string specifying the name of the column
#'   in `data` that contains the gift amounts. Amounts can be numeric or
#'   character (e.g., "$100.00", "1,250.50").
#' @param new_col_name A character string for the name of the new gift range
#'   column to be created. Defaults to "GiftRange".
#' @param breakpoints A numeric vector specifying the lower bounds of each gift
#'   range (e.g., `c(50, 100, 250, 1000)`). If 0 is not the first element,
#'   it will be automatically prepended.
#' @param prefix_style A character string specifying the prefix style for the
#'   range labels. Options are "letters" (A, B, C...), "numbers" (1, 2, 3...),
#'   or "none". Defaults to "letters".
#'
#' @return The input `data.frame` with the new gift range column added.
#'   Non-numeric or negative amounts will result in `NA` in the gift range column.
#'
#' @importFrom dplyr mutate case_when
#' @importFrom stringr str_replace_all str_trim
#' @importFrom scales dollar_format
#' @importFrom rlang sym !! :=
#' @export
#' @examples
#' # Sample Data
#' set.seed(123)
#' transaction_data <- data.frame(
#'   TransactionID = 1:10,
#'   GiftAmountString = c("$50.00", "25.00", "$1,250.75", "75", "$999.99",
#'                        "-$10.00", "Invalid", "$49.99", "$1000", "5000"),
#'   GiftAmountNumeric = c(50, 25, 1250.75, 75, 999.99, -10, NA, 49.99, 1000, 5000),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Example 1: Using string amounts, default letter prefix
#' df1 <- add_gift_range(
#'   data = transaction_data,
#'   amount_col_name = "GiftAmountString",
#'   breakpoints = c(50, 100, 1000)
#' )
#' print(df1[, c("GiftAmountString", "GiftRange")])
#' # Expected Ranges for c(0, 50, 100, 1000, Inf):
#' # A: $0 - $49.99
#' # B: $50 - $99.99
#' # C: $100 - $999.99
#' # D: $1,000 and up
#'
#' # Example 2: Using numeric amounts, number prefix, custom column name
#' df2 <- add_gift_range(
#'   data = transaction_data,
#'   amount_col_name = "GiftAmountNumeric",
#'   new_col_name = "AmountCategory",
#'   breakpoints = c(0, 25, 100, 500, 2000),
#'   prefix_style = "numbers"
#' )
#' print(df2[, c("GiftAmountNumeric", "AmountCategory")])
#'
#' # Example 3: No prefix
#' df3 <- add_gift_range(
#'   data = transaction_data,
#'   amount_col_name = "GiftAmountNumeric",
#'   breakpoints = c(100, 500),
#'   prefix_style = "none"
#' )
#' print(df3[, c("GiftAmountNumeric", "GiftRange")])
#'
gift_range <- function(data,
                           amount_col_name,
                           breakpoints,
                           new_col_name = "Gift_Range",
                           prefix_style = "letters") {

  # --- Input Validations ---
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a data.frame.")
  }
  if (!is.character(amount_col_name) || length(amount_col_name) != 1) {
    stop("'amount_col_name' must be a single character string.")
  }
  if (!(amount_col_name %in% names(data))) {
    stop(paste0("Amount column '", amount_col_name, "' not found in data."))
  }
  if (!is.numeric(breakpoints) || length(breakpoints) < 1) {
    stop("'breakpoints' must be a numeric vector with at least one value.")
  }
  if (any(breakpoints < 0)) {
      stop("'breakpoints' cannot contain negative values.")
  }
  if (!is.character(new_col_name) || length(new_col_name) != 1) {
    stop("'new_col_name' must be a single character string.")
  }
  if (!prefix_style %in% c("letters", "numbers", "none")) {
    stop("'prefix_style' must be one of 'letters', 'numbers', or 'none'.")
  }

  # --- Amount Parsing and Cleaning ---
  amount_col_sym <- rlang::sym(amount_col_name)
  new_col_name_sym <- rlang::sym(new_col_name)

  # Create a temporary column for cleaned numeric amounts
  data <- dplyr::mutate(
    data,
    .temp_numeric_amount = {
      raw_amounts <- .data[[amount_col_sym]]
      # If already numeric, use as is. Otherwise, try to clean and convert.
      if (is.numeric(raw_amounts)) {
        cleaned_amounts <- raw_amounts
      } else {
        # Remove $ and , then trim whitespace
        cleaned_strings <- stringr::str_trim(stringr::str_replace_all(as.character(raw_amounts), "[\\$,]", ""))
        # Convert to numeric, NAs will be introduced for non-numeric strings
        cleaned_amounts <- suppressWarnings(as.numeric(cleaned_strings))
      }
      # Set negative amounts to NA so they don't get a range
      dplyr::if_else(cleaned_amounts < 0, NA_real_, cleaned_amounts)
    }
  )

  # --- Breakpoint Preparation ---
  # Sort and ensure unique breakpoints
  processed_breakpoints <- sort(unique(as.numeric(breakpoints)))

  # If 0 is not the first breakpoint and the smallest breakpoint is > 0, prepend 0
  if (processed_breakpoints[1] != 0) {
    processed_breakpoints <- c(0, processed_breakpoints)
  }
  # Add Infinity for the uppermost category
  final_breakpoints <- c(processed_breakpoints, Inf)
  num_ranges <- length(final_breakpoints) - 1

  # --- Label Generation ---
  range_labels <- character(num_ranges)
  for (i in 1:num_ranges) {
    lower <- final_breakpoints[i]
    upper <- final_breakpoints[i+1]

    # Format lower bound (no cents)
    # scales::dollar_format includes cents by default, need to adjust or use sprintf
    # Using sprintf for more control here for $0 vs $0.00
    # lower_str <- if (lower == 0) "$0" else sprintf("$%.0f", floor(lower))
    lower_str <- scales::dollar(lower, accuracy = 1, prefix = "$")

    if (is.infinite(upper)) {
      label_text <- paste0(lower_str, " and up")
    } else {
      # Upper bound with cents, showing XX.99 for the one below
      # The actual cut point is 'upper', so label should reflect up to 'upper - 0.01'
      upper_display <- upper - 0.01
      # Using scales::dollar for consistent currency formatting
      # upper_str <- scales::dollar(upper_display, accuracy = 0.01)
      upper_str <- scales::dollar(upper_display, accuracy = 0.01, prefix = "$")
      label_text <- paste0(lower_str, " - ", upper_str)
    }
    range_labels[i] <- label_text
  }

  # Add prefixes
  if (prefix_style == "letters") {
    if (num_ranges > 26) {
        warning("More than 26 ranges; letter prefixes will recycle or need adjustment. Using L1, L2,... for now.")
        prefixes <- paste0("L", 1:num_ranges, ") ")
    } else if (num_ranges > 0) {
        prefixes <- paste0(LETTERS[1:num_ranges], ") ")
    } else {
        prefixes <- ""
    }
    range_labels <- paste0(prefixes, range_labels)
  } else if (prefix_style == "numbers" && num_ranges > 0) {
    range_labels <- paste0(1:num_ranges, ") ", range_labels)
  }

  # --- Apply cut() and Add New Column ---
  data <- dplyr::mutate(
    data,
    !!new_col_name_sym := dplyr::case_when(
      is.na(.temp_numeric_amount) ~ NA_character_, # Handle NAs from parsing or negative
      TRUE ~ as.character(cut(
        .temp_numeric_amount,
        breaks = final_breakpoints,
        labels = range_labels,
        right = FALSE,        # Intervals are [lower, upper) -> value on breakpoint goes to higher range
        include.lowest = TRUE # Ensures the first interval starting at 0 is [0, next_break)
      ))
    ),
    .temp_numeric_amount = NULL # Remove temporary column
  )

  return(data)
}