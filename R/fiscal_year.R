#' Add Fiscal Year Column to a Data Frame (using lubridate::quarter)
#'
#' Calculates the fiscal year for a given date column using lubridate::quarter()
#' and adds it as a new column to the input data frame. The fiscal year is
#' determined by a specified start month.
#'
#' @param data A data.frame.
#' @param date_col_name A character string specifying the name of the column in
#'   `data` that contains the dates. Dates can be in Date format, POSIXct, or
#'   character strings that `lubridate::ymd()` or `lubridate::mdy()` can parse.
#' @param fy_start_month An integer representing the starting month of the
#'   fiscal year (1 for January, 12 for December).
#' @param new_col_name A character string for the name of the new fiscal year
#'   column to be created. Defaults to "FiscalYear".
#' @param fy_format A character string specifying the format of the output fiscal
#'   year. Default is "FYYY" (e.g., "FY25"). Other options:
#'   "YYYY" (e.g., "2025" for the year the FY *ends* in),
#'   "FYYYYY" (e.g., "FY2025").
#'
#' @return The input `data.frame` with the new fiscal year column added.
#'
#' @importFrom lubridate quarter ymd mdy
#' @importFrom dplyr mutate
#' @importFrom rlang sym !! :=
#' @export
#' @examples
#' # Sample Data
#' transactions <- data.frame(
#'   TransactionID = 1:5,
#'   TransactionDate = as.Date(c("2023-06-15", "2023-07-01", "2023-12-31",
#'                               "2024-01-01", "2024-08-14")),
#'   Amount = c(100, 150, 50, 200, 75),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Example 1: FY starts July 1st (common)
#' transactions_fy_jul <- fiscal_year(
#'   data = transactions,
#'   date_col_name = "TransactionDate",
#'   fy_start_month = 7
#' )
#' print(transactions_fy_jul)
#' # Expected: 2023-06-15 -> FY23, 2023-07-01 -> FY24, 2024-08-14 -> FY25
#'
#' # Example 2: FY starts October 1st, custom column name, different format
#' transactions_fy_oct <- fiscal_year(
#'   data = transactions,
#'   date_col_name = "TransactionDate",
#'   fy_start_month = 10,
#'   new_col_name = "Fiscal_Period",
#'   fy_format = "YYYY"
#' )
#' print(transactions_fy_oct)
#' # Expected: 2023-06-15 -> 2023, 2023-07-01 -> 2023, 2023-12-31 -> 2024
#'
#' # Example 3: Character dates (lubridate will try to parse)
#' transactions_char_dates <- data.frame(
#'   SaleDate = c("01/15/2023", "08/20/2023", "12/05/2023"), # mdy format
#'   stringsAsFactors = FALSE
#' )
#' transactions_fy_char <- fiscal_year(
#'   data = transactions_char_dates,
#'   date_col_name = "SaleDate",
#'   fy_start_month = 7
#' )
#' print(transactions_fy_char)
#'
fiscal_year <- function(data,
                            date_col_name,
                            fy_start_month,
                            new_col_name = "Fiscal_Year",
                            fy_format = "FYYY") {

  # --- Input Validations ---
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a data.frame.")
  }
  if (!is.character(date_col_name) || length(date_col_name) != 1) {
    stop("'date_col_name' must be a single character string.")
  }
  if (!(date_col_name %in% names(data))) {
    stop(paste0("Date column '", date_col_name, "' not found in data."))
  }
  if (!is.numeric(fy_start_month) || fy_start_month < 1 || fy_start_month > 12) {
    stop("'fy_start_month' must be an integer between 1 and 12.")
  }
  fy_start_month <- as.integer(fy_start_month)
  if (!is.character(new_col_name) || length(new_col_name) != 1) {
    stop("'new_col_name' must be a single character string.")
  }
  if (!fy_format %in% c("FYYY", "YYYY", "FYYYYY")) {
      stop("'fy_format' must be one of 'FYYY', 'YYYY', or 'FYYYYY'.")
  }

  # --- Date Parsing and FY Calculation ---
  date_col_name_sym <- rlang::sym(date_col_name)
  new_col_name_sym <- rlang::sym(new_col_name)

  data_with_fy <- dplyr::mutate(
    data,
    # Attempt to parse dates if they are character
    .temp_parsed_date = if (is.character(.data[[date_col_name_sym]])) {
      parsed_date <- suppressWarnings(lubridate::ymd(.data[[date_col_name_sym]], quiet = TRUE))
      if (all(is.na(parsed_date))) {
        parsed_date <- suppressWarnings(lubridate::mdy(.data[[date_col_name_sym]], quiet = TRUE))
      }
      if (all(is.na(parsed_date))) {
          warning(paste0("Could not parse all character dates in column '", date_col_name,
                       "'. Ensure they are in a recognizable format. NA will be produced."))
      }
      parsed_date
    } else if (inherits(.data[[date_col_name_sym]], "Date") || inherits(.data[[date_col_name_sym]], "POSIXt")) {
      .data[[date_col_name_sym]]
    } else {
      stop(paste0("Date column '", date_col_name, "' is not a character, Date, or POSIXt object."))
    },

    # Calculate the year the FY ends in using lubridate::quarter
    .fy_end_year_str = dplyr::if_else(
        !is.na(.temp_parsed_date),
        # Extract the year part from the quarter(with_year=TRUE) output
        substr(
            as.character(lubridate::quarter(.temp_parsed_date, with_year = TRUE, fiscal_start = fy_start_month)),
            1,
            4
        ),
        NA_character_ # Keep NA if date was NA
    ),

    # Format the Fiscal Year string
    !!new_col_name_sym := dplyr::case_when(
        is.na(.fy_end_year_str) ~ NA_character_,
        fy_format == "FYYY"   ~ paste0("FY", substr(.fy_end_year_str, 3, 4)),
        fy_format == "YYYY"   ~ .fy_end_year_str,
        fy_format == "FYYYYY"  ~ paste0("FY", .fy_end_year_str),
        TRUE ~ NA_character_
    ),
    # Remove temporary columns
    .temp_parsed_date = NULL,
    .fy_end_year_str = NULL
  )

  return(data_with_fy)
}