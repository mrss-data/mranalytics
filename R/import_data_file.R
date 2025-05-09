# In file: mranalytics/R/import_data_file.R

#' Import Data from Various File Types
#'
#' This function detects the file type based on its extension (csv, txt, xlsx, xls)
#' and imports the data into an R data frame. It handles common delimited
#' text files and Excel spreadsheets.
#'
#' @param file_path A character string specifying the full path to the file.
#' @param ... Additional arguments to be passed to the respective reading functions
#'   (e.g., `read.csv`, `read.delim`, `readxl::read_excel`). For example,
#'   `sep = ";"` for a semicolon-separated txt file, or `sheet = "Sheet2"`
#'   for Excel files.
#'
#' @return A data frame containing the imported data.
#'   If the file type is unsupported or the file is not found, an error is thrown.
#'
#' @details
#' Supported file extensions (case-insensitive):
#' \itemize{
#'   \item \code{.csv}: Imported using \code{utils::read.csv()}.
#'   \item \code{.txt}: Imported using \code{utils::read.delim()}. Assumes tab-separated
#'     by default. Use `...` to pass arguments like `sep` for other delimiters.
#'   \item \code{.xlsx}: Imported using \code{readxl::read_excel()}.
#'   \item \code{.xls}: Imported using \code{readxl::read_excel()}.
#' }
#'
#' @export
#' @importFrom readxl read_excel

import_data_file <- function(file_path, ...) { # Added ... for pass-through arguments
  # 1. Check if the file exists
  if (!file.exists(file_path)) {
    stop("File not found: ", file_path)
  }

  # 2. Extract the file extension
  file_extension <- tolower(tools::file_ext(file_path))

  # 3. Initialize data_frame
  data_frame <- NULL

  # 4. Import based on extension
  if (file_extension == "csv") {
    message("Detected CSV file. Importing with utils::read.csv()...")
    data_frame <- utils::read.csv(file_path, stringsAsFactors = FALSE, ...) # Pass ...
  } else if (file_extension == "txt") {
    message("Detected TXT file. Importing with utils::read.delim()...")
    data_frame <- utils::read.delim(file_path, stringsAsFactors = FALSE, ...) # Pass ...
  } else if (file_extension %in% c("xlsx", "xls")) {
    message("Detected Excel file (.", file_extension, "). Importing with readxl::read_excel()...")
    data_frame <- readxl::read_excel(path = file_path, ...) # Pass ...
  } else {
    stop("Unsupported file type: .", file_extension, ". Please provide a CSV, TXT, XLSX, or XLS file.")
  }

  if (!is.null(data_frame)) {
    message("Import successful. The table has ", nrow(data_frame), " rows and ", ncol(data_frame), " columns.")
  } else {
    # This case should ideally not be reached if an error is thrown for unsupported types
    # or if reading functions themselves error out on failure.
    warning("Import might have failed or returned NULL for: ", file_path)
  }
  return(data_frame)
}
