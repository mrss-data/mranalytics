#' Clean Email Addresses in a Data Frame
#'
#' This function cleans a specified column of email addresses within a data frame.
#' It performs several cleaning steps:
#' \itemize{
#'   \item Converts emails to lowercase.
#'   \item Removes leading and trailing whitespace.
#'   \item Removes double quotation marks (`"`).
#'   \item Removes single quotation marks (`'`).
#'   \item Removes all space characters (` `).
#'   \item Removes comma characters (`,`).
#'   \item Filters out rows where the cleaned email address does not contain an "@" symbol.
#'   \item Removes duplicate rows from the entire data frame based on all columns after cleaning.
#' }
#'
#' @param data A data frame containing the email addresses.
#' @param email_col_name A character string specifying the name of the column
#'   in `data` that contains the email addresses.
#'
#' @return A data frame with the specified email column cleaned, invalid emails
#'   (those without "@") removed, and duplicate rows removed.
#'   If the specified column is not found or is not character/factor,
#'   an error will be thrown.
#'
#' @export

clean_emails <- function(data, email_col_name) {

  # --- Input Validation ---
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a data frame.")
  }
  if (!is.character(email_col_name) || length(email_col_name) != 1) {
    stop("'email_col_name' must be a single character string.")
  }
  if (!(email_col_name %in% names(data))) {
    stop(paste0("Column '", email_col_name, "' not found in the data frame."))
  }
  if (!is.character(data[[email_col_name]]) && !is.factor(data[[email_col_name]])) {
    stop(paste0("Column '", email_col_name, "' must be of type character or factor."))
  }

  # --- Data Cleaning ---

  # Work on a copy to avoid modifying the original data frame by reference if it's a data.table
  # For data.frames, assignment creates a copy anyway, but this is safer practice.
  df_cleaned <- data[, email_col_name, drop = FALSE]

  # Ensure the column is character (factors need conversion)
  if (is.factor(df_cleaned[[email_col_name]])) {
    df_cleaned[[email_col_name]] <- as.character(df_cleaned[[email_col_name]])
  }

  # 1. Convert to lowercase
  df_cleaned[[email_col_name]] <- tolower(df_cleaned[[email_col_name]])

  # 2. Remove leading/trailing whitespace (good practice before specific character removal)
  df_cleaned[[email_col_name]] <- trimws(df_cleaned[[email_col_name]])

  # 3. Remove specific characters
  # Using fixed = TRUE for literal matching, which is often safer and faster for these characters.
  df_cleaned[[email_col_name]] <- gsub('"',  "", df_cleaned[[email_col_name]], fixed = TRUE) # double quotes
  df_cleaned[[email_col_name]] <- gsub("'",  "", df_cleaned[[email_col_name]], fixed = TRUE) # single quotes
  df_cleaned[[email_col_name]] <- gsub(" ",  "", df_cleaned[[email_col_name]], fixed = TRUE) # all spaces
  df_cleaned[[email_col_name]] <- gsub(",",  "", df_cleaned[[email_col_name]], fixed = TRUE) # commas

  # 4. Filter out rows where the email does not contain "@"
  # Using fixed = TRUE for "@" is also a good habit.
  valid_email_rows <- grepl("@", df_cleaned[[email_col_name]], fixed = TRUE)
  df_cleaned <- df_cleaned[valid_email_rows, , drop = FALSE] # drop = FALSE to ensure it stays a data.frame

  # 5. Remove duplicate rows from the entire data frame
  # This happens after all cleaning steps, including lowercasing the email.
  df_cleaned <- unique(df_cleaned)

  return(df_cleaned)
}