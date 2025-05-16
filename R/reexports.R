# This file is used to re-export functions from other packages
# so they are available to the user when mranalytics is loaded.

# Re-exporting mutate from dplyr
#' @importFrom dplyr mutate
#' @export mutate
NULL

# Re-exporting the pipe operator (%>%) from dplyr (which itself gets it from magrittr)
# OR, more directly from magrittr if you prefer to list magrittr in Imports:
# #' @importFrom magrittr %>%
#' @importFrom dplyr %>%
#' @export %>%
NULL

# Re-exporting read_excel from readxl
#' @importFrom readxl read_excel
#' @export read_excel
NULL

# Add any other functions you want to re-export here using the same pattern:
# #' @importFrom packageName functionName
# #' @export functionName
# NULL