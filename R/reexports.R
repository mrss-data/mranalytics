# This file is used to re-export functions from other packages
# so they are available to the user when mranalytics is loaded.


#' @export
dplyr::mutate

#' @export
dplyr::`%>%` # Note the backticks for special operators

#' @export
readxl::read_excel

# Add any other functions you want to re-export here.