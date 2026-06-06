#' Clean plastics data
#'
#' Removes Grand Total rows and optionally removes empty country values.
#'
#' @param data Plastics dataset.
#' @param remove_empty_country Logical. If TRUE, removes rows where country is "EMPTY".
#'
#' @return A cleaned data frame.
#' @importFrom dplyr filter
clean_plastics_data <- function(data = load_data(),
                                remove_empty_country = FALSE) {

  required_cols <- c("parent_company", "country")

  if (!all(required_cols %in% names(data))) {
    stop("data must contain parent_company and country columns.")
  }

  if (!is.logical(remove_empty_country) ||
      length(remove_empty_country) != 1) {
    stop("remove_empty_country must be TRUE or FALSE.")
  }

  cleaned_data <- data |>
    filter(parent_company != "Grand Total")

  if (remove_empty_country) {
    cleaned_data <- cleaned_data |>
      filter(country != "EMPTY")
  }

  cleaned_data
}
