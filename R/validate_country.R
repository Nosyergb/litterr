#' Validate a country name
#'
#' Checks whether a country exists in the plastics dataset and returns the
#' correctly capitalized country name.
#'
#' @param country_name Country name as a character string.
#' @param data Plastics dataset.
#'
#' @return A validated country name.
validate_country <- function(country_name, data = load_data()) {

  if (!is.character(country_name) || length(country_name) != 1) {
    stop("country_name must be a single character string.")
  }

  if (!"country" %in% names(data)) {
    stop("data must contain a country column.")
  }

  countries <- unique(data$country)

  matched_country <- countries[
    tolower(countries) == tolower(country_name)
  ]

  if (length(matched_country) == 0) {
    stop(
      paste0(
        "Country '",
        country_name,
        "' not found in the dataset."
      )
    )
  }

  matched_country[1]
}
