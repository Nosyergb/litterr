#' Test whether plastic type composition differs between two countries
#'
#' Performs a chi-square test of independence to determine whether the
#' distribution of plastic types differs between two selected countries.
#' Grand Total rows and EMPTY country values are removed before analysis.
#'
#' @param data Plastics dataset.
#' @param country_one First country to compare.
#' @param country_two Second country to compare.
#'
#' @return A chi-square test object.
#' @importFrom dplyr filter group_by summarize
#' @export
compare_country_plastic_types <- function(
    data = load_data(),
    country_one,
    country_two
) {

  required_cols <- c(
    "parent_company",
    "country",
    "hdpe",
    "ldpe",
    "o",
    "pet",
    "pp",
    "ps",
    "pvc"
  )

  if (!all(required_cols %in% names(data))) {
    stop("data is missing required columns.")
  }

  cleaned_data <- data |>
    clean_plastics_data(remove_empty_country = TRUE)

  matched_one <- validate_country(
    country_name = country_one,
    data = cleaned_data
  )

  matched_two <- validate_country(
    country_name = country_two,
    data = cleaned_data
  )

  if (tolower(matched_one) == tolower(matched_two)) {
    stop("country_one and country_two must be different countries.")
  }

  plastic_types <- c("hdpe", "ldpe", "o", "pet", "pp", "ps", "pvc")

  comparison_data <- cleaned_data |>
    filter(country %in% c(matched_one, matched_two)) |>
    group_by(country) |>
    summarize(
      hdpe = sum(hdpe, na.rm = TRUE),
      ldpe = sum(ldpe, na.rm = TRUE),
      o = sum(o, na.rm = TRUE),
      pet = sum(pet, na.rm = TRUE),
      pp = sum(pp, na.rm = TRUE),
      ps = sum(ps, na.rm = TRUE),
      pvc = sum(pvc, na.rm = TRUE),
      .groups = "drop"
    )

  plastics_matrix <- as.matrix(comparison_data[plastic_types])
  rownames(plastics_matrix) <- comparison_data$country

  stats::chisq.test(plastics_matrix)
}
