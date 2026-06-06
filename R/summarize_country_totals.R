#' Summarize total plastic waste by country
#'
#' Calculates the total amount of plastic waste reported for each country.
#' Grand Total rows and EMPTY country values are removed before
#' summarization.
#'
#' @param data Plastics dataset.
#'
#' @return A tibble with one row per country and total plastic waste.
#' @importFrom dplyr group_by summarize arrange desc
#' @export
summarize_country_totals <- function(data = load_data()) {

  required_cols <- c(
    "parent_company",
    "country",
    "grand_total"
  )

  if (!all(required_cols %in% names(data))) {
    stop(
      "data must contain parent_company, country, and grand_total columns."
    )
  }

  data |>
    clean_plastics_data(
      remove_empty_country = TRUE
    ) |>
    group_by(country) |>
    summarize(
      total_plastic = sum(grand_total, na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(desc(total_plastic))
}
