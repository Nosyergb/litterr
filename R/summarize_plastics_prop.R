#' Summarize changes in plastic type composition from 2019 to 2020
#'
#' @param data Plastics dataset.
#' @param type Type of change to return: "prop", "yoy", or "both".
#'
#' @return A tibble.
#' @importFrom dplyr filter group_by summarize arrange mutate lag ungroup select all_of
#' @importFrom tidyr pivot_longer
#' @export
summarize_plastics_prop <- function(data = load_data(), type = "both") {

  if (!type %in% c("prop", "yoy", "both")) {
    stop("type must be 'prop', 'yoy', or 'both'")
  }

  required_cols <- c(
    "parent_company",
    "year",
    "grand_total"
  )

  if (!all(required_cols %in% names(data))) {
    stop("data is missing required columns.")
  }

  plastic_types <- c("hdpe", "ldpe", "pet", "pp", "ps", "pvc", "o")

  summary_data <- data |>
    clean_plastics_data() |>
    pivot_longer(
      cols = all_of(plastic_types),
      names_to = "plastic_type",
      values_to = "amount"
    ) |>
    group_by(plastic_type, year) |>
    summarize(
      total_type = sum(amount, na.rm = TRUE),
      total_all = sum(grand_total, na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(plastic_type, year) |>
    group_by(plastic_type) |>
    mutate(
      prop = total_type / total_all,
      prop_change = prop - lag(prop),
      yoy_change = (total_type - lag(total_type)) / lag(total_type)
    ) |>
    filter(year == 2020) |>
    ungroup()

  if (type == "prop") {
    summary_data |>
      select(plastic_type, prop_change) |>
      arrange(prop_change)

  } else if (type == "yoy") {
    summary_data |>
      select(plastic_type, yoy_change) |>
      arrange(yoy_change)

  } else {
    summary_data |>
      select(plastic_type, prop_2020 = prop, prop_change, yoy_change) |>
      arrange(prop_change, yoy_change)
  }
}
