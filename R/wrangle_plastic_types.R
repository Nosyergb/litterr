#' Wrangle plastic type totals by year
#'
#' Converts the plastics data from wide to long format and summarizes the
#' total amount and proportion of each plastic type by year.
#' Grand Total rows are removed before summarizing.
#'
#' @param data Plastics dataset.
#'
#' @return A tibble with year, plastic_type, total, and proportion columns.
#' @importFrom dplyr group_by summarize mutate ungroup
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect all_of
#' @export
wrangle_plastic_types <- function(data = load_data()) {

  required_cols <- c(
    "parent_company",
    "year",
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

  plastic_types <- c("hdpe", "ldpe", "o", "pet", "pp", "ps", "pvc")

  data |>
    clean_plastics_data(remove_empty_country = FALSE) |>
    pivot_longer(
      cols = all_of(plastic_types),
      names_to = "plastic_type",
      values_to = "plastic_amount"
    ) |>
    group_by(year, plastic_type) |>
    summarize(
      total = sum(plastic_amount, na.rm = TRUE),
      .groups = "drop"
    ) |>
    group_by(year) |>
    mutate(
      proportion = total / sum(total, na.rm = TRUE)
    ) |>
    ungroup()
}
