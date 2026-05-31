#' Plot plastic waste by country before and during COVID
#'
#' @param data Plastics data.
#' @param label_n Number of countries to label as numeric.
#' @param label_largest_increase If TRUE, labels countries with the largest increase from 2019 to 2020.
#'   If FALSE, labels countries with the largest 2020 totals.
#' @param point_color Color of the points.
#' @param point_size Size of the points as numeric.
#' @param point_alpha Transparency of the points as numeric.
#'
#' @return A ggplot.
#' @importFrom dplyr filter group_by summarize mutate slice_max
#' @importFrom ggplot2 ggplot aes geom_point geom_abline labs theme_light
#' @importFrom tidyr pivot_wider
#' @export
plot_country_dist <- function(data = load_data(),
                              label_n = 2,
                              label_largest_increase = FALSE,
                              point_color = "#D95C5C",
                              point_size = 3,
                              point_alpha = 0.7) {

  required_cols <- c(
    "parent_company",
    "country",
    "year",
    "grand_total"
  )

  if (!all(required_cols %in% names(data))) {
    stop(
      "data must contain parent_company, country, year, and grand_total columns."
    )
  }

  if (!is.numeric(label_n) | length(label_n) != 1 | label_n < 0) {
    stop("label_n must be a nonnegative number.")
  }

  if (!is.logical(label_largest_increase) |
      length(label_largest_increase) != 1) {
    stop("label_largest_increase must be TRUE or FALSE.")
  }

  if (!is.numeric(point_size) | length(point_size) != 1 |
      point_size <= 0) {
    stop("point_size must be a positive number.")
  }

  if (!is.numeric(point_alpha) | length(point_alpha) != 1 |
      point_alpha < 0 | point_alpha > 1) {
    stop("point_alpha must be a number between 0 and 1.")
  }

  country_change <- data |>
    clean_plastics_data(remove_empty_country = TRUE) |>
    group_by(country, year) |>
    summarize(
      total_plastic = sum(grand_total, na.rm = TRUE),
      .groups = "drop"
    ) |>
    pivot_wider(
      names_from = year,
      values_from = total_plastic,
      names_prefix = "year_"
    ) |>
    filter(!is.na(year_2019), !is.na(year_2020)) |>
    mutate(change = year_2020 - year_2019)

  if (label_largest_increase) {
    labels <- country_change |>
      slice_max(change, n = label_n)
  } else {
    labels <- country_change |>
      slice_max(year_2020, n = label_n)
  }

  country_change |>
    ggplot(aes(x = year_2019, y = year_2020)) +
    geom_point(
      color = point_color,
      alpha = point_alpha,
      size = point_size
    ) +
    geom_abline(
      slope = 1,
      intercept = 0,
      linetype = "dashed"
    ) +
    ggrepel::geom_text_repel(
      data = labels,
      aes(label = country),
      size = 4,
      fontface = "bold"
    ) +
    labs(
      title = "Plastic Waste by Country Before and During COVID",
      subtitle = "Countries above the dashed line reported more plastic waste in 2020 than in 2019",
      x = "Total Plastic Waste in 2019",
      y = "Total Plastic Waste in 2020"
    ) +
    theme_light()
}
