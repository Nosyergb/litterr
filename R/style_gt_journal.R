#' Style gt table with journal dashboard formatting
#'
#' @param table A tibble.
#' @param title Table title as string.
#' @param subtitle Table subtitle as string.
#' @param percent_cols Column or vector of columns to format as percentages.
#' @param color_cols Column or vector of columns to color.
#'
#' @return A styled gt table.
#' @importFrom gt tab_header md fmt_percent tab_options tab_style cell_text cells_column_labels cell_borders cells_title px data_color everything gt
#' @importFrom scales col_numeric
#' @export
style_gt_journal <- function(table = load_data() |> summarize_plastics_prop(),
                             title = "Plastic Type Changes",
                             subtitle = NULL,
                             percent_cols = NULL,
                             color_cols = NULL) {

  if (!is.character(title) | (length(title) != 1)) {
    stop("title must be a single character string.")
  }

  if (!is.null(subtitle) && (!is.character(subtitle) || length(subtitle) != 1)) {
    stop("subtitle must be NULL or a single character string.")
  }

  styled_table <- table |>
    gt() |>
    tab_header(
      title = md(title),
      subtitle = subtitle
    ) |>
    tab_options(
      heading.background.color = "#EB6864",
      column_labels.background.color = "lightgrey",
      row.striping.background_color = "lightgrey",
      row.striping.include_table_body = TRUE
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels(everything())
    ) |>
    tab_style(
      style = cell_borders(
        sides = "bottom",
        color = "#333333",
        weight = px(2)
      ),
      locations = list(
        cells_column_labels(),
        cells_title(groups = "subtitle")
      )
    ) |>
    tab_options(
      data_row.padding = px(18),
      heading.padding = px(10)
    )

  if (!is.null(percent_cols)) {
    styled_table <- styled_table |>
      fmt_percent(
        columns = percent_cols,
        decimals = 2
      )
  }

  if (!is.null(color_cols)) {
    styled_table <- styled_table |>
      data_color(
        columns = color_cols,
        fn = col_numeric(
          palette = c("lightgreen", "white", "#D95C5C"),
          domain = NULL
        )
      )
  }

  styled_table
}
