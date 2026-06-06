#' Animate plastic composition by country
#'
#' Creates an animated bar chart showing the composition of plastic types
#' over time for a selected country.
#'
#' @param data Plastics dataset.
#' @param country_name Country to visualize as a character string.
#' @param colors Character vector of 7 colors corresponding to HDPE, LDPE,
#'   PET, PP, PS, PVC, and Other plastics.
#' @param nframes Number of frames in the animation.
#' @param fps Frames per second.
#'
#' @return A gganimate animation object.
#' @importFrom dplyr filter
#' @importFrom ggplot2 ggplot aes geom_col scale_y_continuous scale_fill_manual labs theme_light theme element_text
#' @importFrom scales label_percent
#' @importFrom gganimate transition_states ease_aes animate gifski_renderer
#' @export
animate_country_plastics <- function(
    data = load_data(),
    country_name = "China",
    colors = c(
      "#FDECEC",
      "#F8D6D6",
      "#F1ADAD",
      "#EA8585",
      "#E35C5C",
      "#DE4C4C",
      "#D95C5C"
    ),
    nframes = 80,
    fps = 10
) {

  required_cols <- c(
    "parent_company",
    "country",
    "year",
    "hdpe",
    "ldpe",
    "pet",
    "pp",
    "ps",
    "pvc",
    "o"
  )

  if (!all(required_cols %in% names(data))) {
    stop("data is missing required columns.")
  }

  if (!is.character(colors) || length(colors) != 7) {
    stop("colors must be a character vector of length 7.")
  }

  if (!is.numeric(nframes) || length(nframes) != 1 || nframes <= 0) {
    stop("nframes must be a positive number.")
  }

  if (!is.numeric(fps) || length(fps) != 1 || fps <= 0) {
    stop("fps must be a positive number.")
  }

  plastic_types <- c(
    "hdpe",
    "ldpe",
    "pet",
    "pp",
    "ps",
    "pvc",
    "o"
  )

  color_map <- setNames(colors, plastic_types)

  cleaned_data <- data |>
    clean_plastics_data(remove_empty_country = TRUE)

  matched_country <- validate_country(
    country_name = country_name,
    data = cleaned_data
  )

  country_data <- cleaned_data |>
    filter(country == matched_country) |>
    wrangle_plastic_types()

  country_plot <- country_data |>
    ggplot(
      aes(
        x = plastic_type,
        y = proportion,
        fill = plastic_type
      )
    ) +
    geom_col(show.legend = FALSE) +
    scale_y_continuous(
      labels = label_percent()
    ) +
    scale_fill_manual(
      values = color_map
    ) +
    labs(
      title = paste0(
        matched_country,
        " Plastic Composition in {closest_state}"
      ),
      x = "Plastic Type",
      y = "Proportion"
    ) +
    theme_light() +
    theme(
      plot.title = element_text(
        face = "bold",
        size = 16
      )
    ) +
    transition_states(
      states = year,
      transition_length = 2,
      state_length = 1
    ) +
    ease_aes("cubic-in-out")

  animate(
    country_plot,
    nframes = nframes,
    fps = fps,
    renderer = gifski_renderer()
  )
}
