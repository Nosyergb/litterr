#' Load Break Free From Plastic Data
#'
#' @return a data frame
#' @export
load_data <- function() {

  file_path <- system.file(
    "plastics.rds",
    package = "projectr"
  )

  readRDS(file_path)
}
