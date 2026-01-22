#' Adds the content of assets/ to ConApp/
#'
#' @importFrom shiny addResourcePath
#'
#' @noRd
.onLoad <- function(...) {
  shiny::addResourcePath("ConApp", system.file("assets", package = "ConApp"))
}
