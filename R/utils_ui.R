#' Create footer spacer
#'
#' @description Adds consistent spacing before the footer
#'
#' @return A div with appropriate spacing
#' @noRd
footer_spacer <- function() {
  shiny::div(
    class = "footer-spacer",
    style = "margin-top: 3rem; width: 100%;"
  )
}
