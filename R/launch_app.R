#' Launch the bolteR Shiny Dashboard
#'
#' Opens the interactive Shiny dashboard in your default browser.
#' The dashboard provides two tools:
#' \itemize{
#'   \item \strong{Wound Table} — enter dice count, toughness, hit roll and
#'         modifiers to see expected wounds across five strength profiles.
#'   \item \strong{Dice Distribution} — simulate rolling a set of dice many
#'         times and inspect the resulting success-rate distribution.
#' }
#'
#' @return Does not return a value; opens the Shiny app in the browser.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   launch_bolteR()
#' }
launch_bolteR <- function() {
  app_dir <- system.file("app", package = "bolteR")
  if (app_dir == "") {
    stop("Could not find app directory. Try re-installing bolteR.", call. = FALSE)
  }
  shiny::runApp(app_dir, display.mode = "normal")
}
