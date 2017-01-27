#' Launch shiny app
#'
#' @title Launch CascadeDashboard Shiny App
#'
#' @export
launch <- function() {
    cat("Press ESC (or Ctrl-C) to get back to the R session\n")
    wd <- system.file("app", package = "CascadeDashboard")
    if (wd == "") {
        stop("Could not find app. Try re-installing `CascadeDashboard`.", call. = FALSE)
    }
    shiny::runApp(wd, quiet = FALSE, display.mode = "normal")
}

#' Is this really because I didn't export it?
#'
#' @export
cluster_test <- function() {
    message("HEY JACK, THIS IS WORKING!")
}
