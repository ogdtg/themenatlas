#' Run the Shiny Application
#'
#' @param onStart A function that will be called before the app is actually run.
#' @param options A named list of options passed to [shiny::shinyApp()].
#' @param enableBookmarking Bookmarking mode, see [shiny::enableBookmarking()].
#' @param uiPattern A regular expression matched against the request path.
#' @param ... Arguments passed to `golem_opts`.
#'   See `?golem::get_golem_options` for more details.
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(
    onStart = NULL,
    options = list(),
    enableBookmarking = NULL,
    uiPattern = "/",
    ...) {

  # Load all shared data once, before the first request is served, and chain
  # any user-supplied onStart afterwards.
  combined_on_start <- function() {
    load_app_data()
    if (!is.null(onStart)) onStart()
  }

  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = combined_on_start,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(...)
  )
}
