#' The application server-side logic
#'
#' Delegates to one module server per tab. Shared data objects are loaded once
#' into the global environment by [load_app_data()] (called from the `onStart`
#' hook in [run_app()]), so the module servers can reference them directly.
#'
#' @param input,output,session Internal parameters for `{shiny}`.
#' @noRd
app_server <- function(input, output, session) {

  thematic::thematic_shiny()

  mod_karte_server("karte")
  mod_berichte_server("berichte")
  mod_selfservice_server("selfservice")
  mod_extern_server("extern")
  mod_info_server("info")
}
