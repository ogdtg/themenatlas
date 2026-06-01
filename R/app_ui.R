#' The application User-Interface
#'
#' Wraps the bslib `page_navbar` (one `nav_panel` per tab module) in the
#' Thurgau dashboard chrome (`tgdashboard::tg_header` / `tg_footer`).
#'
#' @param request Internal parameter for `{shiny}`.
#' @noRd
#' @importFrom bsicons bs_icon
app_ui <- function(request) {
  tagList(
    tgdashboard::tg_header(
      title    = "Themenatlas",
      subtitle = "Statistische Indikatoren der Thurgauer Gemeinden"
    ),
    page_navbar(
      id = "nav",
      title = tags$span(bs_icon("map"), " Thurgauer Themenatlas"),
      theme = bs_theme(
        version = 5,
        primary = "#185FA5",
        success = "#1D9E75",
        warning = "#BA7517",
        info    = "#1D9E75",
        danger  = "#A32D2D",
        "font-size-base" = "0.9rem"
      ),
      # Resources injected into every panel head
      header = tagList(
        add_external_resources(),
        tgdashboard::chart_title_inject_css()
      ),
      fillable = "Karte",

      # --- Tabs (one module each) ----------------------------------------
      mod_karte_ui("karte"),
      mod_berichte_ui("berichte"),
      # mod_selfservice_ui("selfservice"),  # TODO: re-enable after refactor
      # mod_extern_ui("extern"),            # TODO: re-enable after refactor
      # mod_info_ui("info"),                # TODO: re-enable after refactor

      # --- Right-aligned header items ------------------------------------
      nav_spacer(),
      nav_item(input_dark_mode(id = "dark_mode", mode = "light"))
    ),
    tgdashboard::tg_footer("ftr")
  )
}
