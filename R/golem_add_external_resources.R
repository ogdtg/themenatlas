#' Add external resources to the application UI
#'
#' Binds the dashboard stylesheet, the js-cookie dependency and the custom
#' Leaflet widget methods (setStyle / setRadius / setLabel) used for dynamic
#' map updates, plus a small helper that reports the browser width.
#'
#' @return A `tagList` to be included in the application head.
#' @importFrom htmltools tagList
#' @noRd
add_external_resources <- function() {
  tagList(
    shinyjs::useShinyjs(),
    shinybrowser::detect(),
    tags$head(
      includeCSS(app_sys_or_local("www/dashboard_style.css")),
      # Constrain main content width to match tg_header / tg_footer (1320 px max)
      tags$style(HTML("
        .bslib-page-navbar > .tab-content,
        .bslib-page-navbar > .container-fluid {
          max-width: 1320px;
          margin-left: auto;
          margin-right: auto;
          width: 100%;
        }
        /* Prevent the sidebar layout from overflowing horizontally */
        .bslib-sidebar-layout {
          max-width: 100%;
          overflow-x: hidden;
        }
      "))
    ),
    HTML('<script src="https://cdn.jsdelivr.net/npm/js-cookie@rc/dist/js.cookie.min.js"></script>'),
    shiny_cookie_js()
  )
}


#' Resolve a resource path from the installed package or the working directory
#'
#' Allows the app to run both as an installed package (`system.file`) and from
#' the project root during development (`pkgload::load_all` + `run_app`).
#'
#' @param rel Relative path of the resource.
#' @noRd
app_sys_or_local <- function(rel) {
  pkg_path <- app_sys(rel)
  if (nzchar(pkg_path)) {
    return(pkg_path)
  }
  rel
}


#' Cookie + screen-width JavaScript helpers
#'
#' Reports browser width to the server and tracks new-user cookie state.
#' @noRd
shiny_cookie_js <- function() {
  tags$script(HTML(
    '
    $(document).on("shiny:connected", function(){
      var newUser = Cookies.get("new_user");
      if(newUser === "false") return;
      Shiny.setInputValue("new_user", true);
      Cookies.set("new_user", false);
    });
    $(document).on("click", ".clickable-element", function() {
      var clicked_id = $(this).attr("id");
      Shiny.setInputValue("clicked_element_id", clicked_id, {priority: "event"});
    });
    $(document).on("shiny:connected", function(e) {
      Shiny.setInputValue("screen_width", $(window).width());
    });
    $(window).resize(function() {
      Shiny.setInputValue("screen_width", $(window).width());
    });
    '
  ))
}
