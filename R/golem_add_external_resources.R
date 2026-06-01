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
      includeCSS(app_sys_or_local("www/dashboard_style.css"))
    ),
    HTML('<script src="https://cdn.jsdelivr.net/npm/js-cookie@rc/dist/js.cookie.min.js"></script>'),
    leaflet_widget_js()
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


#' Custom Leaflet widget JavaScript methods
#'
#' Adds the `setStyle`, `setRadius` and `setLabel` methods to the Leaflet
#' widget so polygon styles, marker radii and tooltips can be updated through
#' `leafletProxy()` without redrawing the whole map. Also reports browser
#' width to the server and wires up generic clickable elements.
#' @noRd
leaflet_widget_js <- function() {
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
    window.LeafletWidget.methods.setStyle = function(category, layerId, style){
      var map = this;
      if (!layerId) return;
      if (!(typeof(layerId) === "object" && layerId.length)) layerId = [layerId];
      style = HTMLWidgets.dataframeToD3(style);
      layerId.forEach(function(d,i){
        var layer = map.layerManager.getLayer(category, d);
        if (layer) layer.setStyle(style[i]);
      });
    };
    window.LeafletWidget.methods.setRadius = function(layerId, radius){
      var map = this;
      if (!layerId) return;
      if (!(typeof(layerId) === "object" && layerId.length)) {
        layerId = [layerId];
        radius = [radius];
      }
      layerId.forEach(function(d,i){
        var layer = map.layerManager.getLayer("marker", d);
        if (layer) layer.setRadius(radius[i]);
      });
    };
    window.LeafletWidget.methods.setLabel = function(category, layerId, label){
      var map = this;
      if (!layerId){
        return;
      } else if (!(typeof(layerId) === "object" && layerId.length)){
        layerId = [layerId];
      }
      layerId.forEach(function(d,i){
        var layer = map.layerManager.getLayer(category, d);
        if (layer){
          layer.unbindTooltip();
          layer.bindTooltip(label[i]);
        }
      });
    };
    $(document).on("shiny:connected", function(e) {
      var width = $(window).width();
      Shiny.setInputValue("screen_width", width);
    });
    $(window).resize(function() {
      var width = $(window).width();
      Shiny.setInputValue("screen_width", width);
    });
    '
  ))
}
