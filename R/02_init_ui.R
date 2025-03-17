#' init_ui.R
#'
#' This script contains utility functions for initializing the UI components of a bs4Dash-based Shiny dashboard.
#' It provides functions to set up the dashboard header, sidebar, and body, along with custom JavaScript enhancements.
#'
#' Functionality:
#' - Initializes the dashboard header with a title and a reference link.
#' - Sets up the sidebar with dynamically provided content.
#' - Defines the dashboard body, including custom CSS, JavaScript, and dynamic elements.
#' - Integrates JavaScript to detect screen size, handle new users, and update leaflet map styles dynamically.
#'
#' Dependencies:
#' - `shiny`, `shinyjs`, `bs4Dash`, `shinybrowser`
#'
#' Output:
#' - A structured UI layout for the Shiny application.
#' - Interactive JavaScript enhancements for better user experience.


#' Initialize the dashboard header
#'
#' This function creates a header for the dashboard, including a brand title and a company logo.
#'
#' @param dashboard_title A string specifying the title of the dashboard.
#' @param reference A URL linking to the company or reference website (default: 'https://statistik.tg.ch').
#'
#' @return A `bs4Dash::dashboardHeader` object.
init_header <- function(dashboard_title,reference='https://statistik.tg.ch'){
  bs4Dash::dashboardHeader(
    title = bs4Dash::dashboardBrand(
      title = dashboard_title,
      href = reference,
    ),
    tags$li(
      a(
        href = reference,
        img(
          src = 'https://www.tg.ch/public/upload/assets/20/logo-kanton-thurgau.svg',
          title = "Company Home",
          height = "30px",
          class = "logoTg"
        ),
        style = "padding-top:10px; padding-bottom:10px;"
      ),
      class = "dropdown"
    )
  )
}


#' Initialize the dashboard sidebar
#'
#' This function creates a sidebar for the dashboard using the provided content.
#'
#' @param sidebar_content UI elements to be included in the sidebar.
#'
#' @return A `bs4Dash::dashboardSidebar` object.
init_sidebar <- function(sidebar_content){
  bs4Dash::dashboardSidebar(
    sidebar_content
  )
}


#' Initialize the dashboard body
#'
#' This function sets up the main body of the dashboard, including CSS, JavaScript, and dynamic UI content.
#'
#' @param db_content UI elements to be displayed in the dashboard body.
#'
#' @return A `bs4Dash::dashboardBody` object.
init_body <- function(db_content){
  bs4Dash::dashboardBody(
    useShinyjs(),  # Initialize shinyjs
    shinybrowser::detect(),
    tags$head(
      includeCSS("www/dashboard_style.css")  # Make sure the path to your CSS is correct
    ),
    HTML('<script src="https://cdn.jsdelivr.net/npm/js-cookie@rc/dist/js.cookie.min.js"></script>'),
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
      $("body").addClass("fixed");
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

// ✅ New function to update tooltips dynamically
window.LeafletWidget.methods.setLabel = function(category, layerId, label){
  var map = this;
  if (!layerId){
    return;
  } else if (!(typeof(layerId) === "object" && layerId.length)){ // in case a single layerid is given
    layerId = [layerId];
  }

  //convert columnstore to row store
  //label = HTMLWidgets.dataframeToD3(label);
  //console.log(label);

  layerId.forEach(function(d,i){
    var layer = map.layerManager.getLayer(category, d);
    if (layer){ // or should this raise an error?
      // layer.setStyle(style[i]);
      layer.unbindTooltip();
      layer.bindTooltip(label[i])
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
    )),
    db_content
  )
}

