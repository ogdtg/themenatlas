# JavaScript and global UI helpers
# Custom Leaflet JS methods used for dynamic map updates

leaflet_js <- tags$script(HTML(
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
