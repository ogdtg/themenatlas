#' General Utility Functions for the Shiny App
#'
#' This script provides helper functions for styling and labeling leaflet map elements,
#' as well as a utility function for cleaning and normalizing text strings.
#'
#' @author Felix Lorenz
#' @date 2025-03-17
#' @note Requires `leaflet` package for map-related functions.
#'



#' Set Shape Style in a Leaflet Map
#'
#' Updates the style of a given shape layer in a leaflet map by modifying attributes
#' such as stroke, color, fill, and opacity.
#'
#' @param map A `leaflet` map object.
#' @param data The dataset associated with the map (default: `getMapData(map)`).
#' @param layerId The ID of the layer to modify.
#' @param stroke Logical, whether to enable stroke.
#' @param color Stroke color of the shape.
#' @param weight Stroke width in pixels.
#' @param opacity Stroke opacity (0 to 1).
#' @param fill Logical, whether to enable fill.
#' @param fillColor Fill color of the shape.
#' @param fillOpacity Fill opacity (0 to 1).
#' @param dashArray Line dash pattern.
#' @param smoothFactor How much to simplify the shape.
#' @param noClip Whether to disable shape clipping.
#' @param options Additional style options.
#' @return Modified `leaflet` map object with updated style.
#' @export
setShapeStyle <- function( map, data = getMapData(map), layerId,
                           stroke = NULL, color = NULL,
                           weight = NULL, opacity = NULL,
                           fill = NULL, fillColor = NULL,
                           fillOpacity = NULL, dashArray = NULL,
                           smoothFactor = NULL, noClip = NULL,
                           options = NULL
){
  options <- c(list(layerId = layerId),
               options,
               filterNULL(list(stroke = stroke, color = color,
                               weight = weight, opacity = opacity,
                               fill = fill, fillColor = fillColor,
                               fillOpacity = fillOpacity, dashArray = dashArray,
                               smoothFactor = smoothFactor, noClip = noClip
               )))
  # evaluate all options
  options <- evalFormula(options, data = data)
  # make them the same length (by building a data.frame)
  options <- do.call(data.frame, c(options, list(stringsAsFactors=FALSE)))

  layerId <- options[[1]]
  style <- options[-1] # drop layer column

  #print(list(style=style))
  leaflet::invokeMethod(map, data, "setStyle", "shape", layerId, style);
}


#' Set Shape Label in a Leaflet Map
#'
#' Adds or updates labels for a given shape layer in a leaflet map.
#'
#' @param map A `leaflet` map object.
#' @param data The dataset associated with the map (default: `getMapData(map)`).
#' @param layerId The ID of the layer to modify.
#' @param label The label text to display.
#' @param options Additional label options.
#' @return Modified `leaflet` map object with updated labels.
#' @export
setShapeLabel <- function( map, data = getMapData(map), layerId,
                           label = NULL,
                           options = NULL
){
  options <- c(list(layerId = layerId),
               options,
               filterNULL(list(label = label
               )))
  # evaluate all options
  options <- evalFormula(options, data = data)
  # make them the same length (by building a data.frame)
  options <- do.call(data.frame, c(options, list(stringsAsFactors=FALSE)))

  layerId <- options[[1]]
  style <- options[-1] # drop layer column

  #print(list(style=style))
  leaflet::invokeMethod(map, data, "setLabel", "shape", layerId, label);
}







#' Clean and Normalize a String
#'
#' Converts text to lowercase, replaces German umlauts, and removes special characters.
#' The function ensures that strings are in a standardized format for further processing.
#'
#' @param text A character string to be cleaned.
#' @return A cleaned character string with only lowercase letters, numbers, and underscores.
#' @examples
#' clean_string("München 2025!")  # Returns "muenchen_2025"
#' @export
clean_string <- function(text) {
  text <- tolower(text) # Convert to lowercase

  # Replace German umlauts
  text <- gsub("ä", "ae", text)
  text <- gsub("ö", "oe", text)
  text <- gsub("ü", "ue", text)
  text <- gsub("ß", "ss", text)

  # Replace spaces with underscores
  text <- gsub("\\s+", "_", text)

  # Remove special characters (keep only a-z, 0-9, and underscores)
  text <- gsub("[^a-z0-9_]", "", text)

  return(text)
}

