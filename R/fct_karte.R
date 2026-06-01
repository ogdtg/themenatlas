#' Geographic Data Processing and Interactive Visualization for Shiny
#'
#' This script contains functions that support interactive geographic data
#' visualization within an R Shiny application. The primary functionalities
#' include:
#' - Filtering and merging datasets with spatial data.
#' - Applying dynamic color mapping to numeric values.
#' - Validating user input conditions to prevent errors.
#' - Updating Shiny UI elements based on available data.
#'
#' Key Features:
#' - Automatic handling of different data types (e.g., percentages vs. absolute values).
#' - Conditional color palettes based on thresholds and data ranges.
#' - Real-time UI adjustments, such as dynamically updating year selections.
#'
#' Dependencies:
#' - dplyr
#' - leaflet
#' - shiny
#' - magrittr
#'
#' @author [Felix Lorenz]
#' @date [2025-03-17]



#' Prepare a value-mapped geographic dataset for echarts4r choropleth rendering.
#'
#' Filters and joins the indicator data with geographic data, applying value-type
#' (absolute / share) selection. The returned `color_map` data frame is passed
#' directly to [build_echarts_map()]; echarts4r handles colour binning via its
#' own `e_visual_map()`.
#'
#' @param df Data frame with columns `bfs_nr_gemeinde`, `jahr`, `value`
#'   (and optionally `filter1`, `share`).
#' @param geo_data sf object with columns `bfsnr` and `name`.
#' @param input Shiny input list (uses `year`, `filter1`, `value_type`,
#'   `indicator`, `topic`).
#' @param palette_ds Unused — kept for API compatibility.
#' @param palette_ds_alternative Unused — kept for API compatibility.
#' @return A list with element `color_map` (data frame ready for echarts4r).
prepare_color_map <- function(df, geo_data, input, palette_ds, palette_ds_alternative) {
  if (!is.null(input$year)) df <- df %>% filter(jahr %in% input$year)
  if (input$filter1 != "Kein Filter" && "filter1" %in% colnames(df)) {
    df <- df %>% filter(filter1 %in% input$filter1)
  }
  if (nrow(df) > 80) req(input$filter1)

  color_map <- as.data.frame(geo_data) %>%
    left_join(df, by = c("bfsnr" = "bfs_nr_gemeinde"))
  color_map$value <- as.numeric(color_map$value)

  # Switch to share column when user requests percentage view
  if (!is.null(input$value_type) && input$value_type == "Prozentual" &&
      "share" %in% names(color_map)) {
    color_map$value <- as.numeric(color_map$share)
  }

  color_map$tooltip_text <- paste0(
    color_map$name, " | ", input$indicator, ": ",
    round(color_map$value, 2)
  )

  list(color_map = color_map)
}







init_selected_data <- function(input,area_data,selected_data){

  observeEvent(list(input$topic, input$subtopic, input$indicator,area_data(),input$area),{
    req(input$topic, input$subtopic, input$indicator,area_data())
    topics <- names(area_data())

    if (input$topic %in% topics){

      subtopics <- names(area_data()[[input$topic]])

      if (input$subtopic %in% subtopics){

        indicators <- names(area_data()[[input$topic]][[input$subtopic]])

        if (input$indicator %in% indicators){


          temp_df <- area_data()[[input$topic]][[input$subtopic]][[input$indicator]]


          if (input$area == "Bezirk") {
            temp_df <- temp_df %>% filter(str_detect(bfs_nr_gemeinde, "^2"))
          } else if (input$area == "Gemeinde") {
            temp_df <- temp_df %>% filter(str_detect(bfs_nr_gemeinde, "^4"))
          }

          selected_data(temp_df)
          #print(head(selected_data()))


        }


      }

    }
  },ignoreInit = F)




}

check_filter1 <- function(selected_data,input){
  req(selected_data(), input$filter1)

  df <- selected_data()

  if ("filter1" %in% colnames(selected_data())){
    if (input$filter1 %in% unique(df$filter1)){
      return(TRUE)
    }

  } else if (!"filter1" %in% colnames(selected_data())){

    if (input$filter1 == "Kein Filter"){
      return(TRUE)
    }
  }
  return(FALSE)
}


check_year <- function(selected_data,input){
  req(selected_data(), input$year)

  df <- selected_data()
  #print(selected_data()["jahr"])
  if (input$year %in% unique(df$jahr)){
    return(TRUE)
  } else {
    return(FALSE)
  }

}


check_value_type <- function(selected_data,input){
  req(selected_data(), input$value_type)

  if (!"share" %in% colnames(selected_data())){
    if (input$value_type == "Prozentual"){
      return(FALSE)
    }
  }
  return(TRUE)
}


check_all_filters <- function(input,selected_data){


    if (!check_filter1(selected_data,input)){
      #print("Filter not correct")
      return(FALSE)
    }

    if (!check_year(selected_data,input)){
      #print("Year not correct")

      return(FALSE)
    }

    if (!check_year(selected_data,input)){
      #print("Valuetype not correct")

      return(FALSE)
    }
    return(TRUE)

}


#' Check if input conditions allow data rendering
#'
#' @param input List of user inputs.
#' @param selected_data Reactive function returning the dataset.
#' @param prevYear, prevValueType, prevIndicator, prevFilter,prevArea Previous user selections.
#' @return Logical value indicating whether the data should be updated.
check_conditions_func <- function(input, selected_data,prevYear,prevValueType,prevIndicator,prevFilter,prevArea) {
  df <- selected_data()



  filter_is_null <- is.null(input$filter1)
  value_type_is_null <- is.null(input$value_type)
  has_filter <- "filter1" %in% colnames(df)
  has_share <- "share" %in% colnames(df)
  contains_year <- input$year %in% df$jahr
  # Wenn es eine filter1 Spalte gibt aber kein Filter gewählt ist


  if (filter_is_null) {
    #print("filter_is_null")
    return(FALSE)
  }
  if (input$filter1 == "Kein Filter" & has_filter) {
    #print("Filter ist kein Filter aber Tabelle besitz Filter")

    return(FALSE)
  }

  # Wenn es einen Filter gibt, aber keine Spalte zum filtern
  if (input$filter1 != "Kein Filter" & !has_filter) {
    #print("Filter ist angegeben aber Tabelle besitzt keinen")

    return(FALSE)
  }


  # Wenn es keine share Spalte gibt aber ein value_type gewählt ist

  if (input$value_type == "Prozentual" & !has_share) {
    #print("keine share Spalte gibt aber  value_type ist gewählt")

    return(FALSE)
  }

  # Wenn das Jahr nicht im datensatz vorhanden ist
  if (!contains_year) {
    #print(" Jahr nicht im datensatz vorhanden")

    return(FALSE)
  }

  # Wenn es einen korrekten Filter gibt, dessen Wert aber nicht in der filter1 Spalte verfügbar ist
  if (input$filter1 != "Kein Filter") {
    contains_filter <- input$filter1 %in% df$filter1
    if (!contains_filter) {
      #print(" Wenn es einen korrekten Filter gibt, dessen Wert aber nicht in der filter1 Spalte verfügbar ist")

      return(FALSE)
    }
    # Wenn es einen korrekten Filter gibt, es für die Kombi aus Jahr und filter aber keine Daten gibt
    df_filtered <- df %>% filter(jahr == input$year &
                                   filter1 == input$filter1)
    num_rows <-  df_filtered %>% nrow()
    if (num_rows == 0) {
      #print(" Wenn es einen korrekten Filter gibt, es für die Kombi aus Jahr und filter aber keine Daten gibt")

      return(FALSE)
    }
    if (sum(is.na(df_filtered$value)) == num_rows) {

      return(FALSE)
    }
  }

  if (
    isTRUE(identical(prevYear(), input$year)) &&
    isTRUE(identical(prevValueType(), input$value_type)) &&
    isTRUE(identical(prevIndicator(), input$indicator)) &&
    isTRUE(identical(prevArea(), input$area)) &&
    isTRUE(identical(prevFilter(), input$filter1))
  ) {
    #print(" Keine Veränderung")

    return(FALSE)
  } else {
    prevYear(input$year)
    prevValueType(input$value_type)
    prevIndicator(input$indicator)
    prevFilter(input$filter1)
    prevArea(input$area)


    return(TRUE)

  }
}


#' Determine if the selected values have changed
#'
#' @param prevYear, prevValueType, prevIndicator, prevFilter Previous user selections.
#' @return Logical value indicating whether the values have changed.
values_have_changed <- function(prevYear,prevValueType,prevIndicator,prevFilter){
  if (
    isTRUE(identical(prevYear(), input$year)) &&
    isTRUE(identical(prevValueType(), input$value_type)) &&
    isTRUE(identical(prevIndicator(), input$indicator)) &&
    isTRUE(identical(prevFilter(), input$filter1))
  ) {
    return(FALSE)
  } else {
    return(TRUE)

  }
}


#' Render UI selections dynamically based on data changes
#'
#' @param session Shiny session object.
#' @param input List of user inputs.
#' @param output Shiny output list.
#' @param area_names df with names and numbers of the area
#' @param selected_data Reactive function returning the dataset.
render_selections_dynamic <- function(session,input,output,selected_data,area_names){


  observeEvent(selected_data(), {
    df <- selected_data()

    bezirk_data <- area_names()


    if ("filter1" %in% colnames(df) && input$filter1!="Kein Filter") {
      df <- df %>% filter(filter1 %in% input$filter1) %>%
        filter(!is.na(value))
    }

    # ✅ Get the newest year available
    latest_year <- max(df$jahr, na.rm = TRUE)

    updateSelectizeInput(session, "year",
                         choices = unique(df$jahr),
                         selected = latest_year)  # ⬅️ Automatically selects the latest year

    updateSelectizeInput(session, "bfs_nr_gemeinde",
                         choices = setNames(bezirk_data$bfs_nr_gemeinde,bezirk_data$name_gemeinde),
                         selected = "")  # ⬅️ No default Gemeinde selected

  })


}

#' Render filter UI dynamically based on data availability
#'
#' @param session Shiny session object.
#' @param input List of user inputs.
#' @param output Shiny output list.
#' @param selected_data Reactive function returning the dataset.
render_filter_ui <- function(session,input,output,selected_data){

  observeEvent(selected_data(), {
    df <- selected_data()


    # ✅ Dynamically render UI only if "filter1" exists
    output$filter_ui <- renderUI({
      if ("filter1" %in% colnames(df)) {
        selectizeInput(session$ns("filter1"), "Filter", choices = unique(df$filter1), selected = unique(df$filter1)[1])
      } else {
        selectizeInput(session$ns("filter1"), "Filter", choices ="Kein Filter", selected = "Kein Filter",options = list(create = FALSE,
                                                                                                                        onDelete = I("function() { return false; }"),  # Prevent deletion
                                                                                                                        plugins = list("restore_on_backspace")))
      }
    })
  })

}


#' Update the available years when the filter selection changes
#'
#' @param session Shiny session object.
#' @param input List of user inputs.
#' @param output Shiny output list.
#' @param selected_data Reactive function returning the dataset.
update_year_on_filter <- function(session,input,output,selected_data){
  observeEvent(input$filter1, {
    df <- selected_data()

    # Filter years based on selected filter1
    if ("filter1" %in% colnames(df) && input$filter1!="Kein Filter") {
      df <- df %>% filter(filter1 %in% input$filter1) %>%
        filter(!is.na(value))
    }

    # Update year choices dynamically
    # (update*Input namespaces via `session`, so pass the bare id)
    updateSelectizeInput(session, "year",
                         choices = unique(df$jahr),
                         selected = max(df$jahr, na.rm = TRUE))  # Select latest year
  })

  # Dynamically render filter UI (only if filter1 exists)
  output$radio_bas_perc <- renderUI({
    df <- selected_data()
    if ("share" %in% colnames(df)) {
      radioButtons(session$ns("value_type"),"",choices = c("Absolut","Prozentual"),selected = "Absolut")
    } else {
      radioButtons(session$ns("value_type"),"",choices = c("Absolut"),selected = "Absolut")
    }
  })
}


#' Build an echarts4r choropleth map from a colour-mapped dataset.
#'
#' @param color_map sf-joined data frame with `name`, `value`, `category`, `tooltip_text`.
#' @param geojson_str GeoJSON string (WGS84) for the map region.
#' @param indicator Label used in the visual-map legend.
#' @param map_name Internal echarts map name (must be unique per area type).
#' @return An echarts4r widget.
build_echarts_map <- function(color_map, geojson_str, indicator, map_name = "thurgau") {
  df_plot <- as.data.frame(color_map) %>%
    select(name, value) %>%
    filter(!is.na(value))

  df_plot %>%
    echarts4r::e_charts(name) %>%
    echarts4r::e_map_register(map_name, geojson_str) %>%
    echarts4r::e_map(value, map = map_name,
                     name = indicator,
                     nameProperty = "name") %>%
    echarts4r::e_visual_map(
      value,
      type       = "piecewise",
      show       = TRUE,
      orient     = "vertical",
      right      = 0,
      bottom     = 20,
      itemSymbol = "rect"
    ) %>%
    echarts4r::e_tooltip(
      trigger   = "item",
      formatter = echarts4r::e_tooltip_item_formatter("decimal", digits = 2)
    ) %>%
    echarts4r::e_toolbox_feature("saveAsImage")
}


#' Initialize the echarts4r choropleth map with grey placeholder fill.
#'
#' @param output Shiny output object.
#' @param input Shiny input object.
#' @param geo_data Reactive sf spatial dataset.
#' @param geo_data_geojson Reactive GeoJSON string (WGS84).
init_map <- function(output, input, geo_data, geo_data_geojson) {
  observeEvent(geo_data_geojson(), {
    geojson_str <- geo_data_geojson()
    req(geojson_str)

    sf_obj  <- geo_data()
    df_init <- as.data.frame(sf_obj) %>%
      select(name) %>%
      mutate(value = NA_real_)

    output$map <- echarts4r::renderEcharts4r({
      df_init %>%
        echarts4r::e_charts(name) %>%
        echarts4r::e_map_register("thurgau_init", geojson_str) %>%
        echarts4r::e_map(value, map = "thurgau_init",
                         name = "Gemeinden", nameProperty = "name",
                         itemStyle = list(areaColor = "#cccccc",
                                          borderColor = "white",
                                          borderWidth = 1)) %>%
        echarts4r::e_tooltip(trigger = "item") %>%
        echarts4r::e_on("click", "function(params){ Shiny.setInputValue(this.id + '_clicked_data', params.data, {priority: 'event'}); }")
    })
  })
}







#' Update and render the data table based on selected filters.
#'
#' @param session Shiny session object.
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param selected_data Reactive dataset filtered based on user inputs.
#' @param check_conditions Function to verify if table update should proceed.
modify_table <- function(session, input, output, selected_data, check_conditions,area_names) {
  observeEvent(check_conditions(), {
    req(check_conditions())  # Ensure all inputs are in a "ready" state
    req(selected_data())

    output$data_table <- renderDT({
      df <- selected_data()

      #print(head(df))
      #print("__________________________________")

      if (input$area == "Bezirk") {
        df <- df %>% filter(str_detect(bfs_nr_gemeinde, "^2"))
      } else if (input$area == "Gemeinde") {
        df <- df %>% filter(str_detect(bfs_nr_gemeinde, "^4"))
      }

      # Apply filters
      if (!is.null(input$year)) {
        df <- df %>% filter(jahr %in% input$year)
      }

      if (input$filter1 != "Kein Filter" && "filter1" %in% colnames(df)) {
        df <- df %>% filter(filter1 %in% input$filter1)
      }

      value_type <- ""
      percentage <- ""

      if (!is.null(input$value_type)) {
        value_type <- paste0(" (", input$value_type, ") ")
        if (input$value_type == "Prozentual") {
          if ("share" %in% names(df)) {
            df$value <- df$share
            percentage <- "%"
          }
        }
      }

      value_name <- paste0(input$indicator, value_type, input$year)

      if ("filter1" %in% colnames(df)) {
        df <- df %>%
          left_join(area_names(), "bfs_nr_gemeinde") %>%
          select(bfs_nr_gemeinde, name_gemeinde, filter1, value) %>%
          mutate(value = round(value, 2)) %>%
          arrange(desc(value))

        col_labels <- c("BFS Nr.", "Gemeinde", "Filter", value_name)
      } else {
        df <- df %>%
          left_join(area_names(), "bfs_nr_gemeinde") %>%
          select(bfs_nr_gemeinde, name_gemeinde, value) %>%
          mutate(value = round(value, 2)) %>%
          arrange(desc(value))

        col_labels <- c("BFS Nr.", "Gemeinde", value_name)
      }

      datatable(df,
                colnames = col_labels,
                options = list(dom = 't', pageLength = nrow(df))) %>%
        formatStyle(
          "bfs_nr_gemeinde",
          target = 'row',
          backgroundColor = styleEqual(input$bfs_nr_gemeinde, 'yellow')
        )
    })
  }, ignoreInit = TRUE)
}


modify_map_and_table <- function(id, session, input, output, selected_data, geo_data,
                                 geo_data_geojson, palette_ds, palette_ds_alternative,
                                 check_all_filters, debounced_inputs, counter, area_names) {

  observeEvent(debounced_inputs(), {
    counter(counter() + 1)
    req(selected_data(), input$year, input$filter1, input$value_type, geo_data(), geo_data_geojson())

    result    <- prepare_color_map(selected_data(), geo_data(), input, palette_ds, palette_ds_alternative)
    color_map <- result$color_map

    # Re-render the echarts4r map with updated data
    output$map <- echarts4r::renderEcharts4r({
      build_echarts_map(color_map, geo_data_geojson(), input$indicator) %>%
        echarts4r::e_on("click", "function(params){ Shiny.setInputValue(this.id + '_clicked_data', params.data, {priority: 'event'}); }")
    })


        output$data_table <- renderDT({
          df <- selected_data()



          if (input$area == "Bezirk"){
            df <- df %>%
              filter(str_detect(bfs_nr_gemeinde,"^2"))
          }
          if (input$area == "Gemeinde"){
            df <- df %>%
              filter(str_detect(bfs_nr_gemeinde,"^4"))
          }
          # Apply filters
          if (!is.null(input$year))
            df <- df %>% filter(jahr %in% input$year)
          if (input$filter1 != "Kein Filter" &&
              "filter1" %in% colnames(df))
            df <- df %>% filter(filter1 %in% input$filter1)

          if (!is.null(input$value_type)) {
            value_type =  paste0(" (", input$value_type, ") ")
            if (input$value_type == "Prozentual") {
              if (!"share" %in% names(df)) {
                percentage = ""
              } else {
                df$value <- df$share
                percentage = "%"
              }
            } else {
              percentage = ""
            }
          } else {
            percentage = ""
            value_type = " "
          }

          value_name <- paste0(input$indicator, value_type, input$year)

          # Select relevant columns
          if ("filter1" %in% colnames(df)) {
            df <- df %>%
              left_join(area_names(), "bfs_nr_gemeinde") %>%
              select(bfs_nr_gemeinde, name_gemeinde, filter1, value) %>%
              mutate(value = round(value, 2)) %>%
              arrange(desc(value))
            col_labels = c("BFS Nr.", "Gemeinde", "Filter", value_name)

          } else {
            df <- df %>%
              left_join(area_names(), "bfs_nr_gemeinde") %>%
              select(bfs_nr_gemeinde, name_gemeinde, value) %>%
              mutate(value = round(value, 2)) %>%
              arrange(desc(value))

            col_labels = c("BFS Nr.", "Gemeinde", value_name)
          }



          # Highlight selected row
          datatable(df,
                    colnames = col_labels,
                    options = list(dom = 't', pageLength = nrow(df))) %>%
            formatStyle(
              "bfs_nr_gemeinde",
              target = 'row',
              backgroundColor = styleEqual(input$bfs_nr_gemeinde, 'yellow')
            )


        })

      # }


  })

}


#' Update the Gemeinde selection when a region is clicked on the echarts4r map.
#'
#' The echarts4r click event for a map series arrives as `input$map_clicked_data`,
#' a list with at least a `name` field (the feature's name property in GeoJSON).
#' We resolve `name` → `bfsnr` via `geo_data`.
#'
#' @param session Shiny session object.
#' @param input Shiny input object.
#' @param geo_data Reactive sf dataset (has columns `name` and `bfsnr`).
update_gemeinde_selection_on_click <- function(session, input, geo_data) {
  observeEvent(input$map_clicked_data, {
    clicked_name <- input$map_clicked_data$name
    req(clicked_name)

    # Resolve name → bfsnr
    match_row <- as.data.frame(geo_data()) %>%
      filter(name == clicked_name)
    if (nrow(match_row) > 0) {
      updateSelectizeInput(session, "bfs_nr_gemeinde",
                           selected = as.character(match_row$bfsnr[1]))
    }
  })
}


#' Handle Gemeinde selection changes: highlight region on the echarts4r map.
#'
#' echarts4r supports programmatic highlighting via `e_dispatch_action_p()`.
#' When a Gemeinde is selected we dispatch a "highlight" action; deselect
#' sends a "downplay" across all regions.
#'
#' @param session Shiny session object.
#' @param input Shiny input object.
#' @param previous_gemeinde Reactive value storing the previously selected name.
#' @param geo_data Reactive sf dataset.
#' @param geo_data_geojson Reactive GeoJSON string (unused here but kept for API symmetry).
zoom_and_zoom_reset <- function(session, input, previous_gemeinde, geo_data, geo_data_geojson) {
  observeEvent(input$bfs_nr_gemeinde, {
    old_bfsnr <- previous_gemeinde()

    # Resolve bfsnr → name for echarts4r dispatch
    resolve_name <- function(bfsnr_val) {
      row <- as.data.frame(geo_data()) %>% filter(bfsnr == bfsnr_val)
      if (nrow(row) > 0) row$name[1] else NULL
    }

    if (!is.null(old_bfsnr) && old_bfsnr != "") {
      old_name <- resolve_name(old_bfsnr)
      if (!is.null(old_name)) {
        echarts4r::echarts4rProxy(session$ns("map")) %>%
          echarts4r::e_dispatch_action_p("downplay",
                                         seriesName = "Gemeinden",
                                         name       = old_name)
      }
    }

    if (!is.null(input$bfs_nr_gemeinde) && input$bfs_nr_gemeinde != "") {
      new_name <- resolve_name(input$bfs_nr_gemeinde)
      if (!is.null(new_name)) {
        echarts4r::echarts4rProxy(session$ns("map")) %>%
          echarts4r::e_dispatch_action_p("highlight",
                                         seriesName = "Gemeinden",
                                         name       = new_name)
      }
    }

    previous_gemeinde(input$bfs_nr_gemeinde)
  }, ignoreNULL = FALSE)
}




#' Update the summary filter dropdown based on the selected Gemeinde.
#'
#' @param session Shiny session object.
#' @param input Shiny input object.
#' @param selected_data Reactive dataset containing filtered results.
update_summary_filter <- function(session, input,selected_data) {

    observeEvent(list(selected_data,input$bfs_nr_gemeinde,input$filter,input$indicator,input$tab_box), {
      if (input$tab_box=="summary_tab"){
        if (input$bfs_nr_gemeinde == "") {
          updateSelectizeInput(session,
                               "summary_select",
                               choices = c("Erste 10 Gebiete", "Letzte 10 Gebiete"),
                               selected = "Erste 10 Gebiete"
          )

        } else {
          if (length(unique(selected_data()$jahr))>1){
            updateSelectizeInput(session,
                               "summary_select",
                                 choices = c("Zeitlicher Verlauf","Erste 10 Gebiete", "Letzte 10 Gebiete"),
                                 selected = "Zeitlicher Verlauf"
            )
          } else {
            updateSelectizeInput(session,
                               "summary_select",
                                 choices = c("Erste 10 Gebiete", "Letzte 10 Gebiete"),
                                 selected = "Erste 10 Gebiete"
            )
          }



        }
      }




    })

}


#' Render a summary visualization (bar or line chart) using echarts4r.
#'
#' @param session Shiny session object.
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param selected_data Reactive dataset.
#' @param check_conditions Reactive checking whether rendering is valid.
#' @param bezirk_data Data frame with `bfs_nr_gemeinde` / `name_gemeinde`.
render_hc_summary <- function(session, input, output, selected_data, check_conditions, bezirk_data) {
  observeEvent(list(input$indicator, input$filter1, input$year, input$value_type,
                    selected_data(), input$tab_box, input$summary_select), {
    req(input$indicator, input$filter1, input$year, input$value_type, selected_data())
    if (input$tab_box != "summary_tab") return()

    df <- selected_data()
    if ("filter1" %in% colnames(df) && input$filter1 != "Kein Filter") {
      df <- df %>% filter(filter1 == input$filter1)
    }

    if (input$summary_select == "Erste 10 Gebiete") {
      df <- df %>%
        filter(jahr == input$year) %>%
        left_join(bezirk_data, by = "bfs_nr_gemeinde") %>%
        arrange(desc(value)) %>%
        slice(1:10)

      output$summary_graph <- echarts4r::renderEcharts4r({
        df %>%
          echarts4r::e_charts(name_gemeinde) %>%
          echarts4r::e_bar(value, name = "Wert", color = "#185FA5") %>%
          echarts4r::e_flip_coords() %>%
          echarts4r::e_title(input$indicator, "Erste 10 Gebiete") %>%
          echarts4r::e_tooltip(trigger = "axis") %>%
          echarts4r::e_toolbox_feature("saveAsImage")
      })

    } else if (input$summary_select == "Letzte 10 Gebiete") {
      df <- df %>%
        filter(jahr == input$year) %>%
        left_join(bezirk_data, by = "bfs_nr_gemeinde") %>%
        arrange(value) %>%
        slice(1:10)

      output$summary_graph <- echarts4r::renderEcharts4r({
        df %>%
          echarts4r::e_charts(name_gemeinde) %>%
          echarts4r::e_bar(value, name = "Wert", color = "#BA7517") %>%
          echarts4r::e_flip_coords() %>%
          echarts4r::e_title(input$indicator, "Letzte 10 Gebiete") %>%
          echarts4r::e_tooltip(trigger = "axis") %>%
          echarts4r::e_toolbox_feature("saveAsImage")
      })

    } else if (input$summary_select == "Zeitlicher Verlauf" &&
               !is.null(input$bfs_nr_gemeinde) && input$bfs_nr_gemeinde != "") {
      df <- df %>%
        filter(bfs_nr_gemeinde == input$bfs_nr_gemeinde) %>%
        left_join(bezirk_data, by = "bfs_nr_gemeinde") %>%
        mutate(jahr = as.character(jahr)) %>%
        arrange(jahr)

      output$summary_graph <- echarts4r::renderEcharts4r({
        df %>%
          echarts4r::e_charts(jahr) %>%
          echarts4r::e_line(value, name = "Wert", color = "#185FA5") %>%
          echarts4r::e_title(input$indicator, unique(df$name_gemeinde)[1]) %>%
          echarts4r::e_tooltip(trigger = "axis") %>%
          echarts4r::e_toolbox_feature("saveAsImage")
      })
    }
  })
}


