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



#' Prepare a color-coded geographic map based on selected data
#'
#' @param df Dataframe containing the dataset to visualize.
#' @param geo_data Spatial dataframe containing geographic information.
#' @param input List of user inputs from the Shiny app.
#' @param palette_ds Color palette for mapping.
#' @param palette_ds_alternative Alternative palette for cases with negative values.
#' @return A list with a color-mapped dataframe and a color palette.
prepare_color_map <- function(df, geo_data, input, palette_ds, palette_ds_alternative) {
  # Apply filters
  if (!is.null(input$year)) df <- df %>% filter(jahr %in% input$year)
  if (input$filter1!="Kein Filter" && "filter1" %in% colnames(df)) df <- df %>% filter(filter1 %in% input$filter1)
  if (nrow(df) > 80) req(input$filter1)


  # Merge geo_data with df
  color_map <- geo_data %>% left_join(df, by = c("bfsnr" = "bfs_nr_gemeinde"))
  color_map$value <- as.numeric(color_map$value)

  if (sum(is.na(color_map$value))==length(color_map$value)){
    pal <- colorFactor(palette = "grey",domain =c(NA))
    color_map$category <- "Keine Werte"
    return(list(color_map = color_map, pal = pal))
  }

  threshold <- NULL
  palette <- palette_ds

  #Bei negativen Werten
  if (min(color_map$value,na.rm = T)<0){
    threshold <- 0
    palette <- palette_ds_alternative
  }

  # Bei Abstimmungen
  if (input$filter1!="Kein Filter"){
    if (input$filter1=="Ja-Stimmenanteil" & input$topic == "Staat und Politik"){ #TODO sauberer lösen
      threshold <- 50
      palette <- palette_ds_alternative
    }

  }

  # Determine value type
  percentage <- ""
  if (!is.null(input$value_type) && input$value_type == "Prozentual" && "share" %in% names(color_map)) {
    color_map$value <- color_map$share
    percentage <- "%"
  }

  # Blaue Palette wenn kein Threshold definiert ist
  if (is.null(threshold)){
    # Define quantile-based bins
    bins <- unique(quantile(color_map$value, probs = seq(0, 1, length.out = 7), na.rm = TRUE, type = 7))
    # bins <- seq(min(color_map$value, na.rm = TRUE), max(color_map$value, na.rm = TRUE), length.out = 7)

    bin_labels <- paste0(round(head(bins, -1), 2), percentage, " bis ", round(tail(bins, -1), 2), percentage)
    # Assign each value to a labeled category
    color_map$category <- cut(color_map$value, breaks = bins, include.lowest = TRUE, labels = bin_labels)

    # Create a categorical color palette
    pal <- colorFactor(palette, domain = color_map$category)
  } else {
    # Separate positive and negative values
    pos_values <- color_map$value[color_map$value > threshold & !is.na(color_map$value)]
    neg_values <- color_map$value[color_map$value < threshold & !is.na(color_map$value)]

    # Nur Werte unter threshold
    if (length(neg_values)>0 & length(pos_values)==0){
      bins <- unique(quantile(color_map$value, probs = seq(0, 1, length.out = 4), na.rm = TRUE, type = 7))
      bin_labels <- paste0(round(head(bins, -1), 2), percentage, " bis ", round(tail(bins, -1), 2), percentage)
      color_map$category <- cut(color_map$value, breaks = bins, include.lowest = TRUE, labels = bin_labels)
      pal <- colorFactor(palette[1:3], domain = color_map$category)

    }

    # Nur werte über threshold
    if (length(neg_values)==0 & length(pos_values)>0){
      bins <- unique(quantile(color_map$value, probs = seq(0, 1, length.out = 4), na.rm = TRUE, type = 7))
      bin_labels <- paste0(round(head(bins, -1), 2), percentage, " bis ", round(tail(bins, -1), 2), percentage)
      color_map$category <- cut(color_map$value, breaks = bins, include.lowest = TRUE, labels = bin_labels)
      pal <- colorFactor(palette[4:6], domain = color_map$category)

    }

    # Beides vorhanden
    if (length(neg_values)>0 & length(pos_values)>0){
      # Define quantiles separately for positive and negative values
      if (length(pos_values) > 0) {
        bins_positive <- unique(c(threshold, quantile(pos_values, probs = seq(0, 1, length.out = 3), na.rm = TRUE, type = 7)))
        labels_positive <- paste0(round(head(bins_positive, -1), 2), percentage, " bis ", round(tail(bins_positive, -1), 2), percentage)
      }

      if (length(neg_values) > 0) {
        bins_negative <- unique(c(quantile(neg_values, probs = seq(0, 1, length.out = 3), na.rm = TRUE, type = 7),threshold))
        labels_negative <- paste0(round(head(bins_negative, -1), 2), percentage, " bis ", round(tail(bins_negative, -1), 2), percentage)
      }

      # Assign categories to values
      color_map$category <- NA
      color_map$num <- NA
      neg_colors <- NULL
      pos_colors <- NULL
      if (length(neg_values) > 0) {

        neg_cat_values <- cut(color_map$value[color_map$value < threshold & !is.na(color_map$value)],
                              breaks = bins_negative,
                              include.lowest = TRUE,
                              labels = labels_negative)


        color_map$category[color_map$value < threshold & !is.na(color_map$value)] <- neg_cat_values %>% as.character()

        color_map$num[color_map$value < threshold & !is.na(color_map$value)] <- neg_cat_values %>% as.numeric()

        neg_palette_index <- length(levels(neg_cat_values))

        neg_colors = palette[1:neg_palette_index]
      }
      if (length(pos_values) > 0) {


        pos_cat_values <- cut(color_map$value[color_map$value > threshold & !is.na(color_map$value)],
                              breaks = bins_positive,
                              include.lowest = TRUE,
                              labels = labels_positive)

        start_val_neg <- 3
        if (length(neg_values)>0)  start_val_neg <- max(color_map$num,na.rm = T)



        color_map$category[which(color_map$value > threshold & !is.na(color_map$value))] <- pos_cat_values %>% as.character()
        color_map$num[color_map$value > threshold & !is.na(color_map$value)] <- pos_cat_values %>% as.numeric() +start_val_neg

        pos_palette_index <- length(unique(pos_cat_values %>% as.numeric() +start_val_neg ))+3

        pos_colors = palette[4:pos_palette_index]


      }
      # Zu Factor
      color_map <- color_map %>%
        mutate(category = factor(category, levels = unique(category[order(num)]), ordered = TRUE)) %>%
        select(-num)



      # Define color mapping: Blue shades for positive, Orange shades for negative
      color_palette <- c(neg_colors,pos_colors)
      pal <- colorFactor(color_palette, domain = color_map$category)
    }

  }


  # Construct tooltip text
  color_map$tooltip_text <- paste0(
    "<b>", color_map$name, "</b><br>",
    "<b>Indicator:</b> ", input$indicator, "<br>",
    if (input$filter1!="Kein Filter") paste0("<b>Filter:</b> ", input$filter1, "<br>") else "",
    "<b>Year:</b> ", color_map$jahr, "<br>",
    "<b>Value:</b> ", round(color_map$value, 2)
  )

  return(list(color_map = color_map, pal = pal))
}




#' Check if input conditions allow data rendering
#'
#' @param input List of user inputs.
#' @param selected_data Reactive function returning the dataset.
#' @param prevYear, prevValueType, prevIndicator, prevFilter Previous user selections.
#' @return Logical value indicating whether the data should be updated.
check_conditions_func <- function(input, selected_data,prevYear,prevValueType,prevIndicator,prevFilter) {
  df <- selected_data()



  filter_is_null <- is.null(input$filter1)
  value_type_is_null <- is.null(input$value_type)
  has_filter <- "filter1" %in% colnames(df)
  has_share <- "share" %in% colnames(df)
  contains_year <- input$year %in% df$jahr
  # Wenn es eine filter1 Spalte gibt aber kein Filter gewählt ist

  if (filter_is_null) {
    print("filter is null")
    return(FALSE)
  }
  if (input$filter1 == "Kein Filter" & has_filter) {
    print("kein filter aber hat filter spalte")
    return(FALSE)
  }

  # Wenn es einen Filter gibt, aber keine Spalte zum filtern
  if (input$filter1 != "Kein Filter" & !has_filter) {
    print("filter aber hat keine filter spalte")

    return(FALSE)
  }


  # Wenn es keine share Spalte gibt aber ein value_type gewählt ist

  if (input$value_type == "Prozentual" & !has_share) {
    print("keine value_type  aber share Spalte")
    return(FALSE)
  }

  # Wenn das Jahr nicht im datensatz vorhanden ist
  if (!contains_year) {
    print("Jahr nicht im datensatz vorhanden")

    return(FALSE)
  }

  # Wenn es einen korrekten Filter gibt, dessen Wert aber nicht in der filter1 Spalte verfügbar ist
  if (input$filter1 != "Kein Filter") {
    contains_filter <- input$filter1 %in% df$filter1
    if (!contains_filter) {
      print(" korrekten Filter gibt, dessen Wert aber nicht in der filter1 Spalte verfügbar")

      return(FALSE)
    }
    # Wenn es einen korrekten Filter gibt, es für die Kombi aus Jahr und filter aber keine Daten gibt
    df_filtered <- df %>% filter(jahr == input$year &
                                   filter1 == input$filter1)
    num_rows <-  df_filtered %>% nrow()
    if (num_rows == 0) {
      print(" korrekten Filter gibt, für die Kombi aus Jahr und filter aber keine Daten")

      return(FALSE)
    }
    if (sum(is.na(df_filtered$value)) == num_rows) {
      print("ungleiche reihen")

      return(FALSE)
    }
  }

  if (
    isTRUE(identical(prevYear(), input$year)) &&
    isTRUE(identical(prevValueType(), input$value_type)) &&
    isTRUE(identical(prevIndicator(), input$indicator)) &&
    isTRUE(identical(prevFilter(), input$filter1))
  ) {
    print("Nichts verändert")
    return(FALSE)
  } else {
    prevYear(input$year)
    prevValueType(input$value_type)
    prevIndicator(input$indicator)
    prevFilter(input$filter1)

    # print("Change")
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
#' @param selected_data Reactive function returning the dataset.
render_selections_dynamic <- function(session,input,output,selected_data){

  observeEvent(selected_data(), {
    df <- selected_data()



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
    updateSelectizeInput(session, session$ns("year"),
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


#' Initialize the Leaflet map with default settings.
#'
#' @param output Shiny output object for rendering the map.
#' @param geo_data Spatial dataset containing geometry and tooltips.
#' @return A Leaflet map rendered in the Shiny UI.
init_map <- function(output,geo_data){
  output$map <- renderLeaflet({
    # Construct initial tooltips
    geo_data$tooltip_text <- paste0(
      "<b>", geo_data$name, "</b>"
    )

    leaflet(geo_data) %>%
      addTiles(
        options = providerTileOptions(minZoom = 9)
      ) %>%
      # addProviderTiles(providers$Stadia.StamenToner) %>%
      addPolygons(
        layerId = ~bfsnr,
        fillColor = "grey",
        color = "white",
        weight = 1.5,
        opacity = .5,
        fillOpacity = 1,
        label = lapply(geo_data$tooltip_text, HTML)  # ✅ Initial tooltip
      ) %>%
      setView(
        lng = mean(st_coordinates(geo_data)[,1]),
        lat = mean(st_coordinates(geo_data)[,2]),
        zoom = 10
      )

  })

}





#' Modify the Leaflet map based on user-selected filters and data.
#'
#' @param id Shiny module ID.
#' @param session Shiny session object.
#' @param input Shiny input object.
#' @param selected_data Reactive dataset containing filtered data.
#' @param geo_data Spatial dataset with polygon geometries.
#' @param palette_ds Color palette function for mapping categories.
#' @param palette_ds_alternative Alternative color palette.
#' @param check_conditions Function to check if update conditions are met.
modify_map <- function(id,session, input,selected_data, geo_data, palette_ds, palette_ds_alternative, check_conditions) {


    observeEvent(list(input$indicator, input$filter1, input$year, input$value_type, selected_data()), {
      req(input$indicator, input$filter1, input$year, input$value_type, selected_data())

      if (check_conditions()) {
        result <- prepare_color_map(selected_data(), geo_data, input, palette_ds, palette_ds_alternative)

        # ✅ Update the reactive color_map inside the function
        color_map <- result$color_map
        pal <- result$pal

        # ✅ Update polygon fill color
        leafletProxy(session$ns("map"), data = geo_data) %>%
          setShapeStyle(
            layerId = ~bfsnr,
            fillColor = pal(color_map$category)
          ) %>%
          clearControls() %>%
          addLegend(
            position = "bottomright",
            pal = pal,
            values = color_map$category,
            title = input$indicator
          ) %>%
          setShapeLabel(layerId = ~bfsnr, label = color_map$tooltip_text)
      }
    })
}


#' Update and render the data table based on selected filters.
#'
#' @param session Shiny session object.
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param selected_data Reactive dataset filtered based on user inputs.
#' @param check_conditions Function to verify if table update should proceed.
modify_table <- function(session,input,output,selected_data,check_conditions){



  observeEvent(list(input$indicator, input$filter1, input$year, input$value_type, selected_data()), {
    req(input$indicator, input$filter1, input$year, input$value_type, selected_data())
    output$data_table <- renderDT({
      df <- selected_data()

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
          left_join(bezirk_data, "bfs_nr_gemeinde") %>%
          select(bfs_nr_gemeinde, name_gemeinde, filter1, value) %>%
          mutate(value = round(value, 2)) %>%
          arrange(desc(value))
        col_labels = c("BFS Nr.", "Gemeinde", "Filter", value_name)

      } else {
        df <- df %>%
          left_join(bezirk_data, "bfs_nr_gemeinde") %>%
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


  })

}


#' Update the selection when a polygon is clicked on the map.
#'
#' @param session Shiny session object.
#' @param input Shiny input object containing click events.
update_gemeinde_selection_on_click <- function(session,input){
  observeEvent(input$map_shape_click, {
    clicked_bfsnr <- input$map_shape_click$id  # Get clicked polygon ID

    # ✅ Set selected BFS number
    updateSelectizeInput(session, session$ns("bfs_nr_gemeinde"), selected = clicked_bfsnr)

    # ✅ Highlight selected polygon
    leafletProxy(session$ns("map")) %>%
      setShapeStyle(layerId = as.character(clicked_bfsnr), weight = 3, color = "red")
  })
}


#' Control zoom behavior based on Gemeinde selection.
#'
#' @param session Shiny session object.
#' @param input Shiny input object.
#' @param previous_gemeinde Reactive value storing the previously selected Gemeinde.
#' @param geo_data Spatial dataset for map rendering.
zoom_and_zoom_reset <- function(session,input,previous_gemeinde,geo_data){
  observeEvent(input$bfs_nr_gemeinde, {

    # Get the previously selected Gemeinde
    old_gemeinde <- previous_gemeinde()

    if (is.null(input$bfs_nr_gemeinde) || input$bfs_nr_gemeinde == "") {
      # ✅ If no Gemeinde is selected, reset to original zoom & reset all borders
      leafletProxy(session$ns("map")) %>%
        setView(
          lng = mean(st_coordinates(geo_data)[,1]),
          lat = mean(st_coordinates(geo_data)[,2]),
          zoom = 10
        ) %>%
        setShapeStyle(
          layerId = geo_data$bfsnr,
          weight = 1,  # Reset border thickness
          opacity = .5,
          color = "white"
        )
    } else {
      # ✅ Reset the old Gemeinde's border before selecting the new one
      if (!is.null(old_gemeinde)) {
        leafletProxy(session$ns("map")) %>%
          setShapeStyle(
            layerId = as.character(old_gemeinde),
            weight = 1,  # Reset border thickness
            opacity = .5,
            color = "white"
          )
      }

      # ✅ Zoom to selected Gemeinde and highlight it
      geom <- geo_data %>% filter(bfsnr == input$bfs_nr_gemeinde)
      if (nrow(geom) > 0) {
        bbox <- st_bbox(geom)
        leafletProxy(session$ns("map")) %>%
          fitBounds(
            lng1 = as.numeric(bbox["xmin"]),
            lat1 = as.numeric(bbox["ymin"]),
            lng2 = as.numeric(bbox["xmax"]),
            lat2 = as.numeric(bbox["ymax"])
          ) %>%
          setShapeStyle(
            layerId = as.character(input$bfs_nr_gemeinde),
            weight = 3,
            opacity = 1,
            color = "red"
          )
      }
    }

    # ✅ Update the stored previous Gemeinde
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
        print(input$bfs_nr_gemeinde)
        if (input$bfs_nr_gemeinde == "") {
          updateSelectizeInput(session,
                               session$ns("summary_select"),
                               choices = c("Erste 10 Gebiete", "Letzte 10 Gebiete"),
                               selected = "Erste 10 Gebiete"
          )

        } else {
          if (length(unique(selected_data()$jahr))>1){
            updateSelectizeInput(session,
                                 session$ns("summary_select"),
                                 choices = c("Zeitlicher Verlauf","Erste 10 Gebiete", "Letzte 10 Gebiete"),
                                 selected = "Zeitlicher Verlauf"
            )
          } else {
            updateSelectizeInput(session,
                                 session$ns("summary_select"),
                                 choices = c("Erste 10 Gebiete", "Letzte 10 Gebiete"),
                                 selected = "Erste 10 Gebiete"
            )
          }



        }
      }




    })

}


#' Render a summary visualization (bar chart or time series) using highcharter.
#'
#' @param session Shiny session object.
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param selected_data Reactive dataset containing user-selected data.
#' @param check_conditions Function to verify if chart update should proceed.
#' @param bezirk_data Dataset containing administrative region mappings.
render_hc_summary <- function(session, input, output, selected_data, check_conditions, bezirk_data) {
  observeEvent(list(input$indicator, input$filter1, input$year, input$value_type, selected_data(),input$tab_box,input$summary_select), {
    req(input$indicator, input$filter1, input$year, input$value_type, selected_data())

    if (input$tab_box=="summary_tab"){
      df <- selected_data()

      print(input$summary_select)
      # ✅ Apply filtering logic if "filter1" exists
      if ("filter1" %in% colnames(df) && input$filter1 != "Kein Filter") {
        df <- df %>% filter(filter1 == input$filter1)
      }

      # ✅ Determine chart type and filter accordingly
      if (input$summary_select == "Erste 10 Gebiete") {
        df <- df %>%
          filter(jahr == input$year) %>%
          arrange(desc(value)) %>%
          slice(1:10)
        chart_title <- "Erste 10 Gebiete"
        x_axis_categories <- df$name_gemeinde
      }
      else if (input$summary_select == "Zeitlicher Verlauf" & input$bfs_nr_gemeinde != "") {
        df <- df %>%
          filter(bfs_nr_gemeinde == input$bfs_nr_gemeinde) %>%
          arrange(jahr)
        chart_title <- "Zeitlicher Verlauf"
        x_axis_categories <- df$jahr
      }
      else if (input$summary_select == "Letzte 10 Gebiete") {
        df <- df %>%
          filter(jahr == input$year) %>%
          arrange(value) %>%
          slice(1:10)
        chart_title <- "Letzte 10 Gebiete"
        x_axis_categories <- df$name_gemeinde
      }
      else {
        return()  # ✅ Exit function if none of the conditions match
      }

      # ✅ Join with bezirk_data at the end
      df <- df %>% left_join(bezirk_data, by = "bfs_nr_gemeinde")

      # ✅ Render highchart (only once)
      output$summary_graph <- renderHighchart({
        highchart() %>%
          hc_chart(type = "bar") %>%
          hc_title(text = chart_title) %>%
          hc_xAxis(categories = x_axis_categories, title = list(text = "Gemeinde")) %>%
          hc_yAxis(title = list(text = "Wert")) %>%
          hc_add_series(name = "Wert", data = df$value, colorByPoint = TRUE) %>%
          hc_tooltip(pointFormat = "Wert: {point.y}")
      })
    }

  })
}


