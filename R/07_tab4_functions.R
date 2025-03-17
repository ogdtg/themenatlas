#' 07_tab4_functions.R
#'
#' This script provides functions for visualizing user-uploaded data on a map. It supports file uploads,
#' column selection for joining data, and dynamic updates to a leaflet map with custom visualizations.
#'
#' Functionality:
#' - Allows users to upload CSV or Excel files.
#' - Enables selection of join and visualization columns dynamically.
#' - Merges user-uploaded data with geographic data.
#' - Updates a leaflet map with numeric or categorical data.
#' - Includes error handling for incorrect selections.
#'
#' Dependencies:
#' - `shiny`, `dplyr`, `leaflet`, `sf`, `DT`, `openxlsx`
#'
#' Output:
#' - A leaflet map with dynamically colored polygons.
#' - A data table displaying the uploaded dataset.
#' - UI elements for selecting join and visualization columns.


#' Upload and process external data for visualization
#'
#' This function handles user file uploads, reads CSV or Excel files, and extracts relevant columns
#' for joining with geographic data.
#'
#' @param session The Shiny session object.
#' @param input The Shiny input object.
#' @param output The Shiny output object.
#' @param uploaded_data A reactive value storing the uploaded dataset.
upload_external_data <- function(session,input,output,uploaded_data){
  observeEvent(input$upload_data, {
    req(input$upload_data)

    ext <- tools::file_ext(input$upload_data$name)
    file_path <- input$upload_data$datapath

    if (ext == "csv") {
      df <- read.csv(file_path, stringsAsFactors = FALSE)
    } else if (ext %in% c("xls", "xlsx")) {
      df <- openxlsx::read.xlsx(file_path)
    } else {
      showNotification("Ungültiges Dateiformat. Bitte laden Sie eine CSV oder Excel-Datei hoch.", type = "error")
      return(NULL)
    }

    uploaded_data(df)

    # Update select input for join column dynamically
    updateSelectInput(session, "join_column", choices = names(df))

    # Identify numeric columns for visualization selection
    numeric_cols <- names(df)[sapply(df, is.numeric)]
    updateSelectInput(session, "visualize_column", choices = numeric_cols)
  })

  # Render Uploaded Data Table
  output$uploaded_data_table <- renderDT({
    req(uploaded_data())
    datatable(uploaded_data(), options = list(pageLength = 10))
  })

  # UI for selecting join and visualization columns
  output$select_join_col <- renderUI({
    req(uploaded_data())
    selectInput(session$ns("join_column"), "Gemeinde Join-Spalte wählen:", choices = names(uploaded_data()), selected = NULL)
  })

  output$select_vis_col <- renderUI({
    # req(uploaded_data(),input$join_col)
    # vis_cols <- names(uploaded_data())[names(uploaded_data())!=input$join_col]
    selectInput(session$ns("vis_col"), "Variable visualisieren:", choices = names(uploaded_data()), selected = NULL)
  })
}


#' Generate an Excel template for user uploads
#'
#' This function creates an Excel template with municipality codes and names, allowing users to add
#' their own data for visualization.
#'
#' @param output The Shiny output object.
#' @param bezirk_data A dataset containing municipality information.
generate_xlsx_pattern <- function(output,bezirk_data){
  output$download_template <- downloadHandler(
    filename = function() {
      "data_template.xlsx"
    },
    content = function(file) {
      template_data <- bezirk_data %>%
        select(bfs_nr_gemeinde,name_gemeinde) %>%
        mutate(ihre_daten = NA)
      write.xlsx(template_data, file)
    }
  )
}



#' Draw the base map with municipality boundaries
#'
#' This function initializes a leaflet map displaying municipality boundaries with tooltips.
#'
#' @param output The Shiny output object.
#' @param geo_data A spatial dataset containing geographic boundaries.
draw_base_map <- function(output,geo_data){
  output$uploaded_map <- renderLeaflet({
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



#' Update the map with user-uploaded data
#'
#' This function merges uploaded data with geographic data and updates the leaflet map
#' with color-coded visualizations based on selected variables.
#'
#' @param session The Shiny session object.
#' @param input The Shiny input object.
#' @param geo_data A spatial dataset containing geographic boundaries.
#' @param uploaded_data A reactive value storing the uploaded dataset.
update_map_with_custom_data <- function(session,input,geo_data,uploaded_data){
  observeEvent(input$process_data, {
    req(uploaded_data(), input$join_column, input$vis_col)


    if (input$join_column==input$vis_col){
      showModal(
        modalDialog(
          title = "Fehlermeldung",
          "Join-Spalte und Visualisierungsvariable dürfen nicht identisch sein",
          easyClose = TRUE,
          footer = modalButton("OK")
        )
      )
    } else {
      # Merge uploaded data with geo data
      merged_data <- geo_data %>%
        left_join(uploaded_data(), by = setNames(input$join_column, "bfsnr"))


      if (input$var_type == "Numerisch") {
        data_col <- suppressWarnings(as.numeric(merged_data[[input$vis_col]]))

        if (all(is.na(data_col))){
          showModal(
            modalDialog(
              title = "Fehlermeldung",
              paste0(input$vis_col, " kann nicht numerisch dargestellt werden"),
              easyClose = TRUE,
              footer = modalButton("OK")
            )
          )
          return(NULL)
        }

        pal <- colorNumeric("Blues", data_col, na.color = "transparent")
      } else {
        data_col <- merged_data[[input$vis_col]]
        pal <- colorFactor("Dark2", data_col, na.color = "transparent")
      }


      # print(pal(data_col))

      tooltips <- paste0(
        "<b>", merged_data$name, "</b><br>",
        "<b>",input$vis_col,":</b> ", data_col, "<br>")


      # ✅ Update polygon fill color
      leafletProxy(session$ns("uploaded_map"), data = geo_data) %>%
        setShapeStyle(
          layerId = ~bfsnr,
          fillColor = pal(data_col)
        ) %>%
        clearControls() %>%
        addLegend(
          position = "bottomright",
          pal = pal,
          values = data_col,
          title = input$vis_col
        ) %>%
        setShapeLabel(layerId = ~bfsnr, label = tooltips)

    }



  })
}


#' Create UI elements for selecting the variable type
#'
#' This function dynamically generates radio buttons for choosing between numerical and categorical
#' visualization types based on user selection.
#'
#' @param session The Shiny session object.
#' @param input The Shiny input object.
#' @param output The Shiny output object.
create_var_type_radio_buttons <- function(session,input,output){
  output$variable_type <- renderUI({
    req(input$vis_col)
    radioButtons(session$ns("var_type"), "Art der Variable:", choices = c("Numerisch", "Text"), inline = TRUE)
  })
}

