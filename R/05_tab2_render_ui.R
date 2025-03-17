#' 05_tab2_render_ui.R
#'
#' This script dynamically generates and renders UI elements for the report tab based on the structure_list.
#' It provides layouts for different types of content boxes, including charts, tables, and indicator tables.
#'
#' Functionality:
#' - Dynamically updates the selected comparison area.
#' - Generates UI components for single and double charts, tables, and indicator tables.
#' - Renders UI elements in a structured layout based on user selections.
#'
#' Dependencies:
#' - `shiny`, `dplyr`
#'
#' Output:
#' - UI components for charts and tables in the Shiny report tab.
#' - Dropdown menus for selecting years and comparison areas.



#' Update the selected comparison area
#'
#' This function ensures that the selected comparison area is valid and updates it dynamically.
#'
#' @param session The Shiny session object.
#' @param input The Shiny input object.
#' @param bezirk_data Data frame containing all available regions.
#' @param bezirk_data_compare Reactive value holding the comparison area data.
#' @param selected_compare_area Reactive value representing the selected comparison area.
#' @param selected_base_area Reactive value representing the selected base area.
update_compare_area <- function(session,input,bezirk_data,bezirk_data_compare,selected_compare_area,selected_base_area){
  observeEvent(input$base_area,{
    if (input$base_area %in% bezirk_data_compare()$"bfs_nr_gemeinde"){
      bezirk_data_mod <- bezirk_data %>%
        filter(bfs_nr_gemeinde != input$base_area)

      if (!selected_compare_area() %in% bezirk_data_mod$bfs_nr_gemeinde){
        selected_compare_area(bezirk_data_mod$bfs_nr_gemeinde[1])
      }

      updateSelectizeInput(session,session$ns("compare_area"), choices = setNames(bezirk_data_mod$bfs_nr_gemeinde,bezirk_data_mod$name_gemeinde),selected = selected_compare_area())
      selected_base_area(input$base_area)
      bezirk_data_compare(bezirk_data_mod)
    }
  })
}


#' Update the selected comparison area based on user input
#'
#' This function updates the `selected_compare_area` reactive value when the user selects a new area.
#'
#' @param input The Shiny input object.
#' @param selected_compare_area Reactive value representing the selected comparison area.
update_selected_compare_area <- function(input,selected_compare_area){
  observeEvent(input$compare_area,{
    selected_compare_area(input$compare_area)
  })
}


#' Create a UI component for a double-chart box
#'
#' Generates a UI box containing two side-by-side Highcharts visualizations with a data table and source links.
#'
#' @param id_num A unique numeric identifier for the box.
#' @param title The title of the box.
#' @param select_year Logical, whether a year selection dropdown should be included.
#' @param topic The topic associated with the chart.
#' @return A `fluidRow` containing the UI elements.
ui_double_chart <- function(id_num, title,select_year,topic) {

  if (select_year){
    title = div(
      HTML(paste0("<b>", title, "</b><br>")),  # Title
      uiOutput(paste0("report_chart_select_", topic,"_", id_num)),  # UI Output for selectInput
      style = "display: flex; justify-content: space-between; align-items: center;"
    )
  } else {
    title = div(
      HTML(paste0("<b>", title, "</b><br>"))
    )
  }


  fluidRow(
    box(
      title = title,
      width = 12,
      tabBox(
        width = 12,
        tabPanel(
          "Diagramm",
          value = paste0("report_chart_tab_", id_num),
          fluidRow(
            column(
              6,
              uiOutput(paste0("report_chart_ba_", topic,"_",id_num,"_title")),
              div(class = "hc-chart", highchartOutput(paste0("report_chart_ba_",topic,"_", id_num)))
            ),
            column(
              6,
              uiOutput(paste0("report_chart_ca_",topic,"_", id_num,"_title")),
              div(class = "hc-chart", highchartOutput(paste0("report_chart_ca_",topic,"_",  id_num)))
            )
          )
        ),
        tabPanel(
          "Tabelle",
          value = paste0("report_table_tab_", id_num),
          div(
            class = "dt-table",
            DTOutput(paste0("report_table_",topic,"_",  id_num)),
            style = "font-size: 75%"
          ),
          div(
            class = "dt-footer",
            downloadButton(paste0("report_table_", topic,"_", id_num,"_download"), "Download")
          )
        ),
        tabPanel(
          "Datenquellen",  # New Tab for Data Sources
          value = paste0("report_sources_tab_", id_num),
          div(class = "data-source-links", uiOutput(paste0("data_sources_", topic, "_", id_num)))
        )
      )
    )
  )
}



#' Create a UI component for a single-chart box
#'
#' Generates a UI box containing a single Highchart visualization with a data table and source links.
#'
#' @param id_num A unique numeric identifier for the box.
#' @param title The title of the box.
#' @param select_year Logical, whether a year selection dropdown should be included.
#' @param topic The topic associated with the chart.
#' @return A `fluidRow` containing the UI elements.
ui_single_chart <- function(id_num, title,select_year,topic) {

  if (select_year){
    title = div(
      HTML(paste0("<b>", title, "</b><br>")),  # Title
      uiOutput(paste0("report_chart_select_", topic,"_", id_num)),  # UI Output for selectInput
      style = "display: flex; justify-content: space-between; align-items: center;"
    )
  } else {
    title = div(
      HTML(paste0("<b>", title, "</b><br>"))
    )
  }

  fluidRow(
    box(
      title = title,  width = 12,
      tabBox(
        collapsible = FALSE, width = 12,
        tabPanel(
          "Diagramm",
          value = paste0("report_chart_tab_", id_num),
          div(class = "hc-chart", highchartOutput(paste0("report_chart_",topic,"_",  id_num)))
        ),
        tabPanel(
          "Tabelle",
          value = paste0("report_table_tab_", id_num),
          div(
            class = "dt-table",
            DTOutput(paste0("report_table_", topic,"_", id_num)),
            style = "font-size: 75%"
          ),
          div(
            class = "dt-footer",
            downloadButton(paste0("report_table_", topic,"_", id_num,"_download"), "Download")
          )
        ),
        tabPanel(
          "Datenquellen",  # New Tab for Data Sources
          value = paste0("report_sources_tab_", id_num),
          div(class = "data-source-links", uiOutput(paste0("data_sources_", topic, "_", id_num)))
        )
      )
    )
  )
}


#' Create a UI component for an indicator table
#'
#' Generates a UI box containing a data table with indicator values and source links.
#'
#' @param id_num A unique numeric identifier for the box.
#' @param title The title of the box.
#' @param select_year Logical, whether a year selection dropdown should be included.
#' @param topic The topic associated with the table.
#' @return A `fluidRow` containing the UI elements.
ui_indicator_table <- function(id_num, title,select_year,topic) {


  if (select_year){
    title = div(
      HTML(paste0("<b>", title, "</b><br>")),  # Title
      uiOutput(paste0("report_chart_select_", topic,"_", id_num)),  # UI Output for selectInput
      style = "display: flex; justify-content: space-between; align-items: center;"
    )
  } else {
    title = div(
      HTML(paste0("<b>", title, "</b><br>"))
    )
  }

  fluidRow(
    box(
      title = title,
      width=12,
      tabBox(collapsible = F,
             width = 12,
             tabPanel(
               "Tabelle",
               div(class = "dt-table", DTOutput(paste0("report_table_", topic, "_", id_num)), style = "font-size: 75%"),
               div(class = "dt-footer", downloadButton(paste0("report_table_", topic, "_", id_num, "_download"),"Download"))
             ),
             tabPanel(
               "Datenquellen",  # New Tab for Data Sources
               value = paste0("report_sources_tab_", id_num),
               div(class = "data-source-links", uiOutput(paste0("data_sources_", topic, "_", id_num)))
             )
      )

    )

    )
}



#' Render the generic report UI dynamically
#'
#' This function dynamically generates and renders report UI components based on the structure list.
#'
#' @param output The Shiny output object.
#' @param struc_list A list containing metadata for each report section.
render_generic_report2 <- function(output, struc_list) {



  output$generic_report_part <- renderUI({

    # ✅ Generate a list of `tabBox()` elements dynamically
    tab_list <- lapply(seq_along(struc_list), function(i) {

      topic <- clean_string(struc_list[[i]]$topic)
      # print(topic)
      if (str_detect(struc_list[[i]]$chart_type,"zeitreihe")){
        select_year=FALSE
      } else {
        select_year=TRUE
      }

      if (str_detect(struc_list[[i]]$chart_type,"double")){
        ui_double_chart(i, title = struc_list[[i]]$title,select_year,topic)

      } else if (str_detect(struc_list[[i]]$chart_type,"single")){
        ui_single_chart(i, title = struc_list[[i]]$title,select_year,topic)


      } else if (struc_list[[i]]$chart_type=="indicator_table"){
        ui_indicator_table(i,title = struc_list[[i]]$title,select_year,topic)
      }
    })

    do.call(tagList, tab_list)  # ✅ Combine all `fluidRow()` elements
  })
}


