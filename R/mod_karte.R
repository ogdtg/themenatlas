#' karte UI Function (Tab 1 – interactive choropleth map)
#'
#' @param id Internal module id.
#' @noRd
#' @importFrom bsicons bs_icon
mod_karte_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    "Karte",
    icon = bs_icon("map"),
    layout_sidebar(
      fillable = TRUE,
      sidebar = sidebar(
        width = 300,
        open = "desktop",
        title = "Optionen",
        selectizeInput(ns("area"), "Gebietseinheit",
          choices = c("Bezirk", "Gemeinde", "Primarschulgemeinde",
                      "Volksschulgemeinde", "Sekundarschulgemeinde"),
          selected = "Gemeinde"),
        selectizeInput(ns("topic"),     "Themenbereich", choices = NULL),
        selectizeInput(ns("subtopic"),  "Thema",         choices = NULL),
        selectizeInput(ns("indicator"), "Indikator",     choices = NULL),
        uiOutput(ns("filter_ui")),
        selectizeInput(ns("bfs_nr_gemeinde"), "Gemeinde", choices = NULL, selected = NULL),
        selectizeInput(ns("year"), "Jahr", choices = NULL),
        uiOutput(ns("radio_bas_perc")),
        actionButton(ns("draw_map"), "Anzeigen", class = "btn-primary w-100 mt-2")
      ),
      card(
        full_screen = TRUE,
        card_header(uiOutput(ns("indicator_title"))),
        navset_card_tab(
          id = ns("tab_box"),
          nav_panel(
            "Karte", value = "map_tab",
            echarts4r::echarts4rOutput(ns("map"), height = "600px")
          ),
          nav_panel(
            "Tabelle", value = "table_tab",
            DTOutput(ns("data_table"))
          ),
          nav_panel(
            "Zusammenfassung", value = "summary_tab",
            selectizeInput(ns("summary_select"), "Diagrammtyp",
                           choices = NULL, selected = NULL),
            echarts4r::echarts4rOutput(ns("summary_graph"), height = "450px")
          )
        )
      )
    )
  )
}

#' karte Server Functions
#'
#' @param id Internal module id.
#' @noRd
mod_karte_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # --- Reactive state -----------------------------------------------------
    previous_gemeinde   <- reactiveVal(NULL)
    prevYear            <- reactiveVal("")
    prevFilter          <- reactiveVal(NULL)
    prevArea            <- reactiveVal("")
    prevIndicator       <- reactiveVal("")
    prevValueType       <- reactiveVal(NULL)
    geo_data            <- reactiveVal(gemeindegrenzen)
    geo_data_geojson    <- reactiveVal(geojson_gemeindegrenzen)
    area_data           <- reactiveVal(nested_list)
    selected_data       <- reactiveVal(NULL)
    area_names          <- reactiveVal(NULL)
    counter             <- reactiveVal(0)

    check_conditions <- reactive({
      req(selected_data())
      check_conditions_func(input, selected_data, prevYear, prevValueType,
                            prevIndicator, prevFilter, prevArea)
    })

    # --- Switch area type ---------------------------------------------------
    observeEvent(input$area, {
      if (input$area == "Gemeinde") {
        geo_data(gemeindegrenzen)
        geo_data_geojson(geojson_gemeindegrenzen)
        area_data(nested_list)
      } else if (input$area == "Bezirk") {
        if (!exists("bezirksgrenzen")) bezirksgrenzen <- readRDS("data/bezirksgrenzen.rds")
        geo_data(bezirksgrenzen)
        geo_data_geojson(geojson_bezirksgrenzen)
        area_data(nested_list)
      } else if (input$area == "Primarschulgemeinde") {
        if (!exists("psg")) psg <- readRDS("data/psg.rds")
        geo_data(psg)
        geo_data_geojson(geojson_psg)
        area_data(psg_list)
      } else if (input$area == "Sekundarschulgemeinde") {
        if (!exists("ssg")) ssg <- readRDS("data/ssg.rds")
        geo_data(ssg)
        geo_data_geojson(geojson_ssg)
        area_data(ssg_list)
      } else if (input$area == "Volksschulgemeinde") {
        if (!exists("vsg")) vsg <- readRDS("data/vsg.rds")
        geo_data(vsg)
        geo_data_geojson(geojson_vsg)
        area_data(vsg_list)
      }

      area_names(area_names_data %>% filter(area_type == input$area))

      topic_temp <- names(area_data())[1]
      updateSelectizeInput(session, "topic",
                           choices = names(area_data()), selected = NULL)

      subtopic_temp <- names(area_data()[[topic_temp]])[1]
      updateSelectizeInput(session, "subtopic",
                           choices = names(area_data()[[topic_temp]]),
                           selected = subtopic_temp)

      indicator_temp <- names(area_data()[[topic_temp]][[subtopic_temp]])[1]
      updateSelectizeInput(session, "indicator",
                           choices = names(area_data()[[topic_temp]][[subtopic_temp]]),
                           selected = indicator_temp)
    }, ignoreInit = FALSE)

    # --- Selected dataset ---------------------------------------------------
    init_selected_data(input, area_data, selected_data)

    observeEvent(input$topic, {
      req(input$topic, area_data())
      topic_data <- area_data()[[input$topic]]
      if (!is.null(topic_data)) {
        updateSelectizeInput(session, "subtopic",
                             choices = names(topic_data),
                             selected = names(topic_data)[1])
      }
    })

    observeEvent(input$subtopic, {
      req(input$topic, input$subtopic, area_data())
      subtopic_data <- area_data()[[input$topic]][[input$subtopic]]
      if (!is.null(subtopic_data)) {
        updateSelectizeInput(session, "indicator",
                             choices = names(subtopic_data),
                             selected = names(subtopic_data)[1])
      }
    })

    # --- Dynamic selection UIs ---------------------------------------------
    render_selections_dynamic(session, input, output, selected_data, area_names)
    render_filter_ui(session, input, output, selected_data)
    update_year_on_filter(session, input, output, selected_data)

    # --- Base map -----------------------------------------------------------
    init_map(output, input, geo_data, geo_data_geojson)

    all_inputs_ready <- reactive({
      if (check_all_filters(input, selected_data)) {
        list(
          data       = selected_data(),
          year       = input$year,
          filter1    = input$filter1,
          value_type = input$value_type,
          area       = input$area
        )
      }
    })
    debounced_inputs <- debounce(all_inputs_ready, millis = 300)

    modify_map_and_table(
      session                = session,
      input                  = input,
      output                 = output,
      selected_data          = selected_data,
      geo_data               = geo_data,
      geo_data_geojson       = geo_data_geojson,
      palette_ds             = palette_ds,
      palette_ds_alternative = palette_ds_alternative,
      check_all_filters      = check_all_filters,
      debounced_inputs       = debounced_inputs,
      counter                = counter,
      area_names             = area_names
    )

    # --- Interaction --------------------------------------------------------
    update_gemeinde_selection_on_click(session, input, geo_data)
    zoom_and_zoom_reset(session, input, previous_gemeinde, geo_data, geo_data_geojson)
    update_summary_filter(session, input, selected_data)
    render_hc_summary(session, input, output, selected_data, check_conditions, bezirk_data)

    # --- Indicator title ----------------------------------------------------
    output$indicator_title <- renderUI({
      req(input$indicator)
      tags$b(input$indicator)
    })
  })
}
