# =============================================================================
# Thurgauer Themenatlas – Shiny App (bslib page_navbar UI)
# =============================================================================

# R/ files are auto-sourced by shiny::runApp()

# UI ---------------------------------------------------------------------------

ui <- page_navbar(
  id  = "nav",
  title = tags$span(bs_icon("map"), " Thurgauer Themenatlas"),
  theme = bs_theme(
    version = 5,
    primary  = "#185FA5",
    success  = "#1D9E75",
    warning  = "#BA7517",
    info     = "#1D9E75",
    danger   = "#A32D2D",
    "font-size-base" = "0.9rem"
  ),
  bg      = "#185FA5",
  inverse = TRUE,
  fillable = c("tab1", "tab2", "tab3"),

  # Global dependencies injected into <head>
  header = tagList(
    useShinyjs(),
    shinybrowser::detect(),
    tags$head(includeCSS("www/dashboard_style.css")),
    HTML('<script src="https://cdn.jsdelivr.net/npm/js-cookie@rc/dist/js.cookie.min.js"></script>'),
    leaflet_js   # defined in R/02_init_ui.R
  ),

  # Dark-mode toggle
  nav_spacer(),
  nav_item(input_dark_mode(id = "dark_mode", mode = "light")),

  # Tabs defined in R/03_ui_db_content.R
  tab_karte,
  tab_vergleich,
  tab_zeitreihe,
  tab_extern,
  tab_info
)


# Server -----------------------------------------------------------------------

server <- function(input, output, session) {

  thematic::thematic_shiny()

  ## Tab 1 – Karte --------------------------------------------------------

  previous_gemeinde   <- reactiveVal(NULL)
  bezirk_data_compare <- reactiveVal(bezirk_data_mod)
  selected_base_area  <- reactiveVal("4566")
  selected_compare_area <- reactiveVal("4671")
  geo_data     <- reactiveVal(gemeindegrenzen)
  area_data    <- reactiveVal(nested_list)
  final_state  <- reactiveVal(FALSE)
  selected_data <- reactiveVal(NULL)
  area_names   <- reactiveVal(NULL)
  counter      <- reactiveVal(0)

  check_conditions <- reactive({
    req(selected_data())
    check_conditions_func(input, selected_data, prevYear, prevValueType,
                          prevIndicator, prevFilter, prevArea)
  })

  observeEvent(input$area, {
    if (input$area == "Gemeinde") {
      geo_data(gemeindegrenzen)
      area_data(nested_list)
    } else if (input$area == "Bezirk") {
      if (!exists("bezirksgrenzen")) {
        bezirksgrenzen <- readRDS("data/bezirksgrenzen.rds")
        geo_data(bezirksgrenzen)
      } else {
        geo_data(bezirksgrenzen)
      }
      area_data(nested_list)
    } else if (input$area == "Primarschulgemeinde") {
      if (!exists("psg")) {
        psg <- readRDS("data/psg.rds")
        geo_data(psg)
      } else {
        geo_data(psg)
      }
      area_data(psg_list)
    } else if (input$area == "Sekundarschulgemeinde") {
      if (!exists("ssg")) {
        ssg <- readRDS("data/ssg.rds")
        geo_data(ssg)
      } else {
        geo_data(ssg)
      }
      area_data(ssg_list)
    } else if (input$area == "Volksschulgemeinde") {
      if (!exists("vsg")) {
        vsg <- readRDS("data/vsg.rds")
        geo_data(vsg)
      } else {
        geo_data(vsg)
      }
      area_data(vsg_list)
    }

    area_names_temp <- area_names_data %>% filter(area_type == input$area)
    area_names(area_names_temp)

    topic_temp <- names(area_data())[1]
    updateSelectizeInput(session, "topic",
                         choices  = names(area_data()),
                         selected = NULL)

    subtopic_temp <- names(area_data()[[topic_temp]])[1]
    updateSelectizeInput(session, "subtopic",
                         choices  = names(area_data()[[topic_temp]]),
                         selected = subtopic_temp)

    indicator_temp <- names(area_data()[[topic_temp]][[subtopic_temp]])[1]
    updateSelectizeInput(session, "indicator",
                         choices  = names(area_data()[[topic_temp]][[subtopic_temp]]),
                         selected = indicator_temp)
  }, ignoreInit = FALSE)

  init_selected_data(input, area_data, selected_data)

  observeEvent(input$topic, {
    req(input$topic, area_data())
    topic_data <- area_data()[[input$topic]]
    if (!is.null(topic_data)) {
      updateSelectizeInput(session, "subtopic",
                           choices  = names(topic_data),
                           selected = names(topic_data)[1])
    }
  })

  observeEvent(input$subtopic, {
    req(input$topic, input$subtopic, area_data())
    subtopic_data <- area_data()[[input$topic]][[input$subtopic]]
    if (!is.null(subtopic_data)) {
      updateSelectizeInput(session, "indicator",
                           choices  = names(subtopic_data),
                           selected = names(subtopic_data)[1])
    }
  })

  render_selections_dynamic(session, input, output, selected_data, area_names)
  render_filter_ui(session, input, output, selected_data)
  update_year_on_filter(session, input, output, selected_data)

  init_map(output, input, geo_data)

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
    session              = session,
    input                = input,
    output               = output,
    selected_data        = selected_data,
    geo_data             = geo_data,
    palette_ds           = palette_ds,
    palette_ds_alternative = palette_ds_alternative,
    check_all_filters    = check_all_filters,
    debounced_inputs     = debounced_inputs,
    counter              = counter,
    area_names           = area_names
  )

  update_gemeinde_selection_on_click(session, input)
  zoom_and_zoom_reset(session, input, previous_gemeinde, geo_data)
  update_summary_filter(session, input, selected_data)
  render_hc_summary(session, input, output, selected_data, check_conditions, bezirk_data)


  ## Tab 2 – Vergleich (Berichte) -----------------------------------------

  update_compare_area(
    session,
    input,
    bezirk_data2,
    bezirk_data_compare,
    selected_compare_area,
    selected_base_area
  )

  update_selected_compare_area(input, selected_compare_area)

  structure_list_reactive <- reactiveVal(NULL)
  renderedTopics          <- reactiveVal(c())

  render_topic_ui(session, output, input, structure_list_reactive, renderedTopics)
  change_at_report_topic(session, output, input, selected_base_area, selected_compare_area,
                         bezirk_data_names2, structure_list_reactive)
  change_at_compare_area(session, output, input, selected_base_area, selected_compare_area,
                         bezirk_data_names2, structure_list_reactive)
  change_at_base_area(session, output, input, selected_base_area, selected_compare_area,
                      bezirk_data_names2, structure_list_reactive)


  ## Tab 3 – Zeitreihe / Self-Service --------------------------------------

  selected_data_serv <- reactive({
    req(input$self_service_topic, input$self_service_subtopic,
        input$self_service_indicator)
    nested_list[[input$self_service_topic]][[input$self_service_subtopic]][[input$self_service_indicator]]
  })

  observeEvent(input$self_service_topic, {
    updateSelectizeInput(session, "self_service_subtopic",
                         choices  = names(nested_list[[input$self_service_topic]]),
                         selected = NULL)
  })

  observeEvent(input$self_service_subtopic, {
    updateSelectizeInput(
      session, "self_service_indicator",
      choices  = names(nested_list[[input$self_service_topic]]
                                  [[input$self_service_subtopic]]),
      selected = NULL)
  })

  render_selections_dynamic_self_service(session, input, output, selected_data_serv)
  render_filter_ui_self_service(session, input, output, selected_data_serv)
  update_year_on_filter_self_service(session, input, output, selected_data_serv)

  download_data <- reactiveVal(bezirk_data)
  add_selection(session, input, output, selected_data_serv, download_data)


  ## Tab 4 – Externe Daten ------------------------------------------------

  uploaded_data <- reactiveVal(NULL)

  upload_external_data(session, input, output, uploaded_data)
  create_var_type_radio_buttons(session, input, output)
  generate_xlsx_pattern(output, bezirk_data)
  draw_base_map(output, gemeindegrenzen)
  update_map_with_custom_data(session, input, gemeindegrenzen, uploaded_data)
}


# Run --------------------------------------------------------------------------
shinyApp(ui = ui, server = server)
