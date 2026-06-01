#' selfservice UI Function (Tab 3 – Data Self Service)
#'
#' Lets users assemble a custom, downloadable table of indicators.
#'
#' @param id Internal module id.
#' @noRd
#' @importFrom bsicons bs_icon
mod_selfservice_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    "Data Self Service",
    icon = bs_icon("graph-up"),
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        title = "Daten konfigurieren",
        selectizeInput(ns("self_service_topic"),     "Themenbereich", choices = names(nested_list)),
        selectizeInput(ns("self_service_subtopic"),  "Thema",         choices = NULL),
        selectizeInput(ns("self_service_indicator"), "Indikator",     choices = NULL),
        uiOutput(ns("self_service_filter_ui")),
        selectizeInput(ns("self_service_year"),      "Jahr",          choices = NULL),
        uiOutput(ns("self_service_radio_bas_perc")),
        hr(),
        actionButton(ns("add_selection"), "Auswahl hinzufügen", class = "btn-primary w-100")
      ),
      layout_columns(
        col_widths = 12,
        card(card_body(uiOutput(ns("selected_filters")))),
        card(
          full_screen = TRUE,
          card_body(
            padding = 0,
            div(class = "dt-table", DTOutput(ns("download_table")), style = "font-size: 75%")
          ),
          card_footer(
            downloadButton(ns("self_service_download_csv"),   "Download als CSV",
                           class = "btn-outline-primary"),
            downloadButton(ns("self_service_download_excel"), "Download als Excel",
                           class = "btn-outline-success ms-2")
          )
        )
      )
    )
  )
}

#' selfservice Server Functions
#'
#' @param id Internal module id.
#' @noRd
mod_selfservice_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    selected_data_serv <- reactive({
      req(input$self_service_topic, input$self_service_subtopic,
          input$self_service_indicator)
      nested_list[[input$self_service_topic]][[input$self_service_subtopic]][[input$self_service_indicator]]
    })

    observeEvent(input$self_service_topic, {
      updateSelectizeInput(session, "self_service_subtopic",
                           choices = names(nested_list[[input$self_service_topic]]),
                           selected = NULL)
    })

    observeEvent(input$self_service_subtopic, {
      updateSelectizeInput(
        session, "self_service_indicator",
        choices = names(nested_list[[input$self_service_topic]][[input$self_service_subtopic]]),
        selected = NULL)
    })

    render_selections_dynamic_self_service(session, input, output, selected_data_serv)
    render_filter_ui_self_service(session, input, output, selected_data_serv)
    update_year_on_filter_self_service(session, input, output, selected_data_serv)

    download_data <- reactiveVal(bezirk_data)
    add_selection(session, input, output, selected_data_serv, download_data)
  })
}
