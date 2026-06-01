#' extern UI Function (Tab 4 – external data upload & visualisation)
#'
#' @param id Internal module id.
#' @noRd
#' @importFrom bsicons bs_icon
mod_extern_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    "Externe Daten",
    icon = bs_icon("upload"),
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        title = "Daten hochladen",
        fileInput(ns("upload_data"), "CSV oder Excel hochladen",
                  accept = c(".csv", ".xls", ".xlsx")),
        downloadButton(ns("download_template"), "Beispiel Excel herunterladen",
                       class = "btn-outline-secondary w-100"),
        hr(),
        uiOutput(ns("select_join_col")),
        uiOutput(ns("select_vis_col")),
        uiOutput(ns("variable_type")),
        uiOutput(ns("numeric_options")),
        uiOutput(ns("category_count")),
        actionButton(ns("process_data"), "Daten verarbeiten", class = "btn-primary w-100 mt-2"),
        uiOutput(ns("messages"))
      ),
      navset_card_tab(
        id = ns("tab_box_upload"),
        nav_panel(
          "Karte", value = "map_tab",
          leafletOutput(ns("uploaded_map"), height = "600px")
        ),
        nav_panel(
          "Daten", value = "data_tab",
          DTOutput(ns("uploaded_data_table"))
        )
      )
    )
  )
}

#' extern Server Functions
#'
#' @param id Internal module id.
#' @noRd
mod_extern_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    uploaded_data <- reactiveVal(NULL)

    upload_external_data(session, input, output, uploaded_data)
    create_var_type_radio_buttons(session, input, output)
    generate_xlsx_pattern(output, bezirk_data)
    draw_base_map(output, gemeindegrenzen)
    update_map_with_custom_data(session, input, gemeindegrenzen, uploaded_data)
  })
}
