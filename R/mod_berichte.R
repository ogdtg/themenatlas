#' berichte UI Function (Tab 2 – area reports / comparison)
#'
#' @param id Internal module id.
#' @noRd
#' @importFrom bsicons bs_icon
mod_berichte_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    "Berichte",
    icon = bs_icon("file-text"),
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        title = "Bericht wählen",
        selectizeInput(ns("base_area"),
          label = "Gebiet wählen:",
          choices = setNames(bezirk_data2$bfs_nr_gemeinde, bezirk_data2$name_gemeinde),
          selected = "4566"),
        selectizeInput(ns("compare_area"),
          label = "Vergleichen mit:",
          choices = setNames(bezirk_data_mod$bfs_nr_gemeinde, bezirk_data_mod$name_gemeinde),
          selected = "4671"),
        hr(),
        selectizeInput(ns("report_topic"),
          label = "Bericht auswählen:",
          choices = c("Bevölkerung", "Haushalte", "Soziales",
                      "Wirtschaft und Arbeit", "Bauen und Wohnen",
                      "Raum", "Öffentliche Finanzen", "Staat und Politik"))
      ),
      uiOutput(ns("generic_report_part"))
    )
  )
}

#' berichte Server Functions
#'
#' @param id Internal module id.
#' @noRd
mod_berichte_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    selected_base_area    <- reactiveVal("4566")
    selected_compare_area <- reactiveVal("4671")
    bezirk_data_compare   <- reactiveVal(bezirk_data_mod)
    structure_list_reactive <- reactiveVal(NULL)
    renderedTopics          <- reactiveVal(character())

    update_compare_area(
      session, input,
      bezirk_data2,
      bezirk_data_compare,
      selected_compare_area,
      selected_base_area
    )

    update_selected_compare_area(input, selected_compare_area)

    render_topic_ui(session, output, input, structure_list_reactive, renderedTopics)

    change_at_report_topic(session, output, input, selected_base_area, selected_compare_area,
                           bezirk_data_names2, structure_list_reactive)
    change_at_compare_area(session, output, input, selected_base_area, selected_compare_area,
                           bezirk_data_names2, structure_list_reactive)
    change_at_base_area(session, output, input, selected_base_area, selected_compare_area,
                        bezirk_data_names2, structure_list_reactive)
  })
}
