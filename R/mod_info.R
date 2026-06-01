#' info UI Function (Info / About panel)
#'
#' @param id Internal module id.
#' @noRd
#' @importFrom bsicons bs_icon
mod_info_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    "Info",
    icon = bs_icon("info-circle"),
    layout_column_wrap(
      width = 1 / 2,
      card(
        card_header("Über den Themenatlas"),
        card_body(
          p("Der Thurgauer Themenatlas visualisiert statistische Daten der ",
            strong("Gemeinden"), " des Kantons Thurgau in Form von Karten, ",
            "Berichten und individuell zusammenstellbaren Tabellen."),
          p("Er umfasst Indikatoren aus den Themenbereichen Bevölkerung & Soziales, ",
            "Wirtschaft & Arbeit, Bauen & Wohnen, Raum & Umwelt sowie Staat & Politik."),
          p(tags$strong("Datenquelle:"), " Amt für Daten und Statistik, Kanton Thurgau."),
          tags$a(
            href = "https://statistik.tg.ch",
            target = "_blank",
            class = "btn btn-outline-primary btn-sm",
            bs_icon("box-arrow-up-right"), " statistik.tg.ch"
          )
        )
      ),
      card(
        card_header("Themenbereiche"),
        card_body(
          layout_column_wrap(
            width = 1 / 2,
            fill = FALSE,
            value_box("Bevölkerung & Soziales", "Bevölkerung, Haushalte, Sozialhilfe",
                      showcase = bs_icon("people"), theme = "primary"),
            value_box("Wirtschaft & Arbeit", "Beschäftigte, Arbeitsstätten, Pendler",
                      showcase = bs_icon("bar-chart-line"), theme = "success"),
            value_box("Bauen & Wohnen", "Leerstand, Bauinvestitionen, Gebäude",
                      showcase = bs_icon("building"), theme = "warning"),
            value_box("Raum & Umwelt", "Flächennutzung, Verkehr",
                      showcase = bs_icon("map"), theme = "info"),
            value_box("Staat & Politik", "Wahlen, Steuern, Finanzausgleich",
                      showcase = bs_icon("bank"), theme = "danger")
          )
        )
      )
    )
  )
}

#' info Server Functions
#'
#' @param id Internal module id.
#' @noRd
mod_info_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Static panel – no server logic required.
  })
}
