# UI: New bslib page_navbar layout
# Maps existing content to the konzept tab structure:
#   Karte      <- old Tab 1 (map + selectors)
#   Vergleich  <- old Tab 2 (Berichte / reports)
#   Zeitreihe  <- old Tab 3 (Data Self Service – chart view)
#   Tabelle    <- old Tab 3 (Data Self Service – download)
#   Extern     <- old Tab 4 (external data upload)
#   Info       <- new informational panel


# --- Tab 1: Karte -----------------------------------------------------------

tab_karte <- nav_panel(
  "Karte",
  icon = bs_icon("map"),
  value = "tab1",
  layout_sidebar(
    fillable = TRUE,
    sidebar = sidebar(
      width = 280,
      open = "desktop",
      title = "Optionen",
      selectizeInput("area", "Gebietseinheit",
        choices = c("Bezirk","Gemeinde","Primarschulgemeinde",
                    "Volksschulgemeinde","Sekundarschulgemeinde"),
        selected = "Gemeinde"),
      selectizeInput("topic",    "Themenbereich", choices = NULL),
      selectizeInput("subtopic", "Thema",         choices = NULL),
      selectizeInput("indicator","Indikator",      choices = NULL),
      uiOutput("filter_ui"),
      selectizeInput("bfs_nr_gemeinde", "Gemeinde", choices = NULL, selected = NULL),
      selectizeInput("year", "Jahr", choices = NULL),
      uiOutput("radio_bas_perc"),
      actionButton("draw_map", "Anzeigen",
                   class = "btn-primary w-100 mt-2")
    ),

    # Main area: map card with inner tabs + indicator title
    card(
      full_screen = TRUE,
      card_header(uiOutput("indicator_title")),
      navset_card_tab(
        id = "tab_box",
        nav_panel(
          "Karte",
          value = "map_tab",
          leafletOutput("map", height = "550px")
        ),
        nav_panel(
          "Tabelle",
          value = "table_tab",
          DTOutput("data_table")
        ),
        nav_panel(
          "Zusammenfassung",
          value = "summary_tab",
          selectizeInput("summary_select", "Diagrammtyp",
                         choices = NULL, selected = NULL),
          highchartOutput("summary_graph", height = "400px")
        )
      )
    )
  )
)


# --- Tab 2: Vergleich (Berichte) --------------------------------------------

tab_vergleich <- nav_panel(
  "Vergleich",
  icon = bs_icon("file-text"),
  value = "tab2",
  layout_sidebar(
    sidebar = sidebar(
      width = 280,
      title = "Bericht wählen",
      selectizeInput("base_area",
        label = "Gebiet wählen:",
        choices = setNames(bezirk_data2$bfs_nr_gemeinde,
                           bezirk_data2$name_gemeinde),
        selected = "4566"),
      selectizeInput("compare_area",
        label = "Vergleichen mit:",
        choices = setNames(bezirk_data_mod$bfs_nr_gemeinde,
                           bezirk_data_mod$name_gemeinde),
        selected = "4671"),
      hr(),
      selectizeInput("report_topic",
        label = "Bericht auswählen:",
        choices = c("Bevölkerung","Haushalte","Soziales",
                    "Wirtschaft und Arbeit","Bauen und Wohnen",
                    "Raum","Öffentliche Finanzen","Staat und Politik"))
    ),
    uiOutput("generic_report_part")
  )
)


# --- Tab 3: Zeitreihe / Self-Service ----------------------------------------

tab_zeitreihe <- nav_panel(
  "Zeitreihe",
  icon = bs_icon("graph-up"),
  value = "tab3",
  layout_sidebar(
    sidebar = sidebar(
      width = 280,
      title = "Daten konfigurieren",
      selectizeInput("self_service_topic",    "Themenbereich", choices = names(nested_list)),
      selectizeInput("self_service_subtopic", "Thema",         choices = NULL),
      selectizeInput("self_service_indicator","Indikator",     choices = NULL),
      uiOutput("self_service_filter_ui"),
      selectizeInput("self_service_year",     "Jahr",          choices = NULL),
      uiOutput("self_service_radio_bas_perc"),
      hr(),
      actionButton("add_selection", "Auswahl hinzufügen",
                   class = "btn-primary w-100")
    ),
    layout_columns(
      col_widths = 12,
      card(
        card_body(uiOutput("selected_filters"))
      ),
      card(
        full_screen = TRUE,
        card_body(
          padding = 0,
          div(
            class = "dt-table",
            DTOutput("download_table"),
            style = "font-size: 75%"
          )
        ),
        card_footer(
          downloadButton("self_service_download_csv",   "Download als CSV",
                         class = "btn-outline-primary"),
          downloadButton("self_service_download_excel", "Download als Excel",
                         class = "btn-outline-success ms-2")
        )
      )
    )
  )
)


# --- Tab 4: Externe Daten ---------------------------------------------------

tab_extern <- nav_panel(
  "Externe Daten",
  icon = bs_icon("upload"),
  value = "tab4",
  layout_sidebar(
    sidebar = sidebar(
      width = 280,
      title = "Daten hochladen",
      fileInput("upload_data", "CSV oder Excel hochladen",
                accept = c(".csv", ".xls", ".xlsx")),
      downloadButton("download_template", "Beispiel Excel herunterladen",
                     class = "btn-outline-secondary w-100"),
      hr(),
      uiOutput("select_join_col"),
      uiOutput("select_vis_col"),
      uiOutput("variable_type"),
      uiOutput("numeric_options"),
      uiOutput("category_count"),
      actionButton("process_data", "Daten verarbeiten",
                   class = "btn-primary w-100 mt-2"),
      uiOutput("messages")
    ),
    navset_card_tab(
      id = "tab_box_upload",
      nav_panel(
        "Karte",
        value = "map_tab",
        leafletOutput("uploaded_map", height = "550px")
      ),
      nav_panel(
        "Daten",
        value = "data_tab",
        DTOutput("uploaded_data_table")
      )
    )
  )
)


# --- Tab 5: Info ------------------------------------------------------------

tab_info <- nav_panel(
  "Info",
  icon = bs_icon("info-circle"),
  value = "tab5",
  layout_column_wrap(
    width = 1/2,
    card(
      card_header("Über den Themenatlas"),
      card_body(
        p("Der Thurgauer Themenatlas visualisiert statistische Daten aller ",
          strong("Gemeinden"), " des Kantons Thurgau."),
        p("Er umfasst Indikatoren aus den Themenbereichen Bevölkerung, Wirtschaft,",
          " Bauen und Wohnen, Raum und Umwelt sowie Staat und Politik."),
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
          width = 1/2,
          fill = FALSE,
          value_box(
            title = "Bevölkerung & Soziales",
            value = "Bevölkerung, Haushalte, Sozialhilfe",
            showcase = bs_icon("people"),
            theme = "primary"
          ),
          value_box(
            title = "Wirtschaft & Arbeit",
            value = "Beschäftigte, Arbeitsstätten, Pendler",
            showcase = bs_icon("bar-chart-line"),
            theme = "success"
          ),
          value_box(
            title = "Bauen & Wohnen",
            value = "Leerstand, Bauinvestitionen, Gebäude",
            showcase = bs_icon("building"),
            theme = "warning"
          ),
          value_box(
            title = "Raum & Umwelt",
            value = "Flächennutzung, Verkehr",
            showcase = bs_icon("map"),
            theme = "info"
          ),
          value_box(
            title = "Staat & Politik",
            value = "Wahlen, Steuern, Finanzausgleich",
            showcase = bs_icon("bank"),
            theme = "danger"
          )
        )
      )
    )
  )
)
