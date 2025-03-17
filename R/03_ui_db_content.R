#' UI Elements for the Shiny App
#'
#' This script defines the user interface (UI) components for the app, including:
#' - Sidebar navigation with multiple tabs.
#' - A dashboard layout with interactive UI elements.
#' - Data selection options for different topics, indicators, and years.
#' - A self-service data selection module.
#' - An upload section for external data processing.
#'
#' The UI includes maps, tables, summary charts, and reports that update based on
#' user input.
#'
#' @note Requires `bs4Dash`, `shiny`, `DT`, `leaflet`, and `highcharter` packages.
#' @author [Felix Lorenz]
#' @date [2025-03-17]


# UI DB Content
# Content of the sidebar

sidebar_content <- bs4Dash::sidebarMenu(id = "tabs",
                                        bs4Dash::menuItem("Karte", tabName = "tab1", icon = icon("map")),
                                        bs4Dash::menuItem("Berichte", tabName = "tab2", icon = icon("file")),
                                        bs4Dash::menuItem("Data Self Service", tabName = "tab3", icon = icon("download")),
                                        bs4Dash::menuItem("Externe Daten", tabName = "tab4", icon = icon("upload"))

)


# Content of the dashboard

db_content <-   tabItems(
  tabItem(tabName = "tab1",
          fluidRow(width=12,
                   box(width = 4,
                       fluidRow(
                         column(12, selectizeInput("topic", "Themenbereich", choices = names(nested_list))),
                         column(12, selectizeInput("subtopic", "Thema", choices = NULL)),
                         column(12, selectizeInput("indicator", "Indikator", choices = NULL)),
                         column(12, uiOutput("filter_ui")), # Filter dynamically displayed,
                         column(12, selectizeInput("bfs_nr_gemeinde", "Gemeinde", choices = NULL,selected=NULL)),
                         column(12, selectizeInput("year", "Jahr", choices = NULL)),
                         column(12, uiOutput("radio_bas_perc"))
                       )
                   ),

                   tabBox(width=8,id = "tab_box",
                          tabPanel("Karte",value = "map_tab",leafletOutput("map")),
                          tabPanel("Tabelle",value = "table_tab",DTOutput("data_table")),
                          tabPanel("Zusammenfassung",value = "summary_tab",
                                   selectizeInput("summary_select","Diagrammtyp",choices = NULL,selected=NULL),
                                   highchartOutput("summary_graph"))

                   )
          )

  ),
  tabItem(tabName = "tab2",
          fluidRow(
            box(
              title = tags$b("Bericht wählen"),
              width = 12,
              fluidRow(
                column(6, selectizeInput("base_area",
                                         label = "Gebiet wählen:",
                                         choices = setNames(bezirk_data2$bfs_nr_gemeinde,bezirk_data2$name_gemeinde),
                                         selected = "4566")),
                column(6, selectizeInput("compare_area",
                                         label = "Vergleichen mit",
                                         choices = setNames(bezirk_data_mod$bfs_nr_gemeinde,bezirk_data_mod$name_gemeinde),
                                         selected = "4671"))
              ),
              fluidRow(
                column(6, selectizeInput("report_topic",
                                         label = "Bericht auswählen:",
                                         choices  = c("Bevölkerung","Haushalte","Soziales","Wirtschaft und Arbeit","Bauen und Wohnen","Raum","Öffentliche Finanzen","Staat und Politik")))

              )



            )
          ),

          uiOutput("generic_report_part")



  ),
  tabItem(tabName = "tab3",
          fluidRow(
            box(width = 4,
                title = "Daten konfigurieren",
                fluidRow(
                  column(12, selectizeInput("self_service_topic", "Themenbereich", choices = names(nested_list))),
                  column(12, selectizeInput("self_service_subtopic", "Thema", choices = NULL)),
                  column(12, selectizeInput("self_service_indicator", "Indikator", choices = NULL)),
                  column(12, uiOutput("self_service_filter_ui")), # Filter dynamically displayed,
                  # column(12, selectizeInput("self_service_bfs_nr_gemeinde", "Gemeinde", choices = NULL,selected=NULL)),
                  column(12, selectizeInput("self_service_year", "Jahr", choices = NULL)),
                  column(12, uiOutput("self_service_radio_bas_perc")),
                  column(12, actionButton("add_selection","Auswahl hinzufügen"))
                )
            ),
            box(width = 8,
                uiOutput("selected_filters"),
                div(
                  class = "dt-table",
                  DTOutput("download_table"),
                  style = "font-size: 75%"
                )
            )


          )
  ),
  # New Tab for Data Upload and Visualization
  tabItem(tabName = "tab4",
          fluidRow(
            box(title = "Daten hochladen", width = 4,
                fileInput("upload_data", "CSV oder Excel hochladen", accept = c(".csv", ".xls", ".xlsx")),
                downloadButton("download_template", "Beispiel Excel herunterladen"),
                uiOutput("select_join_col"),
                uiOutput("select_vis_col"),
                uiOutput("variable_type"),
                uiOutput("numeric_options"),
                uiOutput("category_count"),
                actionButton("process_data", "Daten Verarbeiten"),
                uiOutput("messages")
            ),
            tabBox(width = 8, id = "tab_box_upload",
                   tabPanel("Karte", value = "map_tab", leafletOutput("uploaded_map")),
                   tabPanel("Daten", value = "data_tab", DTOutput("uploaded_data_table"))


            )
          )
  )


)
