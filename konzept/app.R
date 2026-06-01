# =============================================================================
# Thurgauer Themenatlas – Shiny-Konzept
# Packages: shiny, bslib, bsicons, leaflet, ggplot2, dplyr, sf, thematic
# =============================================================================

library(shiny)
library(bslib)
library(bsicons)
library(dplyr)
library(ggplot2)
library(leaflet)

# -----------------------------------------------------------------------------
# Indikatoren-Struktur (Metadaten)
# In der echten App: aus JSON/CSV laden, hier hartkodiert als Referenz
# -----------------------------------------------------------------------------

indikatoren <- list(
  "Bevölkerung & Soziales" = list(
    icon = "people",
    farbe = "primary",
    gruppen = list(
      "Bevölkerungsstand" = list(
        indikatoren = c(
          "Gesamtbevölkerung", "Bevölkerungsdichte", "Durchschnittsalter",
          "Anteil ausländische Bevölkerung", "Anteil Schweizer Bevölkerung",
          "Bevölkerungsverteilung nach Alter", "Bevölkerungsverteilung nach Geschlecht",
          "Bevölkerungsverteilung nach Nationalität", "Anteil unter 20-Jährige",
          "Anteil 20-64-Jährige", "Anteil über 64-Jährige",
          "Bevölkerung nach Konfession", "Anteil evang.-ref. Bevölkerung",
          "Anteil röm.-kath. Bevölkerung", "Anzahl Personen unter 20 Jahren",
          "Anzahl 20-64-Jährige", "Anzahl über 64-Jährige",
          "Schweizer Bevölkerung", "Ausländische Bevölkerung",
          "Durchschnittsalter der Rentnerinnen und Rentner"
        ),
        jahre = c(1970, 2025)
      ),
      "Bevölkerungsentwicklung" = list(
        indikatoren = c(
          "Bevölkerungsentwicklung im Vorjahresvergleich",
          "Bevölkerungsentwicklung im Vergleich zu vor 5 Jahren"
        ),
        jahre = c(1970, 2025)
      ),
      "Bevölkerungsbewegung" = list(
        indikatoren = c(
          "Lebendgeburten", "Todesfälle", "Geburtensaldo", "Geburtensaldo Total",
          "Total Zuzüge", "Total Wegzüge", "Wanderungssaldo Total",
          "Zuzüge aus dem Ausland", "Wegzüge ins Ausland", "Wanderungssaldo mit dem Ausland",
          "Zuzüge aus anderen Kantonen", "Wegzüge in andere Kantone",
          "Wanderungssaldo mit anderen Kantonen",
          "Zuzüge aus anderen Thurgauer Gemeinden",
          "Wegzüge in andere Thurgauer Gemeinden",
          "Wanderungssaldo mit anderen Thurgauer Gemeinden",
          "Wanderungssaldo Total pro 1000 Einw.",
          "Anzahl Heiraten (absolut)", "Anzahl Scheidungen (absolut)",
          "Anzahl Einbürgerungen (absolut)"
        ),
        jahre = c(2005, 2024)
      ),
      "Haushalte" = list(
        indikatoren = c(
          "Anzahl Haushalte", "Durchschnittliche Haushaltsgrösse",
          "Anzahl 1-Personen-Haushalte", "Anzahl 2-Personen-Haushalte",
          "Anzahl 3-Personen-Haushalte", "Anzahl 4-Personen-Haushalte",
          "Anzahl Haushalte mit 5 Personen oder mehr",
          "Anteil 1-Personen-Haushalte", "Anteil 2-Personen-Haushalte",
          "Anteil 3-Personen-Haushalte", "Anteil 4-Personen-Haushalte",
          "Anteil Haushalte mit 5 Personen oder mehr"
        ),
        jahre = c(2012, 2024)
      ),
      "Sozialhilfe" = list(
        indikatoren = c(
          "Sozialhilfequote", "Netto-Sozialhilfeausgaben",
          "Netto-Sozialhilfeausgaben pro Einw.",
          "Netto-Sozialhilfeausgaben pro Einw., 5-Jahresdurchschnitt",
          "Brutto-Sozialhilfeausgaben"
        ),
        jahre = c(2006, 2024)
      ),
      "Bildungsstand" = list(
        indikatoren = c("Tertiärabschluss"),
        jahre = c(2010, 2022)
      ),
      "Krebsneuerkrankungen" = list(
        indikatoren = c(
          "Neuerkrankungen nach Altersklassen, Männer",
          "Neuerkrankungen nach Altersklassen, Frauen",
          "Neuerkrankungen nach Lokalisation, Männer",
          "Neuerkrankungen nach Lokalisation, Frauen"
        ),
        jahre = c(2012, 2022)
      )
    )
  ),

  "Wirtschaft & Arbeit" = list(
    icon = "bar-chart-line",
    farbe = "success",
    gruppen = list(
      "Beschäftigte" = list(
        indikatoren = c(
          "Beschäftigte total", "Beschäftigte Sektor 1",
          "Beschäftigte Sektor 2", "Beschäftigte Sektor 3",
          "Anteil Beschäftigter im Sektor 1", "Anteil Beschäftigter im Sektor 2",
          "Anteil Beschäftigter im Sektor 3",
          "Vorjahresveränderung Beschäftigte total",
          "Vorjahresveränderung Beschäftigte Sektor 1",
          "Vorjahresveränderung Beschäftigte Sektor 2",
          "Vorjahresveränderung Beschäftigte Sektor 3",
          "Veränderung Beschäftigte total gegenüber vor 5 Jahren",
          "Veränderung Anteil Beschäftigter Sektor 1 gegenüber vor 5 Jahren",
          "Veränderung Anteil Beschäftigter Sektor 2 gegenüber vor 5 Jahren",
          "Veränderung Anteil Beschäftigter Sektor 3 gegenüber vor 5 Jahren"
        ),
        jahre = c(2020, 2023)
      ),
      "Arbeitsstätten" = list(
        indikatoren = c(
          "Arbeitsstätten total", "Arbeitsstätten Sektor 1",
          "Arbeitsstätten Sektor 2", "Arbeitsstätten Sektor 3",
          "Vorjahresveränderung Arbeitsstätten total",
          "Veränderung Arbeitsstätten total gegenüber vor 5 Jahren"
        ),
        jahre = c(2022, 2023)
      ),
      "Grenzgänger/-innen" = list(
        indikatoren = c(
          "Grenzgänger/innen total",
          "Anteil Grenzgänger/innen am Total der Beschäftigten"
        ),
        jahre = c(2012, 2025)
      ),
      "Arbeitslosigkeit" = list(
        indikatoren = c(
          "Anteil Arbeitslose (15–64-jährige Bev. Vorjahr)",
          "Anteil Stellensuchende (15–64-jährige Bev. Vorjahr)"
        ),
        jahre = c(2023, 2025)
      ),
      "Neu gegründete Unternehmen" = list(
        indikatoren = c(
          "Neu gegründete Unternehmen (kumuliert 2019-2023)",
          "Neugründungsrate (kumuliert 2019-2023)"
        ),
        jahre = c(2023, 2023)
      ),
      "Pendler" = list(
        indikatoren = c(
          "Anzahl Zupendler (ohne Ausland)", "Anzahl Wegpendler (ohne Ausland)",
          "Zupendlerquote (ohne Ausland)", "Wegpendlerquote (ohne Ausland)",
          "Pendlersaldoquote (ohne Ausland)"
        ),
        jahre = c(2014, 2018)
      )
    )
  ),

  "Bauen & Wohnen" = list(
    icon = "building",
    farbe = "warning",
    gruppen = list(
      "Leer stehende Wohnungen" = list(
        indikatoren = c(
          "Leerwohnungsziffer", "Leer stehende Wohnungen total",
          "Leer stehende Wohnungen nach Angebot"
        ),
        jahre = c(2005, 2025)
      ),
      "Bauinvestitionen" = list(
        indikatoren = c(
          "Bauinvestitionen total", "Bauinvestitionen im Vorjahresvergleich",
          "Bauinvestitionen Wohnbau", "Anteil Wohnbau an Bauinvestitionen",
          "Bauinvestitionen Industrie/Gewerbe/Dienstl.",
          "Bauinvestitionen nach Bauwerkstyp", "Bauinvestitionen nach Auftraggeber"
        ),
        jahre = c(2013, 2023)
      ),
      "Gebäude & Wohnungen" = list(
        indikatoren = c(
          "Wohngebäude total", "Einfamilienhäuser", "Mehrfamilienhäuser",
          "Anteil Einfamilienhäuser", "Anteil Mehrfamilienhäuser",
          "Anteil Wohngebäude mit Wärmepumpe",
          "Anteil Wohngebäude mit Heizöl",
          "Anteil Wohngebäude mit Gas",
          "Wohnungen total", "1-2-Zimmerwohnungen", "3-4-Zimmerwohnungen",
          "5-Zimmerwohnungen", "Wohnungen mit 6+ Zimmern",
          "Neu erstellte Wohngebäude", "Neu erstellte Wohnungen",
          "Anteil neu erstellter Wohnungen am Vorjahresbestand",
          "Gebäude Bauperiode bis 1945", "Gebäude Bauperiode 1946-1970",
          "Gebäude Bauperiode 1971-2000", "Gebäude Bauperiode nach 2000",
          "Drei-Jahres-Veränderung des Wohngebäudebestands",
          "Wohngebäude nach Gebäudekategorie", "Wohnungen nach Zimmerzahl",
          "Gebäude mit Wohnungsnutzung nach Bauperiode"
        ),
        jahre = c(2009, 2024)
      )
    )
  ),

  "Raum, Verkehr & Umwelt" = list(
    icon = "map",
    farbe = "info",
    gruppen = list(
      "Flächennutzung" = list(
        indikatoren = c(
          "Bevölkerungsdichte", "Fläche (ohne Bodensee)", "Landfläche",
          "Anteil Siedlungsfläche", "Anteil Landwirtschaftsfläche",
          "Anteil Waldfläche", "Anteil unproduktive Fläche",
          "Flächenanteil nach Nutzung",
          "Veränderung der Siedlungsfläche (Vergleich letzte Erhebung)",
          "Siedlungsfläche pro Einwohner"
        ),
        jahre = c(1984, 2017)
      ),
      "Personenwagenbestand" = list(
        indikatoren = c("Motorisierungsgrad", "Personenwagenbestand"),
        jahre = c(2023, 2025)
      )
    )
  ),

  "Staat & Politik" = list(
    icon = "bank",
    farbe = "danger",
    gruppen = list(
      "Grossratswahlen" = list(
        indikatoren = c(
          "Parteistärke SVP", "Parteistärke Die Mitte", "Parteistärke SP",
          "Parteistärke FDP", "Parteistärke GRÜNE", "Parteistärke GLP",
          "Parteistärke EVP", "Parteistärke EDU", "Parteistärke Aufrecht TG",
          "Veränderung Parteistärke SVP", "Veränderung Parteistärke FDP",
          "Veränderung Parteistärke Die Mitte", "Veränderung Parteistärke SP",
          "Veränderung Parteistärke GRÜNE", "Veränderung Parteistärke GLP",
          "Veränderung Parteistärke EVP", "Veränderung Parteistärke EDU",
          "Wahlbeteiligung"
        ),
        jahre = c(2012, 2024)
      ),
      "Nationalratswahlen" = list(
        indikatoren = c(
          "Parteistärke SVP", "Parteistärke Die Mitte", "Parteistärke FDP",
          "Parteistärke SP", "Parteistärke GRÜNE", "Parteistärke GLP",
          "Parteistärke EDU", "Parteistärke EVP", "Parteistärke Aufrecht TG",
          "Parteistärke MASS-Voll!", "Parteistärke BDP",
          "Veränderung Parteistärke SVP", "Veränderung Parteistärke Die Mitte",
          "Veränderung Parteistärke FDP", "Veränderung Parteistärke SP",
          "Veränderung Parteistärke GRÜNE", "Veränderung Parteistärke GLP",
          "Veränderung Parteistärke EDU", "Veränderung Parteistärke EVP",
          "Veränderung Parteistärke BDP", "Wahlbeteiligung"
        ),
        jahre = c(2011, 2023)
      ),
      "Steuerkraft & Steuerfüsse" = list(
        indikatoren = c(
          "Steuerkraft pro Einwohner zu 100%",
          "Gesamtsteuerfuss nat. Personen evang.",
          "Gesamtsteuerfuss nat. Personen kath.",
          "Gesamtsteuerfuss jur. Personen",
          "Gemeindesteuerfuss",
          "Veränderung Gemeindesteuerfüsse (Vergleich vor 10 Jahren)"
        ),
        jahre = c(2005, 2025)
      ),
      "Finanzausgleich" = list(
        indikatoren = c(
          "Finanzausgleich: Auszahlungen/Abschöpfungen",
          "Finanzausgleich pro Einwohner"
        ),
        jahre = c(2015, 2025)
      )
    )
  )
)

# Hilfsfunktion: Flache Liste aller Indikatoren für Suche
alle_indikatoren <- function() {
  result <- list()
  for (thema in names(indikatoren)) {
    for (gruppe in names(indikatoren[[thema]]$gruppen)) {
      for (ind in indikatoren[[thema]]$gruppen[[gruppe]]$indikatoren) {
        result[[length(result) + 1]] <- list(
          label = ind,
          gruppe = gruppe,
          thema = thema
        )
      }
    }
  }
  result
}

# Jahre-Range für einen Indikator
get_jahre <- function(thema, gruppe) {
  indikatoren[[thema]]$gruppen[[gruppe]]$jahre
}

# -----------------------------------------------------------------------------
# Simulierte Gemeindedaten (Platzhalter – echte Daten via API/CSV laden)
# -----------------------------------------------------------------------------

gemeinden_demo <- data.frame(
  name = c(
    "Frauenfeld", "Kreuzlingen", "Arbon", "Amriswil", "Weinfelden",
    "Konstanz-TG", "Bischofszell", "Diessenhofen", "Steckborn", "Münchwilen",
    "Aadorf", "Tägerwilen", "Ermatingen", "Gottlieben", "Homburg"
  ),
  bev = c(25000, 22000, 14000, 12000, 11000, 8500, 5800, 4200, 3800, 5200,
          9000, 5500, 2800, 800, 2100),
  anteil_ausl = c(28, 32, 24, 20, 18, 22, 16, 14, 15, 19,
                  21, 25, 17, 12, 13),
  steuerfuss = c(62, 59, 68, 72, 65, 70, 75, 80, 78, 71,
                 66, 61, 73, 82, 77),
  lat = c(47.556, 47.638, 47.514, 47.544, 47.569, 47.676, 47.501, 47.668,
          47.666, 47.459, 47.488, 47.667, 47.661, 47.674, 47.543),
  lon = c(8.898, 9.175, 9.432, 9.289, 9.104, 9.186, 9.228, 8.757,
          8.996, 8.990, 8.900, 9.133, 9.079, 9.102, 8.970)
)

# -----------------------------------------------------------------------------
# UI
# -----------------------------------------------------------------------------

ui <- page_navbar(
  id = "nav",
  title = tags$span(
    bs_icon("map"), " Thurgauer Themenatlas"
  ),
  theme = bs_theme(
    version = 5,
    preset = "shiny",
    primary = "#185FA5",
    success = "#1D9E75",
    warning = "#BA7517",
    info    = "#1D9E75",
    danger  = "#A32D2D",
    "font-size-base" = "0.9rem"
  ),
  fillable = c("Karte", "Vergleich", "Zeitreihe"),
  bg = "#1a2a3a",
  inverse = TRUE,

  # Globaler Header-Bereich rechts
  nav_spacer(),
  nav_item(
    input_dark_mode(id = "dark_mode", mode = "light")
  ),

  # ----------------------------------------------------------
  # TAB 1: Karte
  # ----------------------------------------------------------
  nav_panel(
    "Karte",
    class = "bslib-page-dashboard",
    icon = bs_icon("map"),
    layout_sidebar(
      fillable = TRUE,
      sidebar = sidebar(
        width = 280,
        open = "desktop",
        title = "Indikator wählen",

        # Suche
        textInput(
          "suche", NULL,
          placeholder = "Indikator suchen…",
          width = "100%"
        ),

        # Thema / Gruppe / Indikator als verschachteltes Accordion
        accordion(
          id = "thema_acc",
          open = "Bevölkerung & Soziales",

          lapply(names(indikatoren), function(thema) {
            ti <- indikatoren[[thema]]
            accordion_panel(
              title = tagList(bs_icon(ti$icon), " ", thema),
              value = thema,

              accordion(
                id = paste0("grp_", gsub(" ", "_", thema)),
                open = FALSE,

                lapply(names(ti$gruppen), function(gruppe) {
                  grp <- ti$gruppen[[gruppe]]
                  accordion_panel(
                    title = gruppe,
                    value = gruppe,
                    radioButtons(
                      inputId = paste0(
                        "ind_", gsub("[^a-zA-Z0-9]", "_", thema),
                        "_", gsub("[^a-zA-Z0-9]", "_", gruppe)
                      ),
                      label = NULL,
                      choices = grp$indikatoren,
                      selected = grp$indikatoren[1]
                    )
                  )
                })
              )
            )
          })
        ),

        hr(),

        # Jahresslider
        uiOutput("jahr_slider_ui"),

        # Darstellungsoptionen
        hr(),
        selectInput(
          "farbreihe",
          "Farbschema",
          choices = c(
            "Blau (sequenziell)" = "Blues",
            "Rot-Blau (divergierend)" = "RdBu",
            "Grün (sequenziell)" = "Greens",
            "Orange (sequenziell)" = "Oranges",
            "Viridis" = "viridis"
          )
        ),
        checkboxInput("gemeinde_labels", "Gemeindenamen anzeigen", value = FALSE),
        checkboxInput("legende", "Legende anzeigen", value = TRUE)
      ),

      # Hauptbereich: Karte + Infokarten
      layout_columns(
        col_widths = c(8, 4),
        row_heights = c("1fr"),

        # Karte
        card(
          full_screen = TRUE,
          card_header(
            uiOutput("karten_titel"),
            class = "d-flex justify-content-between align-items-center"
          ),
          leafletOutput("karte", height = "100%")
        ),

        # Rechte Spalte: KPIs + Ranking
        layout_columns(
          col_widths = 12,
          row_heights = c("auto", "1fr"),
          fill = FALSE,

          # KPI-Boxen
          layout_column_wrap(
            width = 1/2,
            fill = FALSE,
            value_box(
              title = "Gemeinden",
              value = "80",
              showcase = bs_icon("geo-alt"),
              theme = "primary"
            ),
            value_box(
              title = "Gewähltes Jahr",
              value = textOutput("aktuelles_jahr", inline = TRUE),
              showcase = bs_icon("calendar3"),
              theme = "secondary"
            )
          ),

          # Ranking-Tabelle
          card(
            full_screen = TRUE,
            card_header(
              "Gemeinderanking",
              popover(
                bs_icon("info-circle", title = "Info"),
                "Top 10 Gemeinden nach gewähltem Indikator"
              )
            ),
            card_body(
              padding = 0,
              tableOutput("ranking_tabelle")
            )
          )
        )
      )
    )
  ),

  # ----------------------------------------------------------
  # TAB 2: Vergleich (zwei Gemeinden)
  # ----------------------------------------------------------
  nav_panel(
    "Vergleich",
    class = "bslib-page-dashboard",
    icon = bs_icon("arrows-expand"),
    layout_sidebar(
      fillable = TRUE,
      sidebar = sidebar(
        width = 260,
        title = "Gemeinden wählen",

        selectInput(
          "gem_a",
          "Gemeinde A",
          choices = sort(gemeinden_demo$name),
          selected = "Frauenfeld"
        ),
        selectInput(
          "gem_b",
          "Gemeinde B",
          choices = sort(gemeinden_demo$name),
          selected = "Kreuzlingen"
        ),

        hr(),
        selectInput(
          "vergl_thema",
          "Thema",
          choices = names(indikatoren)
        ),
        uiOutput("vergl_gruppe_ui"),
        uiOutput("vergl_ind_ui")
      ),

      # Split-Ansicht
      layout_columns(
        col_widths = c(6, 6),

        card(
          full_screen = TRUE,
          card_header(textOutput("gem_a_titel")),
          layout_columns(
            col_widths = 12,
            uiOutput("gem_a_vbox"),
            plotOutput("gem_a_chart", height = "200px")
          )
        ),

        card(
          full_screen = TRUE,
          card_header(textOutput("gem_b_titel")),
          layout_columns(
            col_widths = 12,
            uiOutput("gem_b_vbox"),
            plotOutput("gem_b_chart", height = "200px")
          )
        )
      )
    )
  ),

  # ----------------------------------------------------------
  # TAB 3: Zeitreihe
  # ----------------------------------------------------------
  nav_panel(
    "Zeitreihe",
    class = "bslib-page-dashboard",
    icon = bs_icon("graph-up"),
    layout_sidebar(
      fillable = TRUE,
      sidebar = sidebar(
        width = 260,
        title = "Optionen",

        selectizeInput(
          "zt_gemeinden",
          "Gemeinden (max. 5)",
          choices = sort(gemeinden_demo$name),
          selected = c("Frauenfeld", "Kreuzlingen", "Arbon"),
          multiple = TRUE,
          options = list(maxItems = 5)
        ),

        hr(),
        selectInput("zt_thema", "Thema", choices = names(indikatoren)),
        uiOutput("zt_gruppe_ui"),
        uiOutput("zt_ind_ui"),

        hr(),
        checkboxInput("zt_kanton", "Kantonsdurchschnitt einblenden", TRUE),
        checkboxInput("zt_punkte", "Datenpunkte anzeigen", FALSE)
      ),

      card(
        full_screen = TRUE,
        card_header("Zeitreihe"),
        plotOutput("zeitreihen_plot", height = "100%")
      )
    )
  ),

  # ----------------------------------------------------------
  # TAB 4: Tabelle / Export
  # ----------------------------------------------------------
  nav_panel(
    "Tabelle",
    icon = bs_icon("table"),
    fillable = FALSE,
    layout_sidebar(
      sidebar = sidebar(
        width = 260,
        title = "Filter",
        selectInput(
          "tab_thema",
          "Thema",
          choices = c("(Alle)", names(indikatoren))
        ),
        uiOutput("tab_gruppe_ui"),
        textInput("tab_suche", "Gemeinde filtern", placeholder = "Suchen…"),
        hr(),
        downloadButton("export_csv", "CSV herunterladen", class = "btn-outline-primary w-100"),
        tags$br(), tags$br(),
        downloadButton("export_excel", "Excel herunterladen", class = "btn-outline-success w-100")
      ),

      card(
        card_header(
          "Daten aller Gemeinden",
          class = "d-flex justify-content-between align-items-center"
        ),
        card_body(
          padding = 0,
          DT::dataTableOutput("daten_tabelle")
        )
      )
    )
  ),

  # ----------------------------------------------------------
  # TAB 5: Über / Info
  # ----------------------------------------------------------
  nav_panel(
    "Info",
    icon = bs_icon("info-circle"),
    fillable = FALSE,

    layout_column_wrap(
      width = 1/2,

      card(
        card_header("Über den Themenatlas"),
        card_body(
          p("Der Thurgauer Themenatlas visualisiert statistische Daten aller ",
            strong("80 Gemeinden"), " des Kantons Thurgau in Form von Choroplethen-Karten."),
          p("Datenquelle: Amt für Daten und Statistik, Kanton Thurgau"),
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
            lapply(names(indikatoren), function(thema) {
              ti <- indikatoren[[thema]]
              n_ind <- sum(sapply(ti$gruppen, function(g) length(g$indikatoren)))
              value_box(
                title = thema,
                value = paste(n_ind, "Indikatoren"),
                showcase = bs_icon(ti$icon),
                theme = ti$farbe
              )
            })
          )
        )
      )
    )
  )
)

# -----------------------------------------------------------------------------
# Server
# -----------------------------------------------------------------------------

server <- function(input, output, session) {

  thematic::thematic_shiny()

  # --- Reaktiver ausgewählter Indikator (vereinfachtes Konzept) --------------
  # In der echten App: komplexere Logik, die den aktiven radioButton trackt

  aktiver_indikator <- reactive({
    # Demo: fixer Wert; in der App dynamisch aus Accordion ableiten
    list(
      name  = "Anteil ausländische Bevölkerung",
      thema = "Bevölkerung & Soziales",
      gruppe = "Bevölkerungsstand"
    )
  })

  # --- Jahresslider dynamisch ------------------------------------------------
  output$jahr_slider_ui <- renderUI({
    ind <- aktiver_indikator()
    jahre <- get_jahre(ind$thema, ind$gruppe)
    sliderInput(
      "jahr",
      "Jahr",
      min = jahre[1],
      max = jahre[2],
      value = jahre[2],
      step = 1,
      sep = "",
      width = "100%",
      ticks = FALSE,
      animate = animationOptions(interval = 800, loop = FALSE)
    )
  })

  output$aktuelles_jahr <- renderText({
    req(input$jahr)
    as.character(input$jahr)
  })

  # --- Kartentitel -----------------------------------------------------------
  output$karten_titel <- renderUI({
    ind <- aktiver_indikator()
    tagList(
      tags$span(ind$name, class = "fw-semibold"),
      tags$small(
        class = "text-muted ms-2",
        paste0(ind$thema, " › ", ind$gruppe)
      )
    )
  })

  # --- Leaflet-Karte (Demo mit simulierten Daten) ----------------------------
  output$karte <- renderLeaflet({

    pal <- colorNumeric(
      palette = if (!is.null(input$farbreihe)) input$farbreihe else "Blues",
      domain   = gemeinden_demo$anteil_ausl
    )

    m <- leaflet(gemeinden_demo) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      setView(lng = 9.1, lat = 47.56, zoom = 10) |>
      addCircleMarkers(
        lng = ~lon, lat = ~lat,
        radius = ~sqrt(bev / 400),
        color = "white",
        weight = 1.5,
        fillColor = ~pal(anteil_ausl),
        fillOpacity = 0.85,
        popup = ~paste0(
          "<strong>", name, "</strong><br>",
          "Bevölkerung: ", format(bev, big.mark = "'"), "<br>",
          "Ausländeranteil: ", anteil_ausl, "%"
        ),
        label = ~name
      )

    if (!is.null(input$legende) && input$legende) {
      m <- m |> addLegend(
        pal = pal, values = ~anteil_ausl,
        title = "Anteil ausl. Bev. (%)",
        position = "bottomright"
      )
    }

    m
  })

  # --- Ranking-Tabelle -------------------------------------------------------
  output$ranking_tabelle <- renderTable({
    gemeinden_demo |>
      arrange(desc(anteil_ausl)) |>
      slice_head(n = 10) |>
      select(Gemeinde = name, `Wert (%)` = anteil_ausl) |>
      mutate(Rang = row_number(), .before = 1)
  },
  striped = TRUE, hover = TRUE, spacing = "s",
  width = "100%", digits = 1)

  # --- Vergleich: Dynamische UIs ---------------------------------------------
  output$vergl_gruppe_ui <- renderUI({
    req(input$vergl_thema)
    gruppen <- names(indikatoren[[input$vergl_thema]]$gruppen)
    selectInput("vergl_gruppe", "Gruppe", choices = gruppen)
  })

  output$vergl_ind_ui <- renderUI({
    req(input$vergl_gruppe, input$vergl_thema)
    inds <- indikatoren[[input$vergl_thema]]$gruppen[[input$vergl_gruppe]]$indikatoren
    selectInput("vergl_ind", "Indikator", choices = inds)
  })

  output$gem_a_titel <- renderText(req(input$gem_a))
  output$gem_b_titel <- renderText(req(input$gem_b))

  output$gem_a_vbox <- renderUI({
    req(input$gem_a)
    gem <- gemeinden_demo[gemeinden_demo$name == input$gem_a, ]
    value_box(
      title = "Ausländeranteil",
      value = paste0(gem$anteil_ausl, " %"),
      showcase = bs_icon("people"),
      theme = "primary"
    )
  })

  output$gem_b_vbox <- renderUI({
    req(input$gem_b)
    gem <- gemeinden_demo[gemeinden_demo$name == input$gem_b, ]
    value_box(
      title = "Ausländeranteil",
      value = paste0(gem$anteil_ausl, " %"),
      showcase = bs_icon("people"),
      theme = "info"
    )
  })

  # Demo-Zeitreihenchart für Vergleich
  demo_verlauf <- function(startval) {
    set.seed(42)
    jahre <- 2010:2024
    data.frame(
      Jahr = jahre,
      Wert = cumsum(c(startval, rnorm(length(jahre) - 1, 0.1, 0.5)))
    )
  }

  output$gem_a_chart <- renderPlot({
    req(input$gem_a)
    gem <- gemeinden_demo[gemeinden_demo$name == input$gem_a, ]
    df <- demo_verlauf(gem$anteil_ausl - 5)
    ggplot(df, aes(Jahr, Wert)) +
      geom_line(color = "#185FA5", linewidth = 1.2) +
      geom_point(color = "#185FA5", size = 2) +
      labs(y = "%", x = NULL) +
      theme_minimal(base_size = 11) +
      theme(panel.grid.minor = element_blank())
  })

  output$gem_b_chart <- renderPlot({
    req(input$gem_b)
    gem <- gemeinden_demo[gemeinden_demo$name == input$gem_b, ]
    df <- demo_verlauf(gem$anteil_ausl - 3)
    ggplot(df, aes(Jahr, Wert)) +
      geom_line(color = "#1D9E75", linewidth = 1.2) +
      geom_point(color = "#1D9E75", size = 2) +
      labs(y = "%", x = NULL) +
      theme_minimal(base_size = 11) +
      theme(panel.grid.minor = element_blank())
  })

  # --- Zeitreihe: Dynamische UIs --------------------------------------------
  output$zt_gruppe_ui <- renderUI({
    req(input$zt_thema)
    gruppen <- names(indikatoren[[input$zt_thema]]$gruppen)
    selectInput("zt_gruppe", "Gruppe", choices = gruppen)
  })

  output$zt_ind_ui <- renderUI({
    req(input$zt_gruppe, input$zt_thema)
    inds <- indikatoren[[input$zt_thema]]$gruppen[[input$zt_gruppe]]$indikatoren
    selectInput("zt_ind", "Indikator", choices = inds)
  })

  output$zeitreihen_plot <- renderPlot({
    req(input$zt_gemeinden)

    farben <- c("#185FA5", "#1D9E75", "#BA7517", "#A32D2D", "#7F77DD")
    set.seed(123)

    dfs <- lapply(seq_along(input$zt_gemeinden), function(i) {
      gem_name <- input$zt_gemeinden[i]
      gem <- gemeinden_demo[gemeinden_demo$name == gem_name, ]
      base <- gem$anteil_ausl[1]
      jahre <- 2010:2024
      data.frame(
        Jahr = jahre,
        Wert = cumsum(c(base, rnorm(length(jahre) - 1, 0.1, 0.6))),
        Gemeinde = gem_name
      )
    })

    df_all <- do.call(rbind, dfs)

    p <- ggplot(df_all, aes(Jahr, Wert, color = Gemeinde, group = Gemeinde)) +
      geom_line(linewidth = 1.1) +
      scale_color_manual(values = setNames(farben[seq_along(input$zt_gemeinden)],
                                           input$zt_gemeinden)) +
      labs(
        title = NULL,
        y = "Wert (%)",
        x = NULL,
        color = "Gemeinde"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        legend.title = element_blank()
      )

    if (!is.null(input$zt_punkte) && input$zt_punkte) {
      p <- p + geom_point(size = 2.5)
    }

    if (!is.null(input$zt_kanton) && input$zt_kanton) {
      kant_df <- data.frame(
        Jahr = 2010:2024,
        Wert = seq(20, 23, length.out = 15)
      )
      p <- p +
        geom_line(
          data = kant_df,
          aes(Jahr, Wert),
          inherit.aes = FALSE,
          linetype = "dashed",
          color = "gray50",
          linewidth = 0.8
        ) +
        annotate("text", x = 2024.2, y = 23,
                 label = "Kanton", color = "gray50", size = 3.5, hjust = 0)
    }
    p
  })

  # --- Tabelle: Dynamische UIs + Export -------------------------------------
  output$tab_gruppe_ui <- renderUI({
    req(input$tab_thema)
    if (input$tab_thema == "(Alle)") return(NULL)
    gruppen <- c("(Alle)", names(indikatoren[[input$tab_thema]]$gruppen))
    selectInput("tab_gruppe", "Gruppe", choices = gruppen)
  })

  tabellen_daten <- reactive({
    df <- gemeinden_demo |>
      select(
        Gemeinde = name,
        Bevölkerung = bev,
        `Ausländeranteil (%)` = anteil_ausl,
        `Gemeindesteuerfuss` = steuerfuss
      )

    if (!is.null(input$tab_suche) && nchar(input$tab_suche) > 0) {
      df <- df |> filter(grepl(input$tab_suche, Gemeinde, ignore.case = TRUE))
    }
    df
  })

  output$daten_tabelle <- DT::renderDataTable({
    DT::datatable(
      tabellen_daten(),
      options = list(
        pageLength = 20,
        dom = "frtip",
        language = list(url = "//cdn.datatables.net/plug-ins/1.13.6/i18n/de-DE.json")
      ),
      rownames = FALSE,
      class = "table table-striped table-hover table-sm"
    )
  })

  output$export_csv <- downloadHandler(
    filename = function() paste0("themenatlas_tg_", Sys.Date(), ".csv"),
    content = function(file) write.csv(tabellen_daten(), file, row.names = FALSE)
  )

  output$export_excel <- downloadHandler(
    filename = function() paste0("themenatlas_tg_", Sys.Date(), ".xlsx"),
    content = function(file) openxlsx::write.xlsx(tabellen_daten(), file)
  )
}

# -----------------------------------------------------------------------------
shinyApp(ui, server)
