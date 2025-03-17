#' 05_create_structure_list.R
#'
#' This script generates a structured list containing meta-information about various graphs and tables
#' in the report tab (Tab 2). The data is categorized into different topics such as population,
#' housing, economy, politics, and social welfare. Each topic includes relevant data paths, chart types,
#' and indicator names for structured visualization in the report.
#'
#' Data Source:
#' - Reads the `data_source_list.rds` file to retrieve the relevant dataset.
#'
#' Output:
#' - Creates a combined list (`structure_list`) containing structured metadata for use in the report tab.
#'
#' Dependencies:
#' - The script assumes that the `nested_list` and `additional_data` objects are preloaded in the environment.







data_source_list <- readRDS("data/data_source_list.rds")




# Bevölkerung -------------------------------------------------------------


bevölkerung_list <- list(
  list(
    topic = "Bevölkerung",
    title = "Bevölkerungsstand und -struktur",
    chart_type = "indicator_table",
    indicator_list = list(
      list(
        data_path = "nested_list$`Bevölkerung und Soziales`$Bevölkerungsstand$Gesamtbevölkerung",
        indicator_names = "Gesamtbevölkerung"
      ),
      list(
        data_path = "nested_list$`Bevölkerung und Soziales`$Bevölkerungsentwicklung$`Bevölkerungsentwicklung (Vorjahr/5 Jahre)`",
        indicator_names = "Bevölkerungsentwicklung im Vorjahresvergleich (in %)",
        filter_values = "im Vorjahresvergleich"
      ),
      list(
        data_path = "nested_list$`Bevölkerung und Soziales`$Bevölkerungsentwicklung$`Bevölkerungsentwicklung (Vorjahr/5 Jahre)`",
        indicator_names = "Bevölkerungsentwicklung im Vergleich zu vor 5 Jahren (in %)",
        filter_values = "im Vergleich zu vor 5 Jahren"
      ),
      list(
        data_path = "nested_list$`Bevölkerung und Soziales`$Bevölkerungsstand$`Bevölkerungsverteilung nach Nationalität`",
        indicator_names = "Anteil ausländische Bevölkerung (%)",
        filter_values = "Ausland",
        value_types = "share"
      ),
      list(
        data_path = "nested_list$`Bevölkerung und Soziales`$Bevölkerungsstand$`Bevölkerungsverteilung nach Konfession`",
        indicator_names = "Anteil evang.-ref. Bevölkerung (%)",
        filter_values = "Evangelisch-reformiert",
        value_types = "share"
      ),
      list(
        data_path = "nested_list$`Bevölkerung und Soziales`$Bevölkerungsstand$`Bevölkerungsverteilung nach Konfession`",
        indicator_names = "Anteil röm.-kath. Bevölkerung (%)",
        filter_values = "Römisch-katholisch",
        value_types = "share"
      )
    )
  ),
  list(
    topic = "Bevölkerung",
    title = "Wohnbevölkerung nach Konfession",
    chart_type = "double_donut",
    data_path = "nested_list$`Bevölkerung und Soziales`$Bevölkerungsstand$`Bevölkerungsverteilung nach Konfession`"
  ),
  list(
    topic = "Bevölkerung",
    title = "Wohnbevölkerung, Vorjahresveränderung in %",
    chart_type = "single_line_zeitreihe",
    data_path = "nested_list$`Bevölkerung und Soziales`$Bevölkerungsentwicklung$`Bevölkerungsentwicklung (Vorjahr/5 Jahre)`",
    filter_value = "im Vorjahresvergleich",
    value_title = "Veränderung in %"
  ),
  list(
    topic = "Bevölkerung",
    title = "Wohnbevölkerung nach Nationalität",
    chart_type = "double_donut",
    data_path = "nested_list$`Bevölkerung und Soziales`$Bevölkerungsstand$`Bevölkerungsverteilung nach Nationalität`"
  ),
  list(
    topic = "Bevölkerung",
    title = "Bevölkerungsbewegungen",
    chart_type = "indicator_table",
    indicator_list = list(
      list(
        data_path = "nested_list$`Bevölkerung und Soziales`$Bevölkerungsbewegung$Lebendgeburten",
        indicator_names = "Geburten"
      ),
      list(
        data_path = "nested_list$`Bevölkerung und Soziales`$Bevölkerungsbewegung$Todesfälle",
        indicator_names = "Todesfälle"
      ),
      list(
        data_path = "nested_list$`Bevölkerung und Soziales`$Bevölkerungsbewegung$Geburtensaldo",
        indicator_names = "Geburtensaldo (Geburten minus Todesfälle)"
      ),
      list(
        data_path = "nested_list$`Bevölkerung und Soziales`$Bevölkerungsbewegung$Zuzüge",
        indicator_names = "Zuzüge Total",
        sum_up = TRUE
      ),
      list(
        data_path = "nested_list$`Bevölkerung und Soziales`$Bevölkerungsbewegung$Zuzüge",
        indicator_names = "- davon aus anderen Thurguer Gemeinden",
        filter_values = "aus anderen Gemeinden"
      ),
      list(
        data_path = "nested_list$`Bevölkerung und Soziales`$Bevölkerungsbewegung$Zuzüge",
        indicator_names = "- davon aus anderen Kantonen",
        filter_values = "aus anderen Kantonen"
      ),
      list(
        data_path = "nested_list$`Bevölkerung und Soziales`$Bevölkerungsbewegung$Zuzüge",
        indicator_names = "- davon aus dem Ausland",
        filter_values = "aus dem Ausland"
      ),
      list(
        data_path = "nested_list$`Bevölkerung und Soziales`$Bevölkerungsbewegung$Wegzüge",
        indicator_names = "Wegzüge Total",
        sum_up = TRUE
      ),
      list(
        data_path = "nested_list$`Bevölkerung und Soziales`$Bevölkerungsbewegung$Wegzüge",
        indicator_names = "- davon in andere Thurguer Gemeinden",
        filter_values = "in andere Gemeinden"
      ),
      list(
        data_path = "nested_list$`Bevölkerung und Soziales`$Bevölkerungsbewegung$Wegzüge",
        indicator_names = "- davon in andere Kantone",
        filter_values = "in andere Kantone"
      ),
      list(
        data_path = "nested_list$`Bevölkerung und Soziales`$Bevölkerungsbewegung$Wegzüge",
        indicator_names = "- davon ins Ausland",
        filter_values = "ins Ausland"
      ),
      list(
        data_path = "nested_list$`Bevölkerung und Soziales`$Bevölkerungsbewegung$Wanderungssaldo",
        indicator_names = "Wanderungssaldo (Zuzüge minus Wegzüge)"	,
        sum_up = TRUE
      )
    )
  ),
  list(
    topic = "Bevölkerung",
    title = "Wohnbevölkerung nach Altersgruppen",
    chart_type = "single_column",
    data_path = "nested_list$`Bevölkerung und Soziales`$Bevölkerungsstand$`Bevölkerungsverteilung nach Altersklasse`",
    value_title = "Anzahl"
  ),
  list(
    topic = "Bevölkerung",
    title = "Bevölkerungspyramide",
    chart_type = "double_pyramid",
    data_path = "additional_data$`Bevölkerung und Soziales`$`Bevölkerung nach Altersklasse Geschlecht`"
  ),
  list(
    topic = "Bevölkerung",
    title = "Zivilstandsänderungen",
    chart_type = "indicator_table",
    indicator_list = list(
      list(
        data_path = "nested_list$`Bevölkerung und Soziales`$Bevölkerungsbewegung$Heiraten",
        indicator_names = "Eheschliessungen"
      ),
      list(
        data_path = "nested_list$`Bevölkerung und Soziales`$Bevölkerungsbewegung$Scheidungen",
        indicator_names = "Scheidungen"
      )
    )
  )
)


#

# Bauen und Wohnen --------------------------------------------------------


bauen_wohnen_list <- list(
  list(
    topic = "Bauen und Wohnen",
    title = "Gebäudebestand mit Wohnnutzung",
    chart_type = "double_donut",
    data_path_path = "nested_list$`Bauen und Wohnen`$`Gebäude und Wohnungen`$`Wohngebäude nach Kategorie des Gebäudes`"
  ),
  list(
    topic = "Bauen und Wohnen",
    title = "Gebäudebestand mit Wohnnutzung",
    chart_type = "indicator_table",
    indicator_list = list(
      list(
        data_path ="nested_list$`Bauen und Wohnen`$`Gebäude und Wohnungen`$`Wohngebäude nach Kategorie des Gebäudes`",
        indicator_names = "Total Wohngebäude",
        sum_up = TRUE
      ),
      list(
        data_path ="nested_list$`Bauen und Wohnen`$`Gebäude und Wohnungen`$`Wohngebäude nach Kategorie des Gebäudes`",
        indicator_names = "- davon Einfamilienhäuser",
        filter_values = "Einfamilienhäuser"
      ),
      list(
        data_path ="nested_list$`Bauen und Wohnen`$`Gebäude und Wohnungen`$`Wohngebäude nach Kategorie des Gebäudes`",
        indicator_names = "- davon Mehrfamilienhäuser",
        filter_values = "Mehrfamilienhäuser"
      ),
      list(
        data_path ="nested_list$`Bauen und Wohnen`$`Gebäude und Wohnungen`$`Wohngebäude nach Kategorie des Gebäudes`",
        indicator_names = "- davon Gebäude mit teilweiser Wohnnutzung",
        filter_values = "Gebäude mit teilweiser Wohnnutzung"
      ),
      list(
        data_path ="nested_list$`Bauen und Wohnen`$`Gebäude und Wohnungen`$`Wohngebäude nach Kategorie des Gebäudes`",
        indicator_names = "- davon Wohngebäude mit Nebennutzung",
        filter_values = "Wohngebäude mit Nebennutzung"
      )
    )
  ),
  list(
    topic = "Bauen und Wohnen",
    title = "Wohnungen nach Zimmerzahl",
    chart_type = "single_column",
    data_path = "nested_list$`Bauen und Wohnen`$`Gebäude und Wohnungen`$`Wohnungen nach Zimmerzahl`",
    value_title = "Anzahl Wohnungen"
  ),
  list(
    topic = "Bauen und Wohnen",
    title = "Wohnungen nach Zimmerzahl",
    chart_type = "indicator_table",
    indicator_list = list(
      list(
        data_path ="nested_list$`Bauen und Wohnen`$`Gebäude und Wohnungen`$`Wohnungen total`",
        indicator_names = "Total Wohnungen"
      ),
      list(
        data_path ="nested_list$`Bauen und Wohnen`$`Gebäude und Wohnungen`$`Wohnungen nach Zimmerzahl`",
        indicator_names = "- davon 1 bis 2 Zimmer",
        filter_values = "1-2-Zimmerwohnungen"
      ),
      list(
        data_path ="nested_list$`Bauen und Wohnen`$`Gebäude und Wohnungen`$`Wohnungen nach Zimmerzahl`",
        indicator_names = "- davon 3 bis 4 Zimmer",
        filter_values = "3-4-Zimmerwohnungen"
      ),
      list(
        data_path ="nested_list$`Bauen und Wohnen`$`Gebäude und Wohnungen`$`Wohnungen nach Zimmerzahl`",
        indicator_names = "- davon 5 Zimmer",
        filter_values = "5-Zimmerwohnungen"
      ),
      list(
        data_path ="nested_list$`Bauen und Wohnen`$`Gebäude und Wohnungen`$`Wohnungen nach Zimmerzahl`",
        indicator_names = "- davon 6 oder mehr Zimmer	",
        filter_values = "Wohnungen mit 6 und mehr Zimmern"
      )
    )
  ),
  list(
    topic = "Bauen und Wohnen",
    title = "Gebäude mit Wohnnutzung nach Bauperiode",
    chart_type = "double_donut",
    data_path = "nested_list$`Bauen und Wohnen`$`Gebäude und Wohnungen`$`Wohngebäude nach Bauperiode`"
  ),
  list(
    topic = "Bauen und Wohnen",
    title = "Neu erstellte Wohnungen, in Anzahl Wohnungen",
    chart_type = "single_line_zeitreihe",
    data_path = "nested_list$`Bauen und Wohnen`$`Gebäude und Wohnungen`$`Neu erstellte Wohnungen total`",
    value_title = "Anzahl Wohnungen"
  ),
  list(
    topic = "Bauen und Wohnen",
    title = "Bauinvestitionen, in Mio. CHF",
    chart_type = "single_line_zeitreihe",
    data_path = "nested_list$`Bauen und Wohnen`$Bauinvestitionen$`Bauinvestitionen total`",
    value_title = "CHF in Mio."
  ),
  list(
    topic = "Bauen und Wohnen",
    title = "Bauinvestitionen nach Auftraggeber",
    chart_type = "double_donut",
    data_path = "nested_list$`Bauen und Wohnen`$Bauinvestitionen$`Bauinvestitionen nach Art der Auftraggeber`"
  ),
  list(
    topic = "Bauen und Wohnen",
    title = "Bauinvestitionen von privaten Auftraggebern nach Bauwerkskategorie",
    chart_type = "single_column",
    data_path = "nested_list$`Bauen und Wohnen`$Bauinvestitionen$`Bauinvestitionen nach Kategorie`",
    value_title = "CHF in Mio."
  ),
  list(
    topic = "Bauen und Wohnen",
    title = "Entwicklung der Leerwohnungsziffer, in %",
    chart_type = "single_line_zeitreihe",
    data_path = "nested_list$`Bauen und Wohnen`$`Leer stehende Wohnungen`$Leerwohnungsziffer",
    value_title = "Leerwohnungsziffer in %"
  )
)


# Haushalte ---------------------------------------------------------------


haushalte_list <- list(
  list(
    topic = "Haushalte",
    title = "Privathaushalte nach Anzahl Personen",
    chart_type = "indicator_table",
    indicator_list = list(
      list(
        data_path = "nested_list$`Bevölkerung und Soziales`$Haushalte$`Haushalte nach Haushaltsgrösse`",
        indicator_names = "Anzahl Haushalte",
        sum_up = TRUE
      ),
      list(
        data_path = "nested_list$`Bevölkerung und Soziales`$Haushalte$`Haushalte nach Haushaltsgrösse`",
        indicator_names = "- davon 1-Personen-Haushalte",
        filter_values = "1 Person"
      ),
      list(
        data_path = "nested_list$`Bevölkerung und Soziales`$Haushalte$`Haushalte nach Haushaltsgrösse`",
        indicator_names = "- davon 2-Personen-Haushalte",
        filter_values = "2 Personen"
      ),
      list(
        data_path = "nested_list$`Bevölkerung und Soziales`$Haushalte$`Haushalte nach Haushaltsgrösse`",
        indicator_names = "- davon 3-Personen-Haushalte",
        filter_values = "3 Personen"
      ),
      list(
        data_path = "nested_list$`Bevölkerung und Soziales`$Haushalte$`Haushalte nach Haushaltsgrösse`",
        indicator_names = "- davon 4-Personen-Haushalte",
        filter_values = "4 Personen"
      ),
      list(
        data_path = "nested_list$`Bevölkerung und Soziales`$Haushalte$`Haushalte nach Haushaltsgrösse`",
        indicator_names = "- davon 5-Personen-Haushalte",
        filter_values = "5 Personen"
      ),
      list(
        data_path = "nested_list$`Bevölkerung und Soziales`$Haushalte$`Haushalte nach Haushaltsgrösse`",
        indicator_names = "- davon Haushalte mit 6 oder mehr Personen",
        filter_values = "6 oder mehr Personen"
      )
    )
  ),
  list(
    topic = "Haushalte",
    title = "Privathaushalte nach Haushaltsgrösse",
    chart_type = "double_donut",
    data_path = "nested_list$`Bevölkerung und Soziales`$Haushalte$`Haushalte nach Haushaltsgrösse`"
  ),
  list(
    topic = "Haushalte",
    title = "Entwicklung Singlehaushalte im Zeitverlauf",
    chart_type = "single_line_zeitreihe",
    data_path = "nested_list$`Bevölkerung und Soziales`$Haushalte$`Haushalte nach Haushaltsgrösse`",
    filter_value = "1 Person",
    value_title = "Anzahl",
    share = TRUE
  )
)



# Öffentliche Finanzen ----------------------------------------------------



finanzen_list <- list(
  list(
    topic = "Öffentliche Finanzen",
    title = "Gemeindefinanzkennzahlen",
    chart_type = "indicator_table",
    indicator_list = list(
      list(
        data_path = "nested_list$`Staat und Politik`$Gemeindefinanzkennzahlen$Gemeindefinanzkennzahlen",
        indicator_names = "Selbstfinanzierungsgrad (%)",
        filter_values = "Selbstfinanzierungsgrad in %"
      ),
      list(
        data_path = "nested_list$`Staat und Politik`$Gemeindefinanzkennzahlen$Gemeindefinanzkennzahlen",
        indicator_names = "Selbstfinanzierungsanteil in %",
        filter_values = "Selbstfinanzierungsanteil in %"
      ),
      list(
        data_path = "nested_list$`Staat und Politik`$Gemeindefinanzkennzahlen$Gemeindefinanzkennzahlen",
        indicator_names = "Zinsbelastungsanteil (%)",
        filter_values = "Zinsbelastungsanteil in %"
      ),
      list(
        data_path = "nested_list$`Staat und Politik`$Gemeindefinanzkennzahlen$Gemeindefinanzkennzahlen",
        indicator_names = "Kapitaldienstanteil (%)",
        filter_values = "Kapitaldienstanteil in %"
      ),
      list(
        data_path = "nested_list$`Staat und Politik`$Gemeindefinanzkennzahlen$Gemeindefinanzkennzahlen",
        indicator_names = "Nettoschuld (+) bzw. -vermögen (-) pro Einw. (CHF)	",
        filter_values = "Nettoschuld (-) / Nettovermögen (+) pro Einwohner in Schweizer Franken"
      ),
      list(
        data_path = "nested_list$`Staat und Politik`$Gemeindefinanzkennzahlen$Gemeindefinanzkennzahlen",
        indicator_names = "Investitionsanteil (%)",
        filter_values = "Investitionsanteil in %"
      ),
      list(
        data_path = "nested_list$`Staat und Politik`$Gemeindefinanzkennzahlen$Gemeindefinanzkennzahlen",
        indicator_names = "Bruttoverschuldungsanteil (%)",
        filter_values = "Bruttoverschuldungsanteil in %"
      ),
      list(
        data_path = "nested_list$`Staat und Politik`$Gemeindefinanzkennzahlen$Gemeindefinanzkennzahlen",
        indicator_names = "Nettoverschuldungsquotient (%)",
        filter_values = "Nettoverschuldungsquotient in %"
      ),
      list(
        data_path = "nested_list$`Staat und Politik`$Gemeindefinanzkennzahlen$Gemeindefinanzkennzahlen",
        indicator_names = "Bilanzüberschussquotient (%)",
        filter_values = "Bilanzüberschussquotient in %"
      )

    )
  ),
  # list(
  #   topic = "Öffentliche Finanzen",
  #   title = "Finanzausgleich",
  #   chart_type = "indicator_table",
  #   indicator_list = list(
  #     list(
  #       data_path = "nested_list$`Staat und Politik`$Finanzausgleich$`Gesamtauswirkung Finanzausgleich (positive Werte: Abschöpfung, negative Werte: Auszahlung) (CHF)`",
  #       indicator_names = "Auszahlungen (-), Abschöpfungen (+) (CHF)"
  #     ),
  #     list(
  #       data_path = "nested_list$`Staat und Politik`$Finanzausgleich$`Gesamtauswirkung Finanzausgleich pro Einwohner (positive Werte: Abschöpfung, negative Werte: Auszahlung) (CHF)`",
  #       indicator_names = "Auszahlungen (-) bzw. Abschöpfungen (+) pro Einwohner (CHF pro Einw.)"
  #     )
  #   )
  # ),
  list(
    topic = "Öffentliche Finanzen",
    title = "Steuerfüsse",
    chart_type = "single_column",
    data_path = "nested_list$`Staat und Politik`$`Steuerkraft und Steuerfüsse`$Gesamtsteuerfuss",
    value_title = "in %"
  ),
  list(
    topic = "Öffentliche Finanzen",
    title = "Steuerfüsse",
    chart_type = "indicator_table",
    indicator_list = list(
      list(
        data_path = "nested_list$`Staat und Politik`$`Steuerkraft und Steuerfüsse`$Gemeindesteuerfuss",
        indicator_names = "Gemeindesteuerfuss (%)"
      ),
      list(
        data_path = "nested_list$`Staat und Politik`$`Steuerkraft und Steuerfüsse`$Gesamtsteuerfuss",
        indicator_names = "Gesamtsteuerfuss natürliche Personen evangelisch (%)",
        filter_values = "natürliche Personen evangelisch"
      ),
      list(
        data_path = "nested_list$`Staat und Politik`$`Steuerkraft und Steuerfüsse`$Gesamtsteuerfuss",
        indicator_names = "Gesamtsteuerfuss natürliche Personen katholisch (%)",
        filter_values = "natürliche Personen katholisch"
      ),
      list(
        data_path = "nested_list$`Staat und Politik`$`Steuerkraft und Steuerfüsse`$Gesamtsteuerfuss",
        indicator_names = "Gesamtsteuerfuss juristische Personen (%)",
        filter_values = "juristische Personen"
      ),
      list(
        data_path = "nested_list$`Staat und Politik`$`Steuerkraft und Steuerfüsse`$`Veränderung der Gemeindesteuerfüsse im Vergleich zu vor 10 Jahren (%-Punkte)`",
        indicator_names = "Veränderung des Gemeindesteuerfusses im Vergleich zu vor 10 Jahren (%-Punkte)"
      )
    )
  ),
  list(
    topic = "Öffentliche Finanzen",
    title = "Entwicklung des Gemeindesteuerfusses, in %",
    chart_type = "single_line_zeitreihe",
    data_path = "nested_list$`Staat und Politik`$`Steuerkraft und Steuerfüsse`$Gemeindesteuerfuss",
    value_title = "in %"
  )
)


# Soziales ----------------------------------------------------------------


soziales_list <- list(
  list(
    topic = "Soziales",
    title = "Sozialhilfeausgaben in CHF",
    chart_type = "indicator_table",
    indicator_list = list(
      list(
        data_path = "nested_list$`Bevölkerung und Soziales`$Sozialhilfe$`Brutto Sozialhilfeausgaben total`",
        indicator_names = "Brutto-Sozialhilfeausgaben (CHF)"
      ),
      list(
        data_path = "nested_list$`Bevölkerung und Soziales`$Sozialhilfe$`Netto Sozialhilfeausgaben total`",
        indicator_names = "Netto-Sozialhilfeausgaben (CHF)"
      ),
      list(
        data_path = "nested_list$`Bevölkerung und Soziales`$Sozialhilfe$`Netto Sozialhilfeausgaben je Einwohner`",
        indicator_names = "Netto-Sozialhilfeausgaben pro Einw. (CHF pro Einw.)"
      ),
      list(
        data_path = "nested_list$`Bevölkerung und Soziales`$Sozialhilfe$`Brutto Sozialhilfeausgaben je Einwohner`",
        indicator_names = "Brutto-Sozialhilfeausgaben pro Einw. (CHF pro Einw.)"
      )
    )
  ),
  list(
    topic = "Soziales",
    title = "Brutto-Sozialhilfeausgaben total",
    chart_type = "single_column_zeitreihe",
    data_path = "nested_list$`Bevölkerung und Soziales`$Sozialhilfe$`Brutto Sozialhilfeausgaben total`",
    value_title = "in CHF"
  ),
  list(
    topic = "Soziales",
    title = "Netto-Sozialhilfeausgaben total",
    chart_type = "single_column_zeitreihe",
    data_path = "nested_list$`Bevölkerung und Soziales`$Sozialhilfe$`Netto Sozialhilfeausgaben total`",
    value_title = "in CHF"
  ),
  list(
    topic = "Soziales",
    title = "Brutto-Sozialhilfeausgaben pro Einwohner",
    chart_type = "single_line_zeitreihe",
    data_path = "nested_list$`Bevölkerung und Soziales`$Sozialhilfe$`Brutto Sozialhilfeausgaben je Einwohner`",
    value_title = "in CHF/Einwohner"
  ),
  list(
    topic = "Soziales",
    title = "Netto-Sozialhilfeausgaben pro Einwohner",
    chart_type = "single_line_zeitreihe",
    data_path ="nested_list$`Bevölkerung und Soziales`$Sozialhilfe$`Netto Sozialhilfeausgaben je Einwohner`",
    value_title = "in CHF/Einwohner"
  ),
  list(
    topic = "Soziales",
    title = "Sozialhilfequote",
    chart_type = "single_line_zeitreihe",
    data_path = "nested_list$`Bevölkerung und Soziales`$Sozialhilfe$Sozialhilfequote",
    value_title = "in %"
  )
)


# Wirtschaft und Arbeit ---------------------------------------------------



wua_list <- list(
  list(
    topic = "Wirtschaft und Arbeit",
    title = "Arbeitsstätten nach Wirtschaftssektoren",
    chart_type = "double_donut",
    data_path = "nested_list$`Wirtschaft und Arbeit`$Arbeitsstätten$`Arbeitsstätten nach Sektor`"
  ),
  list(
    topic = "Wirtschaft und Arbeit",
    title = "Arbeitsstätten nach Branchen",
    chart_type = "indicator_table",
    indicator_list = list(
      list(
        data_path = "nested_list$`Wirtschaft und Arbeit`$Arbeitsstätten$`Arbeitsstätten total`",
        indicator_names = "Total Arbeitsstätten"
      ),
      list(
        data_path = "nested_list$`Wirtschaft und Arbeit`$Arbeitsstätten$`Arbeitsstätten nach Sektor`",
        indicator_names = "Land-/Forstwirtschaft (Sektor 1)",
        filter_values = "1"
      ),
      list(
        data_path = "nested_list$`Wirtschaft und Arbeit`$Arbeitsstätten$`Arbeitsstätten nach Sektor`",
        indicator_names = "Industrie, Gewerbe, Bau (Sektor 2)",
        filter_values = "2"
      ),
      list(
        data_path = "nested_list$`Wirtschaft und Arbeit`$Arbeitsstätten$`Arbeitsstätten nach Sektor`",
        indicator_names = "Dienstleistungen (Sektor 3)",
        filter_values = "3"
      )
    )
  ),
  list(
    topic = "Wirtschaft und Arbeit",
    title = "Beschäftigte (in Personen) nach Wirtschaftssektoren",
    chart_type = "double_donut",
    data_path = "nested_list$`Wirtschaft und Arbeit`$Beschäftigte$`Beschäftigte nach Sektor`"
  ),
  list(
    topic = "Wirtschaft und Arbeit",
    title = "Beschäftigte nach Branchen",
    chart_type = "indicator_table",
    indicator_list = list(
      list(
        data_path = "nested_list$`Wirtschaft und Arbeit`$Beschäftigte$`Beschäftigte total`",
        indicator_names = "Total Beschäftigte"
      ),
      list(
        data_path = "nested_list$`Wirtschaft und Arbeit`$Beschäftigte$`Beschäftigte nach Sektor`",
        indicator_names = "Land-/Forstwirtschaft (Sektor 1)",
        filter_values = "1"
      ),
      list(
        data_path = "nested_list$`Wirtschaft und Arbeit`$Beschäftigte$`Beschäftigte nach Sektor`",
        indicator_names = "Industrie, Gewerbe, Bau (Sektor 2)",
        filter_values = "2"
      ),
      list(
        data_path = "nested_list$`Wirtschaft und Arbeit`$Beschäftigte$`Beschäftigte nach Sektor`",
        indicator_names = "Dienstleistungen (Sektor 3)",
        filter_values = "3"
      )
    )
  ),
  list(
    topic = "Wirtschaft und Arbeit",
    title = "Beschäftigte (in Personen) nach Wirtschaftssektoren",
    chart_type = "single_column",
    data_path = "nested_list$`Wirtschaft und Arbeit`$Beschäftigte$`Vorjahresveränderung Beschäftigte nach Sektor (in %)`",
    value_title = "in %"
  )
)

# Raum --------------------------------------------------------------------

raum_list <- list(
  list(
    topic = "Raum",
    title = "Kennzahlen Raum",
    chart_type = "indicator_table",
    indicator_list = list(
      list(
        data_path = "nested_list$`Raum und Umwelt`$Flächennutzung$`Fläche total`",
        indicator_names = "Fläche (ha)"
      ),
      list(
        data_path = "nested_list$`Raum und Umwelt`$Flächennutzung$Landfläche",
        indicator_names = "Landfläche (ha)"
      )
    )
  ),
  list(
    topic = "Raum",
    title = "Bevölkerungsdichte",
    chart_type = "indicator_table",
    indicator_list = list(
      list(
        data_path = "nested_list$`Raum und Umwelt`$Flächennutzung$Bevölkerungsdichte",
        indicator_names = "Personen pro Hektar Landfläche (Einwohner/ha)	"
      )
    )
  ),
  list(
    topic = "Raum",
    title = "Nutzungsarten",
    chart_type = "double_donut",
    data_path ="nested_list$`Raum und Umwelt`$Flächennutzung$`Fläche nach Flächenart`"
  ),
  list(
    topic = "Raum",
    title = "Bevölkerungsdichte",
    chart_type = "single_line_zeitreihe",
    data_path = "nested_list$`Raum und Umwelt`$Flächennutzung$Bevölkerungsdichte"
  )
)
# Staat und Politik -------------------------------------------------------


pol_list <- list(
  list(
    topic = "Staat und Politik",
    title = "Grossratswahlen: Parteistärken und Wahlbeteiligung",
    chart_type = "indicator_table",
    indicator_list = list(
      list(
        data_path ="nested_list$`Staat und Politik`$Grossratswahlen$`Parteistärken nach Partei`",
        indicator_names = "Parteistärke SVP (%)",
        filter_values = "SVP"
      ),
      list(
        data_path ="nested_list$`Staat und Politik`$Grossratswahlen$`Parteistärken nach Partei`",
        indicator_names = "Parteistärke FDP (%)",
        filter_values = "FDP"
      ),
      list(
        data_path ="nested_list$`Staat und Politik`$Grossratswahlen$`Parteistärken nach Partei`",
        indicator_names = "Parteistärke Die Mitte (%)",
        filter_values = "Die Mitte"
      ),
      list(
        data_path ="nested_list$`Staat und Politik`$Grossratswahlen$`Parteistärken nach Partei`",
        indicator_names = "Parteistärke SP (%)",
        filter_values = "SP"
      ),
      list(
        data_path ="nested_list$`Staat und Politik`$Grossratswahlen$`Parteistärken nach Partei`",
        indicator_names = "Parteistärke GRÜNE (%)",
        filter_values = "GRÜNE"
      ),
      list(
        data_path ="nested_list$`Staat und Politik`$Grossratswahlen$`Parteistärken nach Partei`",
        indicator_names = "Parteistärke GLP (%)",
        filter_values = "GLP"
      ),
      list(
        data_path ="nested_list$`Staat und Politik`$Grossratswahlen$`Parteistärken nach Partei`",
        indicator_names = "Parteistärke EVP (%)",
        filter_values = "EVP"
      ),
      list(
        data_path ="nested_list$`Staat und Politik`$Grossratswahlen$`Parteistärken nach Partei`",
        indicator_names = "Parteistärke EDU (%)",
        filter_values = "EDU"
      ),
      list(
        data_path ="nested_list$`Staat und Politik`$Grossratswahlen$`Parteistärken nach Partei`",
        indicator_names = "Parteistärke Aufrecht Thurgau (%)",
        filter_values = "AUFTG"
      ),
      list(
        data_path ="nested_list$`Staat und Politik`$Grossratswahlen$Wahlbeteiligung",
        indicator_names = "Wahlbeteiligung (%)"
      )
    )
  ),
  list(
    topic = "Staat und Politik",
    title = "Nationalratswahlen: Parteistärken und Wahlbeteiligung",
    chart_type = "indicator_table",
    indicator_list = list(
      list(
        data_path ="nested_list$`Staat und Politik`$Nationalratswahlen$`Parteistärken nach Partei`",
        indicator_names = "Parteistärke SVP (%)",
        filter_values = "SVP"
      ),
      list(
        data_path ="nested_list$`Staat und Politik`$Nationalratswahlen$`Parteistärken nach Partei`",
        indicator_names = "Parteistärke FDP (%)",
        filter_values = "FDP"
      ),
      list(
        data_path ="nested_list$`Staat und Politik`$Nationalratswahlen$`Parteistärken nach Partei`",
        indicator_names = "Parteistärke Die Mitte (%)",
        filter_values = "Die Mitte"
      ),
      list(
        data_path ="nested_list$`Staat und Politik`$Nationalratswahlen$`Parteistärken nach Partei`",
        indicator_names = "Parteistärke SP (%)",
        filter_values = "SP"
      ),
      list(
        data_path ="nested_list$`Staat und Politik`$Nationalratswahlen$`Parteistärken nach Partei`",
        indicator_names = "Parteistärke GRÜNE (%)",
        filter_values = "GRÜNE"
      ),
      list(
        data_path ="nested_list$`Staat und Politik`$Nationalratswahlen$`Parteistärken nach Partei`",
        indicator_names = "Parteistärke GLP (%)",
        filter_values = "GLP"
      ),
      list(
        data_path ="nested_list$`Staat und Politik`$Nationalratswahlen$`Parteistärken nach Partei`",
        indicator_names = "Parteistärke EVP (%)",
        filter_values = "EVP"
      ),
      list(
        data_path ="nested_list$`Staat und Politik`$Nationalratswahlen$`Parteistärken nach Partei`",
        indicator_names = "Parteistärke EDU (%)",
        filter_values = "EDU"
      ),
      list(
        data_path ="nested_list$`Staat und Politik`$Nationalratswahlen$Wahlbeteiligung",
        indicator_names = "Wahlbeteiligung (%)"
      )
    )
  ),
  list(
    topic = "Staat und Politik",
    title = "Grossratswahlen: Parteistärke SVP",
    chart_type = "single_line_zeitreihe",
    data_path ="nested_list$`Staat und Politik`$Grossratswahlen$`Parteistärken nach Partei`",
    filter_value = "SVP",
    value_title = "in %"
  ),
  list(
    topic = "Staat und Politik",
    title = "Nationalratswahlen: Parteistärke SVP",
    chart_type = "single_line_zeitreihe",
    data_path ="nested_list$`Staat und Politik`$Grossratswahlen$`Parteistärken nach Partei`",
    filter_value = "SVP",
    value_title = "in %"

  ),
  list(
    topic = "Staat und Politik",
    title = "Grossratswahlen: Parteistärke SP",
    chart_type = "single_line_zeitreihe",
    data_path ="nested_list$`Staat und Politik`$Grossratswahlen$`Parteistärken nach Partei`",
    filter_value = "SP",
    value_title = "in %"

  ),
  list(
    topic = "Staat und Politik",
    title = "Nationalratswahlen: Parteistärke SP",
    chart_type = "single_line_zeitreihe",
    data_path ="nested_list$`Staat und Politik`$Grossratswahlen$`Parteistärken nach Partei`",
    filter_value = "SP"
  ),
  list(
    topic = "Staat und Politik",
    title = "Grossratswahlen: Parteistärke FDP",
    chart_type = "single_line_zeitreihe",
    data_path ="nested_list$`Staat und Politik`$Grossratswahlen$`Parteistärken nach Partei`",
    filter_value = "FDP",
    value_title = "in %"

  ),
  list(
    topic = "Staat und Politik",
    title = "Nationalratswahlen: Parteistärke FDP",
    chart_type = "single_line_zeitreihe",
    data_path ="nested_list$`Staat und Politik`$Grossratswahlen$`Parteistärken nach Partei`",
    filter_value = "FDP",
    value_title = "in %"

  ),
  list(
    topic = "Staat und Politik",
    title = "Grossratswahlen: Parteistärke Die Mitte",
    chart_type = "single_line_zeitreihe",
    data_path ="nested_list$`Staat und Politik`$Grossratswahlen$`Parteistärken nach Partei`",
    filter_value = "Die Mitte",
    value_title = "in %"

  ),
  list(
    topic = "Staat und Politik",
    title = "Nationalratswahlen: Parteistärke Die Mitte",
    chart_type = "single_line_zeitreihe",
    data_path ="nested_list$`Staat und Politik`$Grossratswahlen$`Parteistärken nach Partei`",
    filter_value = "Die Mitte",
    value_title = "in %"

  ),
  list(
    topic = "Staat und Politik",
    title = "Grossratswahlen: Parteistärke EDU",
    chart_type = "single_line_zeitreihe",
    data_path ="nested_list$`Staat und Politik`$Grossratswahlen$`Parteistärken nach Partei`",
    filter_value = "EDU",
    value_title = "in %"

  ),
  list(
    topic = "Staat und Politik",
    title = "Nationalratswahlen: Parteistärke EDU",
    chart_type = "single_line_zeitreihe",
    data_path ="nested_list$`Staat und Politik`$Grossratswahlen$`Parteistärken nach Partei`",
    filter_value = "EDU",
    value_title = "in %"

  ),
  list(
    topic = "Staat und Politik",
    title = "Grossratswahlen: Parteistärke EVP",
    chart_type = "single_line_zeitreihe",
    data_path ="nested_list$`Staat und Politik`$Grossratswahlen$`Parteistärken nach Partei`",
    filter_value = "EVP"
  ),
  list(
    topic = "Staat und Politik",
    title = "Nationalratswahlen: Parteistärke EVP",
    chart_type = "single_line_zeitreihe",
    data_path ="nested_list$`Staat und Politik`$Grossratswahlen$`Parteistärken nach Partei`",
    filter_value = "EVP",
    value_title = "in %"

  ),
  list(
    topic = "Staat und Politik",
    title = "Grossratswahlen: Parteistärke GLP",
    chart_type = "single_line_zeitreihe",
    data_path ="nested_list$`Staat und Politik`$Grossratswahlen$`Parteistärken nach Partei`",
    filter_value = "GLP",
    value_title = "in %"

  ),
  list(
    topic = "Staat und Politik",
    title = "Nationalratswahlen: Parteistärke GLP",
    chart_type = "single_line_zeitreihe",
    data_path ="nested_list$`Staat und Politik`$Grossratswahlen$`Parteistärken nach Partei`",
    filter_value = "GLP",
    value_title = "in %"

  ),
  list(
    topic = "Staat und Politik",
    title = "Grossratswahlen: Parteistärke GRÜNE",
    chart_type = "single_line_zeitreihe",
    data_path ="nested_list$`Staat und Politik`$Grossratswahlen$`Parteistärken nach Partei`",
    filter_value = "GRÜNE",
    value_title = "in %"

  ),
  list(
    topic = "Staat und Politik",
    title = "Nationalratswahlen: Parteistärke GRÜNE",
    chart_type = "single_line_zeitreihe",
    data_path ="nested_list$`Staat und Politik`$Grossratswahlen$`Parteistärken nach Partei`",
    filter_value = "GRÜNE",
    value_title = "in %"

  )

)

structure_list <- c(finanzen_list,haushalte_list,bauen_wohnen_list,bevölkerung_list,soziales_list,pol_list,raum_list,wua_list)

