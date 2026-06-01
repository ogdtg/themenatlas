# Thurgauer Themenatlas – Shiny-Konzept

## Architektur

```
themenatlas_tg/
├── app.R              # Haupt-App (UI + Server)
├── R/
│   ├── indikatoren.R  # Indikatorenstruktur + Metadaten
│   ├── daten.R        # Datenlader (API / CSV)
│   ├── karte.R        # Leaflet-Hilfsfunktionen
│   └── ui_helpers.R   # Wiederverwendbare UI-Komponenten
├── data/
│   ├── gemeinden.geojson   # Gemeindegrenzen Thurgau (von swisstopo)
│   └── indikatoren.csv     # Metadaten alle Indikatoren
└── www/
    └── custom.css          # Wenige Overrides
```

## Packages

```r
install.packages(c(
  "shiny",       # Framework
  "bslib",       # Bootstrap 5 Layout
  "bsicons",     # Bootstrap Icons
  "leaflet",     # Karte
  "sf",          # Geodaten (GeoJSON)
  "ggplot2",     # Zeitreihen / Charts
  "dplyr",       # Datentransformation
  "DT",          # Interaktive Tabellen
  "openxlsx",    # Excel-Export
  "thematic"     # Plot-Theming
))
```

## Datenquellen

- **Gemeindegrenzen**: swisstopo swissBOUNDARIES3D (GeoJSON, kostenlos)
  https://www.swisstopo.admin.ch/de/landschaftsmodell-swissboundaries3d

- **Statistikdaten**: Amt für Daten und Statistik Kanton Thurgau
  - REST API: https://data.tg.ch (OGD-Portal)
  - Oder direkt: themenatlas-tg.ch API-Endpunkte (via Browser DevTools ermitteln)

## 5 Tabs – Kurzbeschrieb

| Tab | Zweck |
|-----|-------|
| **Karte** | Choroplethenkarte + Jahresslider + Ranking-Sidebar |
| **Vergleich** | Zwei Gemeinden nebeneinander, alle Indikatoren |
| **Zeitreihe** | Mehrere Gemeinden als Liniendiagramm |
| **Tabelle** | Alle 80 Gemeinden, sortierbar + CSV/Excel-Export |
| **Info** | Dokumentation, Quellen, Übersicht |

## Schlüsselentscheidungen

### Choroplethenkarte
- **Leaflet** mit GeoJSON-Gemeindegrenzen (nicht Punktmarker)
- Farbpalette wählbar (sequenziell / divergierend)
- Jahresslider mit Play-Button direkt unter Karte

### Indikatornavigation
- Accordion in Sidebar: Thema → Gruppe → Indikator
- **Freitextsuche** filtert über alle ~190 Indikatoren
- Suchresultate ersetzen vorübergehend den Accordion-Baum

### Indikator-Aggregation (Empfehlung)
Absolute + Anteils-Indikatoren zusammenfassen:
```r
# Statt zwei separate Indikatoren:
# "Ausländische Bevölkerung" (absolut)  +  "Anteil ausländische Bevölkerung" (%)
# → Ein Indikator mit Toggle:
input_switch("zeige_anteil", "Als Anteil (%)", value = TRUE)
```

### Zeitreihen-Tab
- Bis 5 Gemeinden gleichzeitig
- Optionaler Kantonsdurchschnitt als gestrichelte Linie
- Download als PNG / SVG

## Weiterentwicklung

1. **GeoJSON einbinden**: `sf::read_sf("data/gemeinden.geojson")` und
   `leaflet() |> addPolygons(data = gemeinden_sf, ...)`

2. **Echte API-Anbindung**: OGD-Portal data.tg.ch via `httr2`

3. **URL-State**: `shiny::updateQueryString()` – damit Karten-Links
   direkt geteilt werden können (Indikator + Jahr in URL kodiert)

4. **Caching**: `memoise` für wiederholte API-Calls

5. **Accessibility**: Karte mit Textalternative (Tabelle) für
   Screenreader-Nutzer
