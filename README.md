# 🌍 **Themenatlas**  

Dieses Repository enthält den Code für die **Themenatlas**-Shiny-App, die verschiedene statistische Daten in einer interaktiven Webanwendung visualisiert. Die App lädt, verarbeitet und präsentiert Daten in Form von Karten, Tabellen und Diagrammen.  

📌 **Hauptfunktionen der App:**  
✔ Dynamische Datenvisualisierung mit **Highcharts** und **Leaflet**  
✔ Interaktive Datenfilterung und -auswahl  
✔ Erstellung benutzerdefinierter Tabellen mit exportierbaren Indikatoren  
✔ Integration externer Datenquellen über **GitHub Actions**  

---  

## 📂 **Dateibeschreibung** (Deutsch)  

### **`02_prepare_helper_data.R`**  
Dieses Skript lädt und verarbeitet alle notwendigen Daten für die Shiny-App. Es liest verschiedene Datensätze ein, darunter geografische und statistische Daten, führt Transformationen durch und erstellt benannte Zeichenfolgen für die Visualisierung. Zusätzlich werden Farbpaletten für die Karten definiert und externe Daten über **GitHub Actions** aktualisiert.  

### **`03_ui_db_content.R`**  
Dieses Skript definiert die **Benutzeroberfläche (UI)** für die verschiedenen Tabs der Shiny-App. Es strukturiert die **Seitenleiste** und das **Dashboard-Layout** und integriert interaktive Komponenten wie Karten, Tabellen, Diagramme und Upload-Funktionen. Die UI-Elemente werden dynamisch basierend auf den Benutzereingaben generiert.  

### **`02_functions.R`**  
Dieses Skript enthält allgemeine **Hilfsfunktionen** für die App, darunter:  
✔ Funktionen zur Anpassung von **Leaflet-Karten** (Stile & Labels)  
✔ Eine Funktion zur **Text-Normalisierung** für einheitliche Datenverarbeitung  
✔ Weitere Utility-Funktionen zur Optimierung der **Interaktivität** und **Datenverarbeitung**  

### **`05_create_structure_list.R`**  
Erstellt eine **strukturierte Liste** mit **Metadaten** zu allen Diagrammen und Tabellen im **Report-Tab (Tab 2)**.  
✔ **Datenquellen** werden aus `data/data_source_list.rds` geladen  
✔ Themen wie **Bevölkerung, Wohnen, Wirtschaft, Politik & Soziales** werden organisiert  
✔ Jedes Element enthält **Informationen zu Thema, Diagrammtyp, Datenpfad & relevanten Indikatoren**  

### **`05_tab2_create_datatable.R`**  
Dieses Skript generiert **interaktive Datentabellen** (`DT`-Objekte) basierend auf `structure_list`. Es verarbeitet Daten **dynamisch** und formatiert sie als:  
✔ **Indikatortabellen** mit oder ohne prozentuale Anteile  
✔ **Zeitreihen-Tabellen** für Trends  
✔ **Bevölkerungspyramiden-Tabellen**  
✔ Tabellen mit **individuellen Filter- & Download-Optionen**  

### **`05_tab2_create_highchart.R`**  
Generiert **interaktive Diagramme** mit **Highcharts**, darunter:  
✔ **Donut-Diagramme**  
✔ **Linien- & Balkendiagramme**  
✔ **Bevölkerungspyramiden**  
✔ Dynamische **Farbzuweisungen** für bessere Lesbarkeit  

### **`05_tab2_render_content.R`**  
Dieses Skript steuert das **Rendering von Highcharts, Tabellen & Datenquellen**.  
✔ **Automatische Aktualisierung** basierend auf Benutzereingaben  
✔ Unterstützung für **verschiedene Diagrammtypen** (Linie, Balken, Donut, Pyramide)  
✔ **Links zu externen Datenquellen** passend zum aktuellen Dataset  

### **`05_tab2_render_ui.R`**  
Generiert und rendert die UI-Elemente im **Report-Tab (Tab 2)**.  
✔ **Anpassbare Layouts** für Diagramme, Tabellen & Indikatoren  
✔ **Interaktive Steuerung** je nach **Jahresauswahl & Vergleichsregion**  
✔ **Optimierte Darstellung für Shiny-Apps**  

### **`05_tab2_functions.R`**  
Das zentrale **Steuerungsskript** für das Rendering von **UI & Inhalt**.  
✔ Dynamische **Aktualisierung & Anzeige von Diagrammen & Tabellen**  
✔ Reaktive Steuerung basierend auf **Benutzerauswahl**  
✔ Integration der anderen **`05_tab2_*`-Skripte**  

### **`06_tab3_functions.R`**  
Funktionen für den **Daten-Service**:  
✔ Benutzer können **eigene Tabellen mit ausgewählten Indikatoren** erstellen  
✔ Dynamische **Filterung nach Jahr & Wertetyp**  
✔ **Geplante Exportfunktion** für verschiedene **Dateiformate**  

### **`06_tab3_ui.R`**  
Definiert die **UI für den Daten-Self-Service**:  
✔ Dynamische Aktualisierung von **Filterauswahl & Jahren**  
✔ Interaktive **Datenfilterung & Verfeinerung der Indikatoren**  

### **`07_tab4_functions.R`**  
Funktionen zur **Kartenvisualisierung hochgeladener Benutzerdaten**:  
✔ **CSV- & Excel-Upload**  
✔ Auswahl relevanter **Join-Spalten** zur Verbindung mit **Geodaten**  
✔ Dynamische **Darstellung numerischer & kategorialer Werte**  
✔ **Fehlermeldungen für ungültige Eingaben**  

### **`init_ui.R`**  
Initialisiert die **UI-Komponenten der Shiny-App**, darunter:  
✔ **Header, Sidebar & Dashboard-Body**  
✔ Integration von **JavaScript-Funktionen** für **Tracking, Tooltips & Responsive Design**  

---

# 📂 **File Descriptions** (English)  

### **`02_prepare_helper_data.R`**  
Loads and processes all necessary data for the Shiny app. Reads various datasets, including geographic and statistical data, applies transformations, and creates named character vectors for visualization. Also defines color palettes for maps and updates external data via **GitHub Actions**.  

### **`03_ui_db_content.R`**  
Defines the **user interface (UI)** elements for different tabs in the Shiny app. Structures **sidebar navigation & dashboard content**, including interactive components such as **maps, tables, charts & upload options**. UI elements are dynamically generated based on user input.  

### **`02_functions.R`**  
General **utility functions**, including:  
✔ **Leaflet map styling & label adjustments**  
✔ **Text normalization** for consistent data processing  
✔ **Other helper functions** for improved interactivity  

### **`05_create_structure_list.R`**  
Creates a **structured list of metadata** for all graphs and tables in **Report Tab (Tab 2)**.  
✔ **Loads data sources** from `data/data_source_list.rds`  
✔ Organizes topics like **population, housing, economy, politics & social welfare**  
✔ Each entry contains **topic, chart type, data path & relevant indicators**  

### **`05_tab2_create_datatable.R`**  
Generates **interactive data tables** (`DT` objects) based on `structure_list`. Supports:  
✔ **Indicator tables** (with or without percentage shares)  
✔ **Time series tables** for trend analysis  
✔ **Population pyramid tables**  
✔ **Custom filtering & download options**  

### **`05_tab2_create_highchart.R`**  
Generates **interactive charts** using **Highcharts**, including:  
✔ **Donut charts**  
✔ **Line & bar charts**  
✔ **Population pyramids**  
✔ **Dynamic color mapping** for better readability  

### **`05_tab2_render_content.R`**  
Manages **rendering of Highcharts, tables & data sources**:  
✔ **Automatic updates** based on user selections  
✔ Supports **various chart types** (line, bar, donut, pyramid)  
✔ **Links to external data sources**  

### **`05_tab2_render_ui.R`**  
Generates and renders **UI elements in Report Tab (Tab 2)**:  
✔ **Custom layouts** for charts, tables & indicators  
✔ **Dynamic controls** based on **year & comparison area**  
✔ **Optimized display for Shiny apps**  

### **`05_tab2_functions.R`**  
Main **control script** for **UI & content rendering**:  
✔ **Dynamically updates & displays charts & tables**  
✔ Reactively adjusts content based on **user selections**  
✔ **Integrates all `05_tab2_*` scripts**  

### **`06_tab3_functions.R`**  
Functions for the **data service**:  
✔ Users can **create custom tables with selected indicators**  
✔ **Dynamic filtering** by year & value type  
✔ **Planned export functionality** for multiple file formats  

### **`06_tab3_ui.R`**  
Defines the **UI for self-service data selection**:  
✔ **Dynamic filter & year selection**  
✔ Interactive **data filtering & refinement**  

### **`07_tab4_functions.R`**  
Functions for **mapping user-uploaded data**:  
✔ **CSV & Excel upload**  
✔ Selection of **join columns** for merging with **geo-data**  
✔ **Dynamic visualization of numeric & categorical values**  
✔ **Error handling for invalid inputs**  

### **`init_ui.R`**  
Initializes **UI components for the Shiny app**, including:  
✔ **Header, Sidebar & Dashboard Body**  
✔ **JavaScript functions for tracking, tooltips & responsive design**
