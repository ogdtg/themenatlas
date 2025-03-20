# Test Dashboard


# Header
db_header <- init_header(dashboard_title = "Test Dashboard",reference='https://statistik.tg.ch')




# Initialize Dashboard Body
db_body <- init_body(db_content)

# Initialize Dashboard Sidebar
db_sidebar <- init_sidebar(sidebar_content)


# Create UI
ui <- dashboardPage(
  title = "Test Dashboard",
  dark = NULL,
  help = TRUE, # show popover
  header = db_header,
  sidebar = db_sidebar,
  body = db_body
)

# Server Content

# Am besten wird jeder Tab in einem einzelnen Script definiert, welches dann im "R/" Ordner abgelegt wird (Beispiel tab 2)
# Darin sind dann alles Funktionen enthalten, die für den Tab von Nöten sind
# Es ist zu beachten, dass es auch globale Element gibt, welche dann direkt im Server Teil definiert werden müssen und nicht ausgelagfert werden können
# z.B. Zuordnung eines gerenderten Outputs zum entsprechenden OPutput Element



# Server ------------------------------------------------------------------


server <- function(input, output, session) {



  ## Tab 1 ------------------------------------------------------------


  ### Reactive Variablen initialisieren ------------------------------------------------------------

  previous_gemeinde <- reactiveVal(NULL)
  prevYear <- reactiveVal("")
  prevFilter <- reactiveVal(NULL)
  prevIndicator <- reactiveVal("")
  prevValueType <- reactiveVal(NULL)
  prevTab <- reactiveVal(NULL)
  bezirk_data_compare <- reactiveVal(bezirk_data_mod)
  selected_base_area <- reactiveVal("4566")
  selected_compare_area <- reactiveVal("4671")
  geo_data <- reactiveVal(gemeindegrenzen)

  ### Check conditions function -----------------------------------------------

  check_conditions <- reactive({
    req(selected_data())  # Ensure selected_data() is not NULL

    check_conditions_func(input, selected_data,prevYear,prevValueType,prevIndicator,prevFilter)

  })


  observeEvent(input$area,{
    if (input$area == "Gemeinde") {

      geo_data(gemeindegrenzen)


    } else if (input$area == "Bezirk") {
      if (!exists("bezirksgrenzen")) {
        bezirksgrenzen <- readRDS("data/bezirksgrenzen.rds")
        geo_data(bezirksgrenzen)
      } else {
        geo_data(bezirksgrenzen)
      }

    } else if (input$area == "Primarschulgemeinde") {
      if (!exists("psg")) {
        psg <- readRDS("data/psg.rds")
        geo_data(psg)
      } else {
        geo_data(psg)
      }

    } else if (input$area == "Sekundarschulgemeinde") {
      if (!exists("ssg")) {
        ssg <- readRDS("data/ssg.rds")
        geo_data(ssg)
      } else {
        geo_data(ssg)
      }
    } else if (input$area == "Volksschulgemeinde") {
      if (!exists("vsg")) {
        vsg <- readRDS("data/vsg.rds")
        geo_data(vsg)
      } else {
        geo_data(vsg)
      }
    }
  },ignoreInit = FALSE)
  ### Screen width ------------------------------------------------------------



  # output$test <- renderText({
  #   input$screen_width
  # })


  ### selected_data auswählen ------------------------------------------------------------

  # Reactive object for selected dataset
  selected_data <- reactive({
    req(input$topic, input$subtopic, input$indicator)
    nested_list[[input$topic]][[input$subtopic]][[input$indicator]]
  })



  ### Subtopic updaten ------------------------------------------------------------

  # Update Subtopics when Topic changes
  observeEvent(input$topic, {
    updateSelectizeInput(session, "subtopic", choices = names(nested_list[[input$topic]]), selected = NULL)

  })

  ### Indicator updaten ------------------------------------------------------------


  # Update Indicators when Subtopic changes
  observeEvent(input$subtopic, {
    updateSelectizeInput(session, "indicator", choices = names(nested_list[[input$topic]][[input$subtopic]]), selected = NULL)

  })


  ### Jahr, Filter und gemeinde updaten ------------------------------------------------------------

  render_selections_dynamic(session,input,output,selected_data)

  render_filter_ui(session,input,output,selected_data)


  ### Jahr updaten basierend auf Filter ---------------------------------------

  update_year_on_filter(session,input,output,selected_data)



  ### Basiskarte initialisieren ---------------------------------------

  init_map(output,input,geo_data)



  ### Wenn Screen zu schmal evt updaten ---------------------------------------





  ### Tabelle und Karte updaten -----------------------------------------------


  # modify_map("map",color_map,geo_data)
  modify_map(
    session=session,
    input=input,
    selected_data = selected_data,
    geo_data = geo_data(),
    palette_ds = palette_ds,
    palette_ds_alternative = palette_ds_alternative,
    check_conditions = check_conditions
  )

  modify_table(session,input,output,selected_data,check_conditions)

  render_hc_summary(session,input,output,check_conditions,selected_data)


  ### Ausgewählte Gemeinde per Klick updaten ----------------------------------

  update_gemeinde_selection_on_click(session,input)



  ### Zoom auf Gemeinde ----------------------------------

  zoom_and_zoom_reset(session,input,previous_gemeinde,geo_data())



  ### Summary filter updaten --------------------------------------------------


  update_summary_filter(session, input,selected_data)


  render_hc_summary(session, input, output, selected_data, check_conditions, bezirk_data)


  ## Tab 2 ------------------------------------------------------------



  update_compare_area(
    session,
    input,
    bezirk_data2,
    bezirk_data_compare,
    selected_compare_area,
    selected_base_area
  )

  update_selected_compare_area(input, selected_compare_area)

  structure_list_reactive <- reactiveVal(NULL)
  render_topic_ui(session,
                  output,
                  input,
                  structure_list_reactive,
                  renderedTopics)





  change_at_report_topic(session, output, input, selected_base_area, selected_compare_area, bezirk_data_names2, structure_list_reactive)
  change_at_compare_area(session, output, input, selected_base_area, selected_compare_area, bezirk_data_names2, structure_list_reactive)
  change_at_base_area(session, output, input, selected_base_area, selected_compare_area, bezirk_data_names2, structure_list_reactive)



  ## Tab 3 -------------------------------------------------------------------


  # Reactive object for selected dataset
  selected_data_serv <- reactive({
    req(input$self_service_topic, input$self_service_subtopic, input$self_service_indicator)
    nested_list[[input$self_service_topic]][[input$self_service_subtopic]][[input$self_service_indicator]]
  })

  # Update Subtopics when Topic changes
  observeEvent(input$self_service_topic, {
    updateSelectizeInput(session, "self_service_subtopic", choices = names(nested_list[[input$self_service_topic]]), selected = NULL)

  })

  ### Indicator updaten ------------------------------------------------------------


  # Update Indicators when Subtopic changes
  observeEvent(input$self_service_subtopic, {
    updateSelectizeInput(session, "self_service_indicator", choices = names(nested_list[[input$self_service_topic]][[input$self_service_subtopic]]), selected = NULL)

  })

  ### Jahr, Filter und gemeinde updaten ------------------------------------------------------------

  render_selections_dynamic_self_service(session,input,output,selected_data_serv)

  render_filter_ui_self_service(session,input,output,selected_data_serv)


  ### Jahr updaten basierend auf Filter ---------------------------------------

  update_year_on_filter_self_service(session,input,output,selected_data_serv)

  download_data <- reactiveVal(bezirk_data)
  add_selection(session,input,output,selected_data_serv,download_data)



  # Tab 4 -------------------------------------------------------------------

  uploaded_data <- reactiveVal(NULL)


  # Daten hochladen und einlesen
  upload_external_data(session,input,output,uploaded_data)


  # Create Radio Buttons
  create_var_type_radio_buttons(session,input,output)

  # Function to generate pattern file
  generate_xlsx_pattern(output,bezirk_data)




  # Base Map
  draw_base_map(output,gemeindegrenzen)

  # Karte neu einfärben basieredn auf hochgeladenenen Daten
  update_map_with_custom_data(session,input,gemeindegrenzen,uploaded_data)



}


# Run app
shinyApp(ui = ui, server = server)



