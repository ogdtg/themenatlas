#' 06_tab3_ui.R
#'
#' This script defines the UI elements for the data self-service tab, allowing users to select indicators
#' and customize data tables dynamically. It updates available years, filters, and value types based on
#' user selections.
#'
#' Functionality:
#' - Updates the available years dynamically based on the selected dataset.
#' - Renders UI components for filtering data by indicators, years, and value types.
#' - Ensures that users only select relevant filters for the chosen data.
#'
#' Dependencies:
#' - `shiny`, `dplyr`
#'
#' Output:
#' - UI elements for selecting filters, years, and value types dynamically.
#' - Reactive updates to the selection interface.




#' Render dynamic selections for the data self-service tab
#'
#' This function updates the available years based on the selected dataset and ensures that
#' the latest available year is selected by default.
#'
#' @param session The Shiny session object.
#' @param input The Shiny input object.
#' @param output The Shiny output object.
#' @param selected_data_serv A reactive expression providing the available dataset.
render_selections_dynamic_self_service <- function(session,input,output,selected_data_serv){

  observeEvent(selected_data_serv(), {
    df <- selected_data_serv()



    if ("filter1" %in% colnames(df) && input$filter1!="Kein Filter") {
      df <- df %>% filter(filter1 %in% input$filter1) %>%
        filter(!is.na(value))
    }

    # ✅ Get the newest year available
    latest_year <- max(df$jahr, na.rm = TRUE)

    updateSelectizeInput(session, "self_service_year",
                         choices = unique(df$jahr),
                         selected = latest_year)  # ⬅️ Automatically selects the latest year

    # updateSelectizeInput(session, "bfs_nr_gemeinde",
    #                      choices = setNames(bezirk_data$bfs_nr_gemeinde,bezirk_data$name_gemeinde),
    #                      selected = "")  # ⬅️ No default Gemeinde selected

  })


}


#' Render the filter UI dynamically for the data self-service tab
#'
#' This function creates a filter selection dropdown if applicable, based on the dataset.
#'
#' @param session The Shiny session object.
#' @param input The Shiny input object.
#' @param output The Shiny output object.
#' @param selected_data_serv A reactive expression providing the available dataset.
render_filter_ui_self_service <- function(session,input,output,selected_data_serv){

  observeEvent(selected_data_serv(), {
    df <- selected_data_serv()


    # ✅ Dynamically render UI only if "filter1" exists
    output$self_service_filter_ui <- renderUI({
      if ("filter1" %in% colnames(df)) {
        selectizeInput(session$ns("self_service_filter1"), "Filter", choices = unique(df$filter1), selected = unique(df$filter1)[1])
      } else {
        selectizeInput(session$ns("self_service_filter1"), "Filter", choices ="Kein Filter", selected = "Kein Filter",options = list(create = FALSE,
                                                                                                                        onDelete = I("function() { return false; }"),  # Prevent deletion
                                                                                                                        plugins = list("restore_on_backspace")))
      }
    })
  })

}


#' Update available years when a filter is selected
#'
#' This function updates the list of available years based on the selected filter
#' and dynamically selects the latest year.
#'
#' @param session The Shiny session object.
#' @param input The Shiny input object.
#' @param output The Shiny output object.
#' @param selected_data_serv A reactive expression providing the available dataset.
update_year_on_filter_self_service <- function(session,input,output,selected_data_serv){
  observeEvent(input$self_service_filter1, {
    df <- selected_data_serv()

    # Filter years based on selected filter1
    if ("filter1" %in% colnames(df) && input$self_service_filter1!="Kein Filter") {
      df <- df %>% filter(filter1 %in% input$self_service_filter1) %>%
        filter(!is.na(value))
    }

    # Update year choices dynamically
    updateSelectizeInput(session, session$ns("self_service_year"),
                         choices = unique(df$jahr),
                         selected = max(df$jahr, na.rm = TRUE))  # Select latest year
  })

  # Dynamically render filter UI (only if filter1 exists)
  output$self_service_radio_bas_perc <- renderUI({
    df <- selected_data_serv()
    if ("share" %in% colnames(df)) {
      radioButtons(session$ns("self_service_value_type"),"",choices = c("Absolut","Prozentual"),selected = "Absolut")
    } else {
      radioButtons(session$ns("self_service_value_type"),"",choices = c("Absolut"),selected = "Absolut")
    }
  })
}
