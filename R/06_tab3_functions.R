#' 06_tab3_functions.R
#'
#' This script provides functions for the data service feature, allowing users to create custom tables
#' with selected indicators. Users can filter data by year, value type, and other criteria,
#' and dynamically manage their selections.
#'
#' Functionality:
#' - Enables users to add and remove selected indicators from a custom table.
#' - Filters data based on user inputs (year, value type, filter conditions).
#' - Dynamically updates a reactive table displaying the selected data.
#' - Future expansion: Add download handlers for exporting data in multiple formats.
#'
#' Dependencies:
#' - `shiny`, `dplyr`, `DT`
#'
#' Output:
#' - A dynamically updated `datatable` displaying selected indicators.
#' - UI elements for managing selected indicators.


#' Add a selection to the data service table
#'
#' This function allows users to add indicators to their custom table based on selected filters.
#' It ensures that duplicate indicators are not added and updates the table dynamically.
#'
#' @param session The Shiny session object.
#' @param input The Shiny input object.
#' @param output The Shiny output object.
#' @param selected_data_serv A reactive expression providing the available dataset.
#' @param download_data A reactive value storing the selected data for download.
add_selection <- function(session, input, output, selected_data_serv, download_data) {

  # Reactive value to store selected columns
  selected_columns <- reactiveVal(character())

  # Base columns that should never be removed
  base_columns <- c("bfs_nr_gemeinde")

  observeEvent(input$add_selection, {
    req(input$self_service_filter1, input$self_service_year, input$self_service_value_type, selected_data_serv(), download_data())

    data_to_add <- selected_data_serv() %>%
      filter(jahr == input$self_service_year)

    if (input$self_service_filter1 != "Kein Filter") {
      data_to_add <- data_to_add %>%
        filter(filter1 == input$self_service_filter1)
      ft <- clean_string(input$self_service_filter1)
    } else {
      ft <- NULL
    }

    if (input$self_service_filter1 == "Prozentual") {
      data_to_add <- data_to_add %>%
        filter(value == share)
      vt <- "proz"
    } else {
      vt <- "abs"
    }

    name_col <- paste(clean_string(input$self_service_indicator), ft, clean_string(input$self_service_year), vt, sep = "_")

    # Only add if not already present
    if (!(name_col %in% names(download_data()))) {
      data_to_add <- data_to_add %>%
        select(bfs_nr_gemeinde, value) %>%
        setNames(c("bfs_nr_gemeinde", name_col))

      download_data(download_data() %>%
                      left_join(data_to_add, by = "bfs_nr_gemeinde"))

      # Store added column in reactive list
      selected_columns(unique(c(selected_columns(), name_col)))
    }
  })

  # Render dynamic selection "buttons" with remove option
  output$selected_filters <- renderUI({
    req(length(selected_columns()) > 0)

    tagList(
      lapply(selected_columns(), function(col) {
        div(
          style = "display: inline-block; margin: 5px; padding: 5px; background-color: lightgray; border-radius: 5px; cursor: pointer;",
          span(col, style = "margin-right: 5px;"),
          actionButton(inputId = paste0("remove_", col), label = "❌", class = "btn btn-danger btn-sm")
        )
      })
    )
  })

  # Observe clicks on remove buttons
  observe({
    req(selected_columns())

    lapply(selected_columns(), function(col) {
      observeEvent(input[[paste0("remove_", col)]], {
        isolate({
          # Check if column exists and is not a base column
          if (col %in% names(download_data()) && !(col %in% base_columns)) {
            download_data(download_data() %>% select(-all_of(col)))
          }

          # Update selected columns list
          selected_columns(setdiff(selected_columns(), col))

          # Re-render the DataTable
          output$download_table <- renderDT({
            datatable(
              download_data(),
              options = list(
                scrollX = TRUE,
                scrollY = "400px",
                paging = FALSE,
                dom = 'Bfrtip'
              ),
              rownames = FALSE
            )
          })
        })
      }, ignoreInit = TRUE, once = TRUE)
    })
  })

  # Render the initial DataTable
  output$download_table <- renderDT({
    datatable(
      download_data(),
      options = list(
        scrollX = TRUE,
        scrollY = "400px",
        paging = FALSE,
        dom = 'Bfrtip'
      ),
      rownames = FALSE
    )
  })
}
