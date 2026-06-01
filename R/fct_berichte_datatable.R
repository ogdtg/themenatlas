#' 05_tab2_create_datatable.R
#'
#' This script creates data tables (`DT` objects) for the report tab based on the structured
#' content of `structure_list`. It formats data dynamically, applies filters, and supports
#' multiple table types such as time series, indicator tables, and population pyramids.
#'
#' Functionality:
#' - Generates tables with or without share percentages.
#' - Supports time-series tables and indicator tables.
#' - Creates population pyramid tables with structured headers.
#' - Provides interactive download functionality for tables.
#'
#' Dependencies:
#' - `dplyr`, `tidyr`, `DT`, `htmltools`, `openxlsx`
#'
#' Output:
#' - Rendered `DT` tables with custom headers and formatting.
#' - Downloadable Excel tables.





#' Create a report table with share percentages
#'
#' This function generates a data table with both absolute values and share percentages.
#'
#' @param data Data frame containing the relevant data.
#' @param base_area The base area for comparison.
#' @param compare_area The area to compare against.
#' @param year The selected year.
#' @param bezirk_data_names A named vector mapping area codes to their names.
#' @param ts Logical, whether the table is a time series.
#' @param filter_value Optional, filter for specific categories.
#' @return A `DT` object representing the formatted table.
create_report_table_with_share <- function(data,base_area,compare_area,year,bezirk_data_names,ts,filter_value = NULL){


  relevant_names <- bezirk_data_names[bezirk_data_names %in% c(base_area,compare_area)]



  if (is.null(filter_value)){
    df <- data %>%
      filter(bfs_nr_gemeinde %in% relevant_names) %>%
      filter(jahr == year) %>%
      pivot_wider(names_from = bfs_nr_gemeinde, values_from = c(share,value)) %>%
      select(jahr,filter1,contains(base_area),contains(compare_area)) %>%
      mutate_at(vars(contains("share")),round,1)

    sketch <- htmltools::tags$table(
      tags$thead(
        tags$tr(
          tags$th(rowspan = 2, "Jahr", align = "left"),  # ✅ Main header
          tags$th(rowspan = 2, "Ausprägung", align = "left"),  # ✅ Main header

          tags$th(colspan = 2, names(bezirk_data_names[bezirk_data_names==base_area]), align = "left"),   # ✅ Group 1 header
          tags$th(colspan = 2, names(bezirk_data_names[bezirk_data_names==compare_area]), align = "left")    # ✅ Group 2 header
        ),
        tags$tr(
          tags$th("Anteil", align = "left"), tags$th("Absolut", align = "left"),  # ✅ Sub-headers under Group 1
          tags$th("Anteil", align = "left"), tags$th("Absolut", align = "left")   # ✅ Sub-headers under Group 2
        )
      )
    )

  } else {
    df <- data %>%
      filter(bfs_nr_gemeinde %in% relevant_names) %>%
      filter(jahr == year) %>%
      pivot_wider(names_from = bfs_nr_gemeinde, values_from = c(share,value)) %>%
      select(jahr,filter1,contains(base_area),contains(compare_area)) %>%
      mutate_at(vars(contains("share")),round,1)

    sketch <- htmltools::tags$table(
      tags$thead(
        tags$tr(
          tags$th(rowspan = 2, "Jahr", align = "left"),  # ✅ Main header
          tags$th(rowspan = 2, filter_value, align = "left"),  # ✅ Main header

          tags$th(colspan = 2, names(bezirk_data_names[bezirk_data_names==base_area]), align = "left"),   # ✅ Group 1 header
          tags$th(colspan = 2, names(bezirk_data_names[bezirk_data_names==compare_area]), align = "left")    # ✅ Group 2 header
        ),
        tags$tr(
          tags$th("Anteil", align = "left"), tags$th("Absolut", align = "left"),  # ✅ Sub-headers under Group 1
          tags$th("Anteil", align = "left"), tags$th("Absolut", align = "left")   # ✅ Sub-headers under Group 2
        )
      )
    )

  }







  # Define custom header


  # Render DataTable with custom header
  datatable(df,
            container = sketch,  # ✅ Apply custom header
            rownames = F,
            options = list(
              autoWidth = FALSE, scrollX = TRUE,
              columnDefs = list(
                # list(width = "125px", targets = "_all"),
                list(className = "dt-left", targets = "_all")),
              dom = 't',
              pageLength = nrow(df)
            )
  )
}



#' Create a time-series report table without share percentages
#'
#' This function generates a data table for time-series data without share percentages.
#'
#' @param data Data frame containing the relevant data.
#' @param base_area The base area for comparison.
#' @param compare_area The area to compare against.
#' @param year The selected year.
#' @param bezirk_data_names A named vector mapping area codes to their names.
#' @param filter_value Optional, filter for specific categories.
#' @return A `DT` object representing the formatted time-series table.
create_report_table_ts_without_share <- function(data,base_area,compare_area,year,bezirk_data_names,filter_value = NULL){

  relevant_names <- bezirk_data_names[bezirk_data_names %in% c(base_area,compare_area)]

  if (is.null(filter_value)){
    df <- data %>%
      filter(bfs_nr_gemeinde %in% relevant_names) %>%
      pivot_wider(names_from = bfs_nr_gemeinde, values_from = c(value)) %>%
      mutate_at(vars(contains("share")),round,1)

  } else {
    df <- data %>%
      filter(filter1 == filter_value) %>%
      filter(bfs_nr_gemeinde %in% relevant_names) %>%
      pivot_wider(names_from = bfs_nr_gemeinde, values_from = c(value)) %>%
      mutate_at(vars(contains("share")),round,1) %>%
      select(-filter1)
  }

  # Render DataTable with custom header
  datatable(df,
            rownames = F,
            colnames = c("Jahr",names(bezirk_data_names[bezirk_data_names==base_area]),names(bezirk_data_names[bezirk_data_names==compare_area])),
            options = list(
              autoWidth = FALSE, scrollX = TRUE,
              columnDefs = list(
                # list(width = "125px", targets = "_all"),
                list(className = "dt-left", targets = "_all")),
              dom = 't',
              pageLength = nrow(df)
            )
  )


}


#' Create a time-series report table with share percentages
#'
#' This function generates a time-series data table with both absolute values and share percentages.
#'
#' @param data Data frame containing the relevant data.
#' @param base_area The base area for comparison.
#' @param compare_area The area to compare against.
#' @param year The selected year.
#' @param bezirk_data_names A named vector mapping area codes to their names.
#' @param filter_value Optional, filter for specific categories.
#' @return A `DT` object representing the formatted time-series table.
create_report_table_ts_with_share <- function(data,base_area,compare_area,year,bezirk_data_names,filter_value = NULL){

  relevant_names <- bezirk_data_names[bezirk_data_names %in% c(base_area,compare_area)]


  if (is.null(filter_value)){
    df <- data %>%
      filter(bfs_nr_gemeinde %in% relevant_names) %>%
      pivot_wider(names_from = bfs_nr_gemeinde, values_from = c(share,value)) %>%
      mutate_at(vars(contains(relevant_names)),round,1) %>%
      select(jahr,contains(base_area),contains(compare_area))

  } else {
    df <-data %>%
      filter(filter1 == filter_value) %>%
      filter(bfs_nr_gemeinde %in% relevant_names) %>%
      pivot_wider(names_from = bfs_nr_gemeinde, values_from =  c(share,value)) %>%
      mutate_at(vars(contains("share")),round,1) %>%
      select(-filter1) %>%
      select(jahr,contains(base_area),contains(compare_area))
  }




  # Define custom header
  sketch <- htmltools::tags$table(
    tags$thead(
      tags$tr(
        tags$th(rowspan = 2, "Jahr", align = "left"),  # ✅ Main header

        tags$th(colspan = 2, names(bezirk_data_names[bezirk_data_names==base_area]), align = "left"),   # ✅ Group 1 header
        tags$th(colspan = 2, names(bezirk_data_names[bezirk_data_names==compare_area]), align = "left")    # ✅ Group 2 header
      ),
      tags$tr(
        tags$th("Anteil", align = "left"), tags$th("Absolut", align = "left"),  # ✅ Sub-headers under Group 1
        tags$th("Anteil", align = "left"), tags$th("Absolut", align = "left")   # ✅ Sub-headers under Group 2
      )
    )
  )

  # Render DataTable with custom header
  datatable(df,
            container = sketch,  # ✅ Apply custom header
            rownames = F,
            options = list(
              autoWidth = FALSE, scrollX = TRUE,
              columnDefs = list(
                # list(width = "125px", targets = "_all"),
                list(className = "dt-left", targets = "_all")),
              dom = 't',
              pageLength = nrow(df)
            )
  )


}


#' Create a report table without share percentages
#'
#' This function generates a standard data table without share percentages.
#'
#' @param data Data frame containing the relevant data.
#' @param base_area The base area for comparison.
#' @param compare_area The area to compare against.
#' @param year The selected year.
#' @param bezirk_data_names A named vector mapping area codes to their names.
#' @param ts Logical, whether the table is a time series.
#' @param filter_value Optional, filter for specific categories.
#' @return A `DT` object representing the formatted table.
create_report_table_without_share <- function(data,base_area,compare_area,year,bezirk_data_names,ts,filter_value = NULL){


  relevant_names <- bezirk_data_names[bezirk_data_names %in% c(base_area,compare_area)]


  df <- data %>%
    filter(bfs_nr_gemeinde %in%c(base_area,compare_area)) %>%
    filter(jahr == year) %>%
    pivot_wider(names_from = bfs_nr_gemeinde, values_from = c(value)) %>%
    select(jahr,contains(base_area),contains(compare_area)) %>%
    mutate_at(vars(all_of(relevant_names)),round,1)




  # Render DataTable with custom header
  datatable(df,
            rownames = F,
            colnames = c("Jahr",names(bezirk_data_names[bezirk_data_names==base_area]),names(bezirk_data_names[bezirk_data_names==compare_area])),
            options = list(
              autoWidth = FALSE, scrollX = TRUE,
              columnDefs = list(
                # list(width = "125px", targets = "_all"),
                list(className = "dt-left", targets = "_all")),
              dom = 't',
              pageLength = nrow(df)
            )
  )
}


#' Create a population pyramid report table
#'
#' This function generates a data table for population pyramids, showing age distribution by gender.
#'
#' @param data Data frame containing the relevant data.
#' @param base_area The base area for comparison.
#' @param compare_area The area to compare against.
#' @param year The selected year.
#' @param bezirk_data_names A named vector mapping area codes to their names.
#' @return A `DT` object representing the formatted population pyramid table.
create_report_table_pyramid <- function(data,base_area,compare_area,year,bezirk_data_names){

  df <- data %>%
    filter(jahr == year) %>%
    filter(bfs_nr_gemeinde %in% c(base_area,compare_area)) %>%
    pivot_wider(names_from = c(sex,bfs_nr_gemeinde), values_from = value)%>%
    arrange(ageclass_code) %>%
    select(jahr,ageclass,contains(base_area),contains(compare_area))



  # Define custom header
  sketch <- htmltools::tags$table(
    tags$thead(
      tags$tr(
        tags$th(rowspan = 2, "Jahr", align = "left"),  # ✅ Main header
        tags$th(rowspan = 2, "Altersklasse", align = "left"),  # ✅ Main header

        tags$th(colspan = 2, names(bezirk_data_names[bezirk_data_names==base_area]), align = "left"),   # ✅ Group 1 header
        tags$th(colspan = 2, names(bezirk_data_names[bezirk_data_names==compare_area]), align = "left")    # ✅ Group 2 header
      ),
      tags$tr(
        tags$th("Männer", align = "left"), tags$th("Frauen", align = "left"),  # ✅ Sub-headers under Group 1
        tags$th("Männer", align = "left"), tags$th("Frauen", align = "left")   # ✅ Sub-headers under Group 2
      )
    )
  )

  # Render DataTable with custom header
  datatable(df,
            container = sketch,  # ✅ Apply custom header
            rownames = F,
            options = list(
              autoWidth = FALSE, scrollX = TRUE,
              columnDefs = list(
                # list(width = "125px", targets = "_all"),
                list(className = "dt-left", targets = "_all")),
              dom = 't',
              pageLength = nrow(df)
            )
  )
}


#' Create a report table dynamically based on data structure
#'
#' Determines the appropriate table type based on the dataset and calls the respective function.
#'
#' @param data Data frame containing the relevant data.
#' @param base_area The base area for comparison.
#' @param compare_area The area to compare against.
#' @param year The selected year.
#' @param bezirk_data_names A named vector mapping area codes to their names.
#' @param ts Logical, whether the table is a time series.
#' @param filter_value Optional, filter for specific categories.
#' @return A `DT` object representing the formatted table.
create_report_table <- function(data,base_area,compare_area,year,bezirk_data_names,ts=FALSE,filter_value = NULL){

  data <- data %>%
    mutate_if(is.numeric,round,1)

  if (ts){
    if ("share" %in% colnames(data)){
      return(create_report_table_ts_with_share(data,base_area,compare_area,year=NULL,bezirk_data_names,filter_value = filter_value))
    } else {
      return(create_report_table_ts_without_share(data,base_area,compare_area,year=NULL,bezirk_data_names,filter_value = filter_value))
    }
  } else {
    if ("share" %in% colnames(data)){
      return(create_report_table_with_share(data=data,base_area=base_area,compare_area=compare_area,year=year,bezirk_data_names=bezirk_data_names,ts=ts,filter_value = filter_value))

    } else {
      return(create_report_table_without_share(data,base_area,compare_area,year,bezirk_data_names))
    }
  }


}


#' Create a download button for report tables
#'
#' Generates a downloadable Excel file from a given data table.
#'
#' @param data The data frame to be exported.
#' @param selected_base_area The selected base area.
#' @param selected_compare_area The area for comparison.
#' @param title The title of the report.
#' @param bezirk_data_names A named vector mapping area codes to their names.
#' @return A `downloadHandler` object for the table.
create_download_button <- function(data, selected_base_area, selected_compare_area, title, bezirk_data_names) {

  ca_name <- names(bezirk_data_names)[bezirk_data_names == selected_compare_area]
  ba_name <- names(bezirk_data_names)[bezirk_data_names == selected_base_area]

  header_list <- list()
  merge_list <- list()  # ✅ Ensure merge_list is initialized

  if (ncol(data) == 3) {
    header_list$header1 <- c("Jahr", ba_name, ca_name)

  } else if (ncol(data) == 5) {
    header_list$header1 <- c("Jahr", ba_name, ba_name, ca_name, ca_name)
    header_list$header2 <- c("", "Anteil", "Absolut", "Anteil", "Absolut")

    merge_list <- list(
      list(rows = 3, cols = 2:3),
      list(rows = 3, cols = 4:5),
      list(rows = 3:4, cols = 1)
    )

  } else if (ncol(data) == 6) {
    merge_list <- list(
      list(rows = 3, cols = 3:4),
      list(rows = 3, cols = 5:6),
      list(rows = 3:4, cols = 1),
      list(rows = 3:4, cols = 2)
    )

    if (!any(grepl("Weiblich", names(data)))) {  # ✅ Fixed str_detect issue
      header_list$header1 <- c("Jahr", "Altersklasse", ba_name, ba_name, ca_name, ca_name)
      header_list$header2 <- c("", "", "Anteil", "Absolut", "Anteil", "Absolut")
    } else {
      header_list$header1 <- c("Jahr", "Ausprägung", ba_name, ba_name, ca_name, ca_name)
      header_list$header2 <- c("", "", "Männer", "Frauen", "Männer", "Frauen")
    }

  } else if (ncol(data) == 4) {
    header_list$header1 <- c("Jahr", "Ausprägung", ba_name, ca_name)  # ✅ Fixed incorrect list assignment
  }

  return(downloadHandler(
    filename = function() {
      paste0(clean_string(title), ".xlsx")  # ✅ Ensured valid filename
    },
    content = function(file) {
      wb <- createWorkbook()
      addWorksheet(wb, "Data")

      writeData(wb, "Data", title, startRow = 1, colNames = FALSE)

      if (length(header_list) == 2) {
        writeData(wb, "Data", rbind(header_list$header1, header_list$header2), startRow = 3, colNames = FALSE)
        writeData(wb, "Data", data, startRow = 5, colNames = FALSE)

        if (length(merge_list) > 0) {  # ✅ Ensure merge_list exists before using it
          lapply(merge_list, function(x) {
            mergeCells(wb, "Data", cols = x$cols, rows = x$rows)
          })
        }

      } else if (length(header_list) == 1) {
        writeData(wb, "Data", header_list$header1, startRow = 3, colNames = FALSE)
        writeData(wb, "Data", data, startRow = 4, colNames = FALSE)
      }

      # Save file
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  ))
}



#' Render a report table in a Shiny session
#'
#' This function renders a report table within a Shiny session.
#'
#' @param session The Shiny session object.
#' @param id The output ID for the table.
#' @param input The Shiny input object.
#' @param output The Shiny output object.
#' @param data The data frame for the report.
#' @param bezirk_data_names A named vector mapping area codes to their names.
#' @param selected_base_area The selected base area.
#' @param selected_compare_area The area for comparison.
#' @param title The title of the report.
#' @param ts Logical, whether the table is a time series.
#' @param filter_value Optional, filter for specific categories.
render_report_table <- function(session, id, input, output, data, bezirk_data_names, selected_base_area, selected_compare_area,title, ts = FALSE, filter_value = NULL) {
  req(selected_base_area(), selected_compare_area())

  ba <- selected_base_area()
  ca <- selected_compare_area()


  if (!ts){
    select_id <- paste0("select_year_",clean_string(input$report_topic),"_",str_extract(id,"\\d+"))

    year <- input[[select_id]]

    ##print(paste0("render_single_chart: ",year))

  } else {
    year=NULL
  }


  dt_object <- create_report_table(
    data = data,
    base_area = ba,
    compare_area = ca,
    year = year,
    bezirk_data_names = bezirk_data_names,
    ts = ts,
    filter_value = filter_value
  )

  output[[id]] <- renderDT({
    dt_object
  })

  output[[paste0(id,"_download")]] <- create_download_button(data=dt_object$x$data,selected_base_area=ba, selected_compare_area=ca,title=title,bezirk_data_names)
}

#' Render a population pyramid report table in a Shiny session
#'
#' This function renders a population pyramid table within a Shiny session.
#'
#' @param session The Shiny session object.
#' @param id The output ID for the table.
#' @param input The Shiny input object.
#' @param output The Shiny output object.
#' @param data The data frame for the report.
#' @param bezirk_data_names A named vector mapping area codes to their names.
#' @param selected_base_area The selected base area.
#' @param selected_compare_area The area for comparison.
#' @param title The title of the report.
render_report_table_pramid <- function(session, id, input, output, data, bezirk_data_names, selected_base_area, selected_compare_area,title){

  select_id <- paste0("select_year_",clean_string(input$report_topic),"_",str_extract(id,"\\d+"))

  req(selected_base_area(), selected_compare_area())

  ba <- selected_base_area()
  ca <- selected_compare_area()

  year <- input[[select_id]]
  #print(paste0("render_report_table_pramid: ",year))

  dt_object <- create_report_table_pyramid(
    data = data,
    base_area = ba,
    compare_area = ca,
    year = year,
    bezirk_data_names = bezirk_data_names
  )


  output[[id]] <- renderDT({
    dt_object
  })

  output[[paste0(id,"_download")]] <- create_download_button(data=dt_object$x$data,selected_base_area=ba, selected_compare_area=ca,title=title,bezirk_data_names)

}





#' Prepare an indicator table for display
#'
#' Extracts relevant indicators from a given list and formats them into a structured table.
#'
#' @param indicator_list List of indicator metadata.
#' @param base_area The base area for comparison.
#' @param compare_area The area for comparison.
#' @param year The selected year.
#' @return A data frame containing the prepared indicator table.
prepare_indicator_table <- function(indicator_list,base_area,compare_area,year){




  final_list <- list()


  # year <- input[[paste0("select_year_",str_extract(id,"\\d+"))]]
  # #print(paste0("prepare_indicator_table: ",year))

  final_list <- lapply(indicator_list, function(x){

    data <- eval(parse(text = x$data_path)) %>%
      filter(jahr == year) %>%
      filter(bfs_nr_gemeinde %in% c(base_area, compare_area)) %>%
      mutate(jahr = as.character(jahr))


    if (!is.null(x$value_types)) {
      if (x$value_types == "share") {
        data <- data %>%
          mutate(value = share) %>%
          select(-share)
      } else {
        data <- data %>%
          select(-share)
      }
    } else if (is.null(x$value_types) & "share" %in% names(data)){
      data <- data %>%
        select(-share)
    }

    if (isTRUE(x$sum_up)){
      data <- data %>%
        group_by(bfs_nr_gemeinde,jahr) %>%
        summarise(value = sum(as.numeric(value)),.groups = "drop") %>%
        ungroup()
    }

    if ("filter1" %in% names(data)) {

      if (!is.null(x$value_types)) {
        if (x$value_types == "share") {
          data <- data %>%
            mutate(filter1 = paste0(filter1," (in %)"))
        }
      }


      if (!is.null(x$value_types)) {
        data <- data %>%
          filter(filter1 %in% paste0(x$filter_values," (in %)"))
      } else {
        data <- data %>%
          filter(filter1 %in% x$filter_values)
      }

    } else {
      data$filter1 <- x$indicator_names
    }
    #
    if (!is.null(x$indicator_names)){
      data$filter1 <- x$indicator_names
    }

    data %>%
      pivot_wider(names_from = bfs_nr_gemeinde, values_from = value)

  })



  indicator_df <- final_list %>%
    bind_rows()

  indicator_df %>%
    select(jahr,filter1,contains(as.character(base_area)),contains(as.character(compare_area)))

}

#' Create an indicator table
#'
#' Generates a formatted indicator table from a given list of indicators.
#'
#' @param indicator_list List of indicator metadata.
#' @param base_area The base area for comparison.
#' @param compare_area The area for comparison.
#' @param year The selected year.
#' @param bezirk_data_names A named vector mapping area codes to their names.
#' @return A `DT` object representing the formatted indicator table.
create_indicator_table <- function(indicator_list,base_area,compare_area,year,bezirk_data_names){


  df <- prepare_indicator_table(indicator_list,base_area,compare_area,year) %>%
    mutate_if(is.numeric,round,2)

  names(df)[1:2] <- c("Jahr","Indikator")

  names(df)[3] <- names(bezirk_data_names)[bezirk_data_names == base_area]
  names(df)[4] <- names(bezirk_data_names)[bezirk_data_names == compare_area]


  datatable(df,
            rownames = F,
            options = list(
              autoWidth = FALSE, scrollX = TRUE,
              columnDefs = list(
                # list(width = "125px", targets = "_all"),
                list(className = "dt-left", targets = "_all")),
              dom = 't',
              pageLength = nrow(df)
            )
  )

}

#' Render an indicator table in a Shiny session
#'
#' This function renders an indicator table within a Shiny session.
#'
#' @param session The Shiny session object.
#' @param id The output ID for the table.
#' @param input The Shiny input object.
#' @param output The Shiny output object.
#' @param indicator_list List of indicator metadata.
#' @param bezirk_data_names A named vector mapping area codes to their names.
#' @param selected_base_area The selected base area.
#' @param selected_compare_area The area for comparison.
#' @param title The title of the report.
render_indicator_table <- function(session, id, input, output, indicator_list, bezirk_data_names, selected_base_area, selected_compare_area,title){


  select_id <- paste0("select_year_",clean_string(input$report_topic),"_",str_extract(id,"\\d+"))
  # print(select_id)

  # print(paste0("render_indicator_table: ",selected_base_area()," ",selected_compare_area()," ",input[[select_id]]))

  # print(input$select_year_bevoelkerung_1)
  req(selected_base_area(), selected_compare_area(),input[[select_id]])


  year <- input[[select_id]]
  #print(paste0("render_indicator_table: ",year))


  dt_object <- create_indicator_table(
    indicator_list = indicator_list,
    base_area = selected_base_area(),
    compare_area = selected_compare_area(),
    year = year,
    bezirk_data_names = bezirk_data_names
  )



  output[[id]] <- renderDT({
    dt_object
  })

  output[[paste0(id, "_download")]] <- create_download_button(
    data = dt_object$x$data,
    selected_base_area = selected_base_area(),
    selected_compare_area = selected_compare_area(),
    title = title,
    bezirk_data_names
  )

}



