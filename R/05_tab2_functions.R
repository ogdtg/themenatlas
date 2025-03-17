#' 05_tab2_functions.R
#'
#' This script integrates the UI rendering and content rendering functions from the other `05_tab2` scripts.
#' It dynamically updates charts, tables, and data sources based on user interactions in the Shiny app.
#'
#' Functionality:
#' - Reactively updates UI elements when a new report topic is selected.
#' - Renders charts and tables dynamically based on the structure list.
#' - Handles changes in selected base and comparison areas.
#' - Supports multiple chart types, including time series, donut charts, pyramids, and indicator tables.
#'
#' Dependencies:
#' - `shiny`, `dplyr`, `stringr`, `highcharter`, `DT`
#'
#' Output:
#' - Dynamically updated Highcharts visualizations and tables in the Shiny UI.
#' - UI elements for selecting report topics, years, and areas of comparison.



#' Render the UI for a selected report topic
#'
#' This function updates the structure list based on the selected report topic and dynamically renders the UI components.
#'
#' @param session The Shiny session object.
#' @param output The Shiny output object.
#' @param input The Shiny input object.
#' @param structure_list_reactive A reactive value storing the structure list.
#' @param renderedTopics A reactive value tracking already rendered topics.
render_topic_ui <- function(session, output, input,structure_list_reactive,renderedTopics){
  # Reactively store the structure list
  # structure_list_reactive <- reactiveVal(NULL)

  # 1️⃣ Observe report_topic changes -> Set structure_list & call render_generic_report2()
  observeEvent(input$report_topic, {
    req(input$report_topic)

    # if (!input$report_topic %in% renderedTopics()){
      filtered_list <- Filter(function(x) x$topic == input$report_topic, structure_list)
      structure_list_reactive(filtered_list)

      # Ensure the list is not empty
      req(length(structure_list_reactive()) > 0)

      # Render the full report structure
      render_generic_report2(output, filtered_list)


      lapply(seq_along(filtered_list), function(i) {

        id <- paste0("report_chart_select_", clean_string(input$report_topic),"_",i)

        if (filtered_list[[i]]$chart_type == "indicator_table"){



          if (filtered_list[[i]]$topic %in% c("Raum","Staat und Politik")) {
            data <- eval(parse(text = filtered_list[[i]]$indicator_list[[1]]$data_path))
            choices <- unique(data$jahr) %>% as.numeric() %>% sort()
          } else {
            min_jahr <- c()
            max_jahr <- c()

            for (x in filtered_list[[i]]$indicator_list) {

              data <- eval(parse(text = x$data_path)) %>% mutate(jahr = as.numeric(jahr))

              min_jahr <- c(min_jahr, min(data$jahr, na.rm = T))
              max_jahr <- c(max_jahr, max(data$jahr, na.rm = T))

            }
            choices <- max(min_jahr):min(max_jahr)
          }




        } else {
          data <- eval(parse(text = filtered_list[[i]]$data_path))  # Evaluate dataset path dynamically


          choices <- unique(data$jahr) %>% as.numeric() %>% sort()
        }

        select_id <- paste0("select_year_",clean_string(filtered_list[[i]]$topic),"_",i)


        output[[id]] <- renderUI({
          selectizeInput(select_id, "", choices = choices,selected = max(choices))
        })

      })

      structure_list_reactive(filtered_list)
      # renderedTopics(c(renderedTopics(),input$report_topic))
      # print(renderedTopics())
    # }


  })

}



#' Handle changes in the selected report topic
#'
#' This function updates charts and tables when the user selects a new report topic.
#'
#' @param session The Shiny session object.
#' @param output The Shiny output object.
#' @param input The Shiny input object.
#' @param selected_base_area Reactive value representing the selected base area.
#' @param selected_compare_area Reactive value representing the selected comparison area.
#' @param bezirk_data_names A named vector mapping area codes to names.
#' @param structure_list_reactive A reactive value storing the structure list.
change_at_report_topic <- function(session, output, input, selected_base_area, selected_compare_area, bezirk_data_names, structure_list_reactive){

  observeEvent(list(input$report_topic), {
    req(structure_list_reactive())  # Ensure structure_list_reactive() is available


    # Nur zeitreihen rendern
    lapply(seq_along(structure_list_reactive()), function(i) {


      if (str_detect(structure_list_reactive()[[i]]$chart_type,"zeitreihe")) {



        share <- FALSE
        if (!is.null(structure_list_reactive()[[i]]$share)){
          share <- structure_list_reactive()[[i]]$share
        }
        chart_type_raw <- str_extract(structure_list_reactive()[[i]]$chart_type, "column|line")

        data <- eval(parse(text = structure_list_reactive()[[i]]$data_path))  # Get dataset



        render_single_chart_and_table(
          session,
          output,
          input,
          selected_base_area,
          selected_compare_area,
          topic = structure_list_reactive()[[i]]$topic,
          data = data,
          id_num = i,
          ts = TRUE,
          filter_value = structure_list_reactive()[[i]]$filter_value,
          bezirk_data_names = bezirk_data_names,
          value_title = structure_list_reactive()[[i]]$value_title,
          chart_type = chart_type_raw,
          title = structure_list_reactive()[[i]]$title,
          share = share
        )

        #Data sources
        render_data_sources(output,id=paste0("data_sources_",clean_string(structure_list_reactive()[[i]]$topic),"_", i),data_path=structure_list_reactive()[[i]]$data_path)
      } else {
        select_id <- paste0("select_year_",clean_string(structure_list_reactive()[[1]]$topic),"_",i)

        observeEvent(input[[select_id]], {
          req(input[[select_id]])  # Ensure year is selected



          if (structure_list_reactive()[[i]]$chart_type=="indicator_table"){

            render_indicator_table(session,
                                   id=paste0("report_table_",clean_string(structure_list_reactive()[[i]]$topic),"_", i),
                                   input,
                                   output,
                                   indicator_list = structure_list_reactive()[[i]]$indicator_list,
                                   bezirk_data_names = bezirk_data_names,
                                   selected_base_area,
                                   selected_compare_area,
                                   title = structure_list_reactive()[[i]]$title)



            render_data_sources_indicator_table(output,
                                                id=paste0("data_sources_",clean_string(structure_list_reactive()[[i]]$topic),"_", i),
                                                indicator_list=structure_list_reactive()[[i]]$indicator_list)

          } else {
            data_path <- structure_list_reactive()[[i]]$data_path
            req(data_path)  # Ensure data_path is valid

            data <- eval(parse(text = data_path))  # Get dataset
            data_filtered <- data[data$jahr == input[[select_id]], ]  # Filter by selected year

            chart_type <- structure_list_reactive()[[i]]$chart_type
            share <- ifelse(
              !is.null(structure_list_reactive()[[i]]$share),
              structure_list_reactive()[[i]]$share,
              FALSE
            )

            # print(structure_list_reactive()[[i]]$chart_type)
            # print(select_id)

            if (str_detect(chart_type, "donut")) {
              color_mapping <- generate_color_mapping(sort(unique(data_filtered$filter1)))
              # print(paste0("report_table_",clean_string(structure_list_reactive()[[i]]$topic),"_", i))

              render_donut_chart(
                session,
                id = paste0("report_chart_ba_",clean_string(structure_list_reactive()[[i]]$topic),"_", i),
                input,
                output,
                reactive_val = selected_base_area,
                data = data_filtered,
                bezirk_data_names = bezirk_data_names,
                color_mapping = color_mapping
              )

              render_donut_chart(
                session,
                id = paste0("report_chart_ca_",clean_string(structure_list_reactive()[[i]]$topic),"_", i),
                input,
                output,
                reactive_val = selected_compare_area,
                data = data_filtered,
                bezirk_data_names = bezirk_data_names,
                color_mapping = color_mapping
              )

              render_report_table(
                session,
                id = paste0("report_table_",clean_string(structure_list_reactive()[[i]]$topic),"_", i),
                input,
                output,
                data,
                bezirk_data_names,
                selected_base_area,
                selected_compare_area,
                title = structure_list_reactive()[[i]]$title
              )

              render_data_sources(output,id=paste0("data_sources_",clean_string(structure_list_reactive()[[i]]$topic),"_", i),data_path=structure_list_reactive()[[i]]$data_path)

            }

            if (str_detect(chart_type, "pyramid")) {
              render_pyramid(
                session,
                id = paste0("report_chart_ba_",clean_string(structure_list_reactive()[[i]]$topic),"_", i),
                input,
                output,
                reactive_val = selected_base_area,
                data = data_filtered,
                bezirk_data_names = bezirk_data_names
              )

              render_pyramid(
                session,
                id = paste0("report_chart_ca_",clean_string(structure_list_reactive()[[i]]$topic),"_", i),
                input,
                output,
                reactive_val = selected_compare_area,
                data = data_filtered,
                bezirk_data_names = bezirk_data_names
              )

              render_report_table_pramid(session,
                                         id = paste0("report_table_",clean_string(structure_list_reactive()[[i]]$topic),"_", i),
                                         input,
                                         output,
                                         data,
                                         bezirk_data_names,
                                         selected_base_area,
                                         selected_compare_area,
                                         title=structure_list_reactive()[[i]]$title)
              #Data sources
              render_data_sources(output,id=paste0("data_sources_",clean_string(structure_list_reactive()[[i]]$topic),"_", i),data_path=structure_list_reactive()[[i]]$data_path)
            }

            if (str_detect(chart_type, "column") || str_detect(chart_type, "line")) {
              chart_type_raw <- str_extract(chart_type, "column|line")

              render_single_chart_and_table(
                session, output, input,
                selected_base_area, selected_compare_area,
                topic = structure_list_reactive()[[i]]$topic,
                data = data_filtered,
                id_num = i,
                ts = str_detect(chart_type, "zeitreihe"),
                filter_value = structure_list_reactive()[[i]]$filter_value,
                bezirk_data_names = bezirk_data_names,
                value_title = structure_list_reactive()[[i]]$value_title,
                chart_type = chart_type_raw,
                title = structure_list_reactive()[[i]]$title,
                share = share
              )

              render_data_sources(output,id=paste0("data_sources_",clean_string(structure_list_reactive()[[i]]$topic),"_", i),data_path=structure_list_reactive()[[i]]$data_path)

            }

          }


        }
        )

      }

      # render datasources



    })
  })

}



#' Handle changes in the selected comparison area
#'
#' This function updates charts and tables when the user selects a new comparison area.
#'
#' @param session The Shiny session object.
#' @param output The Shiny output object.
#' @param input The Shiny input object.
#' @param selected_base_area Reactive value representing the selected base area.
#' @param selected_compare_area Reactive value representing the selected comparison area.
#' @param bezirk_data_names A named vector mapping area codes to names.
#' @param structure_list_reactive A reactive value storing the structure list.
change_at_compare_area <- function(session, output, input, selected_base_area, selected_compare_area, bezirk_data_names, structure_list_reactive){

  observeEvent(list(selected_compare_area()), {
    filtered_list <- structure_list_reactive()
    req(length(filtered_list) > 0)

    lapply(seq_along(filtered_list), function(i) {
      #print(paste0("XXXXX",clean_string(filtered_list[[i]]$topic),"_",i))


      if (filtered_list[[i]]$chart_type=="indicator_table"){
        render_indicator_table(session,
                               id=paste0("report_table_",clean_string(filtered_list[[i]]$topic),"_", i),
                               input,
                               output,
                               indicator_list = filtered_list[[i]]$indicator_list,
                               bezirk_data_names = bezirk_data_names,
                               selected_base_area,
                               selected_compare_area,
                               title = filtered_list[[i]]$title)

      } else {
        chart_type <- filtered_list[[i]]$chart_type
        share <- FALSE
        if (!is.null(filtered_list[[i]]$share)){
          share <- filtered_list[[i]]$share
        }

        data <- eval(parse(text = filtered_list[[i]]$data_path))  # Evaluate dataset path dynamically


        if (str_detect(chart_type, "donut")) {


          color_mapping <- generate_color_mapping(sort(unique(data$filter1)))


          render_donut_chart(
            session,
            id = paste0("report_chart_ca_",clean_string(filtered_list[[i]]$topic),"_", i),
            input,
            output,
            reactive_val = selected_compare_area,
            data = data,
            bezirk_data_names = bezirk_data_names,
            color_mapping=color_mapping
          )

          render_report_table(
            session,
            id = paste0("report_table_",clean_string(filtered_list[[i]]$topic),"_", i),
            input,
            output,
            data,
            bezirk_data_names,
            selected_base_area,
            selected_compare_area,
            title = filtered_list[[i]]$title
          )

        }

        if (str_detect(chart_type, "pyramid")) {
          render_pyramid(
            session, id = paste0("report_chart_ca_",clean_string(filtered_list[[i]]$topic),"_", i), input, output,
            reactive_val = selected_compare_area,
            data = data,
            bezirk_data_names = bezirk_data_names
          )
          render_report_table_pramid(session,
                                     id = paste0("report_table_", clean_string(filtered_list[[i]]$topic),"_",i),
                                     input,
                                     output,
                                     data,
                                     bezirk_data_names,
                                     selected_base_area,
                                     selected_compare_area,
                                     title=filtered_list[[i]]$title)
        }

        if (str_detect(chart_type, "column") || str_detect(chart_type, "line")) {
          chart_type_raw <- str_extract(chart_type, "column|line")  # FIXED HERE

          render_single_chart_and_table(
            session,
            output,
            input,
            selected_base_area,
            selected_compare_area,
            topic = filtered_list[[i]]$topic,
            data = data,
            id_num = i,
            ts = str_detect(chart_type, "zeitreihe"),
            filter_value = filtered_list[[i]]$filter_value,
            bezirk_data_names = bezirk_data_names,
            value_title = filtered_list[[i]]$value_title,
            chart_type = chart_type_raw,
            # FIXED HERE
            title = filtered_list[[i]]$title,
            share = share
          )
        }

      }




    })
  })

}






#' Handle changes in the selected base area
#'
#' This function updates charts and tables when the user selects a new base area.
#'
#' @param session The Shiny session object.
#' @param output The Shiny output object.
#' @param input The Shiny input object.
#' @param selected_base_area Reactive value representing the selected base area.
#' @param selected_compare_area Reactive value representing the selected comparison area.
#' @param bezirk_data_names A named vector mapping area codes to names.
#' @param structure_list_reactive A reactive value storing the structure list.
change_at_base_area <- function(session, output, input, selected_base_area, selected_compare_area, bezirk_data_names, structure_list_reactive){


  observeEvent(list(selected_base_area()), {
    filtered_list <- structure_list_reactive()
    req(length(filtered_list) > 0)

    lapply(seq_along(filtered_list), function(i) {
      #print(paste0("XXXXX",clean_string(filtered_list[[i]]$topic),"_",i))


      if (filtered_list[[i]]$chart_type=="indicator_table"){
        render_indicator_table(session,
                               id=paste0("report_table_",clean_string(filtered_list[[i]]$topic),"_", i),
                               input,
                               output,
                               indicator_list = filtered_list[[i]]$indicator_list,
                               bezirk_data_names = bezirk_data_names,
                               selected_base_area,
                               selected_compare_area,
                               title = filtered_list[[i]]$title)

      } else {
        chart_type <- filtered_list[[i]]$chart_type
        share <- FALSE
        if (!is.null(filtered_list[[i]]$share)){
          share <- filtered_list[[i]]$share
        }

        data <- eval(parse(text = filtered_list[[i]]$data_path))  # Evaluate dataset path dynamically


        if (str_detect(chart_type, "donut")) {


          color_mapping <- generate_color_mapping(sort(unique(data$filter1)))


          render_donut_chart(
            session,
            id = paste0("report_chart_ba_",clean_string(filtered_list[[i]]$topic),"_", i),
            input,
            output,
            reactive_val = selected_base_area,
            data = data,
            bezirk_data_names = bezirk_data_names,
            color_mapping=color_mapping
          )

          render_report_table(
            session,
            id = paste0("report_table_",clean_string(filtered_list[[i]]$topic),"_", i),
            input,
            output,
            data,
            bezirk_data_names,
            selected_base_area,
            selected_compare_area,
            title = filtered_list[[i]]$title
          )

        }

        if (str_detect(chart_type, "pyramid")) {
          render_pyramid(
            session, id = paste0("report_chart_ba_",clean_string(filtered_list[[i]]$topic),"_", i), input, output,
            reactive_val = selected_base_area,
            data = data,
            bezirk_data_names = bezirk_data_names
          )
          render_report_table_pramid(session,
                                     id = paste0("report_table_", clean_string(filtered_list[[i]]$topic),"_",i),
                                     input,
                                     output,
                                     data,
                                     bezirk_data_names,
                                     selected_base_area,
                                     selected_compare_area,
                                     title=filtered_list[[i]]$title)
        }

        if (str_detect(chart_type, "column") || str_detect(chart_type, "line")) {
          chart_type_raw <- str_extract(chart_type, "column|line")  # FIXED HERE

          render_single_chart_and_table(
            session,
            output,
            input,
            selected_base_area,
            selected_compare_area,
            topic = filtered_list[[i]]$topic,
            data = data,
            id_num = i,
            ts = str_detect(chart_type, "zeitreihe"),
            filter_value = filtered_list[[i]]$filter_value,
            bezirk_data_names = bezirk_data_names,
            value_title = filtered_list[[i]]$value_title,
            chart_type = chart_type_raw,
            # FIXED HERE
            title = filtered_list[[i]]$title,
            share = share
          )
        }

      }




    })
  })

}

