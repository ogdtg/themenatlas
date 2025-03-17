#' 05_tab2_render_content.R
#'
#' This script handles the rendering of Highcharts visualizations, data tables, and external data source links
#' in the report tab. It ensures that charts and tables update dynamically based on user selections.
#'
#' Functionality:
#' - Renders donut charts, line charts, bar charts, and population pyramids.
#' - Displays data tables and indicator tables based on user input.
#' - Generates dynamic UI elements for linking external data sources.
#' - Supports filtering data based on user-selected years and regions.
#'
#' Dependencies:
#' - `highcharter`, `DT`, `dplyr`, `tidyr`, `shiny`, `htmltools`
#'
#' Output:
#' - Highcharts visualizations embedded in the Shiny UI.
#' - Data tables formatted for interactive display.
#' - UI elements containing links to external data sources.




#' Render a donut chart in a Shiny session
#'
#' This function renders a Highcharts donut chart based on user-selected year and region.
#'
#' @param session The Shiny session object.
#' @param id The output ID for the chart.
#' @param input The Shiny input object.
#' @param output The Shiny output object.
#' @param reactive_val A reactive value representing the selected region.
#' @param data The data frame containing the chart data.
#' @param trigger_inputs A list of input triggers for updating the chart.
#' @param bezirk_data_names A named vector mapping area codes to names.
#' @param color_mapping A named vector mapping categories to colors.
render_donut_chart <- function(session, id, input, output, reactive_val, data, trigger_inputs,bezirk_data_names,color_mapping) {

  select_id <- paste0("select_year_",clean_string(input$report_topic),"_",str_extract(id,"\\d+"))


  req(reactive_val(),input[[select_id]])


  year <- input[[select_id]]


  year <- input[[select_id]]
  gemeinde <- names(bezirk_data_names)[bezirk_data_names==reactive_val()]

  df <- data %>%
    filter(bfs_nr_gemeinde == reactive_val(),
           jahr == year) %>%
    arrange(filter1)


  output[[id]] <- renderHighchart({
    create_donut_chart(df,color_mapping)
  })

  output[[paste0(id,"_title")]] <- renderUI({
    HTML(paste0('<div style="text-align: center;"><h3><b>', gemeinde, '</b></h3></div>'))
  })
}














#' Render a single chart (line or column) in a Shiny session
#'
#' This function renders a Highcharts chart for categorical or time-series data.
#'
#' @param session The Shiny session object.
#' @param id The output ID for the chart.
#' @param input The Shiny input object.
#' @param output The Shiny output object.
#' @param data The data frame containing the chart data.
#' @param selected_base_area The selected base area.
#' @param selected_compare_area The area for comparison.
#' @param ts Logical, whether the chart is a time series.
#' @param chart_type The type of chart ("line", "column", etc.).
#' @param value_title Label for the y-axis.
#' @param gemeinde_names A named vector mapping area codes to names.
#' @param filter_value Optional, filter for specific categories.
#' @param share Logical, whether to display percentage shares.
render_single_chart <- function(session, id, input, output, data,selected_base_area,selected_compare_area,ts=FALSE,chart_type = "line",value_title = "Wert",gemeinde_names = bezirk_data_names,filter_value = NULL,share = FALSE) {
  req(selected_base_area(),selected_compare_area())



  data <- data %>%
    filter(bfs_nr_gemeinde %in% c(selected_base_area(),selected_compare_area()))

  if (share){
    data$value <- data$share %>% round(1)
  }

  if(ts){

    output[[id]] <- renderHighchart({
      create_report_chart_year(df=data,chart_type=chart_type,value_title=value_title,gemeinde_names=gemeinde_names,filter_value=filter_value)
    })

  } else {

    select_id <- paste0("select_year_",clean_string(input$report_topic),"_",str_extract(id,"\\d+"))

    year <- input[[select_id]]

    ##print(paste0("render_single_chart: ",year))


    data <- data %>%
      filter(jahr == year)


    output[[id]] <- renderHighchart({
      create_report_chart_categories(df=data,chart_type=chart_type,value_title=value_title,gemeinde_names=gemeinde_names)
    })


  }




}


#' Render both a chart and a table for a given dataset
#'
#' This function renders a Highcharts chart and a corresponding data table for the report tab.
#'
#' @param session The Shiny session object.
#' @param output The Shiny output object.
#' @param input The Shiny input object.
#' @param selected_base_area The selected base area.
#' @param selected_compare_area The area for comparison.
#' @param topic The selected topic.
#' @param data The data frame containing the data.
#' @param id_num A numeric identifier for the output elements.
#' @param title The title of the report.
#' @param ts Logical, whether the chart/table is a time series.
#' @param filter_value Optional, filter for specific categories.
#' @param bezirk_data_names A named vector mapping area codes to names.
#' @param value_title Label for the y-axis.
#' @param chart_type The type of chart ("line", "column", etc.).
#' @param share Logical, whether to display percentage shares.
render_single_chart_and_table <- function(session,output,input,selected_base_area,selected_compare_area, topic, data,id_num,title,ts=FALSE,filter_value=NULL,bezirk_data_names,value_title="Wert",chart_type="column",share = FALSE){


  # data <- nested_list$`Bevölkerung und Soziales`$Bevölkerungsstand$`Bevölkerungsverteilung nach Konfession`

  if (ts){
    # observeEvent(list(input$report_topic,selected_base_area(),selected_compare_area(),input$report_year),{

      if (input$report_topic==topic){

        render_report_table(session, id = paste0("report_table_",clean_string(topic),"_", id_num), input, output, data, bezirk_data_names, selected_base_area, selected_compare_area,ts = ts,filter_value = filter_value,title=title)

        render_single_chart(session, id = paste0("report_chart_",clean_string(topic),"_",id_num), input, output, data,selected_base_area,selected_compare_area,ts,chart_type ,value_title,gemeinde_names = bezirk_data_names,filter_value = filter_value,share=share)

      }
    # })
  } else {
    # observeEvent(list(input$report_topic,selected_base_area(),selected_compare_area()),{

      if (input$report_topic==topic){

        render_report_table(session, id = paste0("report_table_",clean_string(topic),"_", id_num), input, output, data, bezirk_data_names, selected_base_area, selected_compare_area,ts = ts,filter_value = filter_value,title=title)

        render_single_chart(session, id=paste0("report_chart_",clean_string(topic),"_",id_num), input, output, data,selected_base_area,selected_compare_area,ts,chart_type ,value_title,gemeinde_names = bezirk_data_names,filter_value = filter_value,share = share)

      }
    # })
  }





}








#' Render a population pyramid in a Shiny session
#'
#' This function renders a Highcharts population pyramid for a selected region and year.
#'
#' @param session The Shiny session object.
#' @param id The output ID for the chart.
#' @param input The Shiny input object.
#' @param output The Shiny output object.
#' @param reactive_val A reactive value representing the selected region.
#' @param data The data frame containing the population data.
#' @param bezirk_data_names A named vector mapping area codes to names.
render_pyramid <- function(session, id, input, output, reactive_val, data,bezirk_data_names) {
  select_id <- paste0("select_year_",clean_string(input$report_topic),"_",str_extract(id,"\\d+"))

  req(reactive_val(),input[[select_id]])


  gemeinde <- names(bezirk_data_names)[bezirk_data_names==reactive_val()]
  year <- input[[select_id]]



  output[[id]] <- renderHighchart({
    create_highchart_pyramid(df=data,year=year,gemeinde = reactive_val())
  })

  output[[paste0(id,"_title")]] <- renderUI({
    HTML(paste0('<div style="text-align: center;"><h3><b>', gemeinde, '</b></h3></div>'))
  })
}

#' Render data source links in the UI
#'
#' This function generates clickable links to external data sources based on the dataset being displayed.
#'
#' @param output The Shiny output object.
#' @param id The output ID for the UI element.
#' @param data_path The data path used to retrieve metadata.
render_data_sources <- function(output,id,data_path){

  data_source <- data_source_list[[data_path]]

  if (!is.null(data_source$id)){


    output[[id]] <- renderUI({
      tagList(
        lapply(seq_along(data_source$id), function(i) {
          id_temp <- data_source$id[i]
          if (str_detect(id_temp,"px")){
            url = paste0("https://www.pxweb.bfs.admin.ch/pxweb/de/",id_temp,"/-/",id_temp,".px/")
          } else {
            url = paste0("https://data.tg.ch/explore/dataset/",id_temp,"/table/")
          }


          tags$a(href = url, data_source$title[i], target = "_blank", style = "display:block; margin-bottom:5px;")
        })
      )
    })
  }


}





#' Render data source links for indicator tables
#'
#' This function generates clickable links to external data sources based on an indicator list.
#'
#' @param output The Shiny output object.
#' @param id The output ID for the UI element.
#' @param indicator_list A list of indicator metadata containing data paths.
render_data_sources_indicator_table <- function(output,id,indicator_list){


  urls <- c()
  titles <- c()

  for (elem in indicator_list){
    data_source <- data_source_list[[elem$data_path]]

    if (!is.null(data_source$id)){
      for (i in seq_along(data_source$id)){
        id_temp <- data_source$id[i]
        if (str_detect(id_temp,"px")){
          url = paste0("https://www.pxweb.bfs.admin.ch/pxweb/de/",id_temp,"/-/",id_temp,".px/")
        } else {
          url = paste0("https://data.tg.ch/explore/dataset/",id_temp,"/table/")
        }
        urls <- c(urls,url)
        titles <- c(titles,data_source$title[i])

      }
    }

  }
  urls <- unique(urls)
  titles <- unique(titles)

  if (length(urls)>0){
    output[[id]] <- renderUI({
      tagList(
        lapply(seq_along(urls), function(i) {


          tags$a(href = urls[i], titles[i], target = "_blank", style = "display:block; margin-bottom:5px;")
        })
      )
    })
  }



}

