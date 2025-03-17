#' 05_tab2_create_highchart.R
#'
#' This script generates interactive Highcharts visualizations for the report tab.
#' It includes different chart types such as line charts, donut charts, and population pyramids.
#'
#' Functionality:
#' - Generates donut charts with custom color mappings.
#' - Creates line charts for time series data.
#' - Renders bar charts for categorical comparisons.
#' - Constructs population pyramid charts with gender-based distribution.
#'
#' Dependencies:
#' - `highcharter`, `dplyr`, `tidyr`, `RColorBrewer`
#'
#' Output:
#' - Highcharts visualizations with interactive tooltips, custom colors, and exporting options.


#' Generate a color mapping for categories
#'
#' This function assigns unique colors to categories for consistent visualization.
#'
#' @param categories A character vector of categories.
#' @return A named vector mapping colors to category names.
generate_color_mapping <- function(categories) {
  num_categories <- length(categories)

  # Define a base color palette
  colors <- c("#1d6aa2",
              "#f5a951",
              "#add9a1",

              "#a73559",
              "#96c2e8",
              "#5399a0",
              "#bebebe")

  # Extend the palette dynamically if there are more categories
  if (num_categories > length(colors)) {
    extra_colors <- colorRampPalette(RColorBrewer::brewer.pal(7, "Set3"))(num_categories - length(colors))
    colors <- c(colors, extra_colors)
  }

  # Ensure colors vector has the same length as categories
  colors <- colors[seq_along(categories)]  # Trim if necessary

  # Create a named vector with colors as names and categories as values
  setNames(categories, colors)
}



#' Create a donut chart
#'
#' Generates a Highcharts donut chart from a given dataset.
#'
#' @param df A data frame containing the data.
#' @param color_mapping A named vector mapping categories to colors.
#' @return A `highchart` object representing the donut chart.
create_donut_chart <- function(df,color_mapping) {
  total_value <- sum(df$value, na.rm = TRUE)  # Calculate total




  # Calculate total for center text
  total_value <- sum(df$value, na.rm = TRUE)

  # Create a list of objects with names, values, and colors
  data_list <- lapply(1:nrow(df), function(i) {
    list(name = df$filter1[i], y = df$value[i], color = names(color_mapping[color_mapping==df$filter1[i]]))
  })

  # Render Highcharter Pie Chart
  highchart() %>%
    hc_chart(type = "pie") %>%
    hc_plotOptions(pie = list(
      innerSize = "50%",  # Creates a donut chart
      dataLabels = list(enabled = FALSE, format = "{point.name}: {point.y}"),
      showInLegend = TRUE
    )) %>%
    # hc_title(text = paste("Summe:", total_value), align = "center", verticalAlign = "middle", style = list(fontSize = "18px")) %>%
    hc_add_series(
      name = "Value",
      data = data_list  # ✅ Assign custom colors correctly
    )

}



#' Create a categorical comparison chart
#'
#' Generates a Highcharts line or bar chart comparing values across categories.
#'
#' @param df A data frame containing the data.
#' @param gemeinde_names A named vector mapping area codes to names.
#' @param chart_type Type of chart (default: "line").
#' @param value_title Label for the y-axis.
#' @return A `highchart` object representing the chart.
create_report_chart_categories <- function(df,gemeinde_names,chart_type = "line",value_title = "Wert") {


  # ✅ Extract categories (x-axis)
  categories <- unique(df$filter1)

  # ✅ Create a list of values for each Gemeinde
  data_list <- split(df$value, df$bfs_nr_gemeinde)  # Splits values by Gemeinde

  # ✅ Extract Gemeinde names (series names)
  series_names <- names(data_list)

  # ✅ Define colors (expand if needed)
  colors <- c("#1d6aa2", "#f5a951")

  # ✅ Initialize Highchart
  hc <- highchart() %>%
    hc_chart(type = chart_type) %>%
    hc_xAxis(categories = categories, title = list(text = NULL)) %>%
    hc_yAxis(title = list(text = value_title)) %>%
    hc_plotOptions(series = list(dataLabels = list(enabled = FALSE)))

  # ✅ Add each Gemeinde as a separate series
  for (i in seq_along(data_list)) {
    hc <- hc %>%
      hc_add_series(name = names(gemeinde_names)[gemeinde_names==series_names[i]], data = data_list[[i]], color = colors[i %% length(colors) + 1])
  }

  hc %>%
    hc_exporting(enabled = TRUE)
}












#' Create a time-series chart
#'
#' Generates a Highcharts line chart showing trends over time.
#'
#' @param df A data frame containing the data.
#' @param gemeinde_names A named vector mapping area codes to names.
#' @param chart_type Type of chart (default: "line").
#' @param value_title Label for the y-axis.
#' @param filter_value Optional, filter for specific categories.
#' @return A `highchart` object representing the time-series chart.
create_report_chart_year <- function(df, gemeinde_names,chart_type = "line",value_title = "Wert",filter_value = NULL) {

  if (!is.null(filter_value)){
    df <- df %>%
      filter(filter1==filter_value)
  }

  # Define unique colors (expand as needed)
  colors <- c("#1d6aa2", "#f5a951")

  # Ensure years are numeric and data is sorted
  df <- df %>%
    mutate(jahr = as.numeric(jahr)) %>%
    arrange(bfs_nr_gemeinde, jahr)   # ✅ Ensure chronological order

  # Create a list of series data for each `bfs_nr_gemeinde`
  series_list <- df %>%
    group_by(bfs_nr_gemeinde) %>%
    summarise(
      data = list(map2(jahr, value, ~ list(.x, round(.y, 2))))  # ✅ Create [year, value] pairs, rounded
    ) %>%
    mutate(color = colors[row_number() %% length(colors) + 1])  # ✅ Assign unique colors

  # Initialize highchart
  hc <- highchart() %>%
    hc_chart(type = chart_type) %>%
    hc_xAxis(
      type = "datetime",
      title = list(text = "Jahr"),
      labels = list(format = "{value}")  # ✅ Displays years correctly
    ) %>%
    hc_yAxis(title = list(text = value_title)) %>%
    hc_plotOptions(series = list(
      dataLabels = list(enabled = FALSE)  # ✅ Removes labels from dots
    )) %>%
    hc_tooltip(
      shared = FALSE,
      useHTML = TRUE,
      headerFormat = "<b>{series.name}</b><br/>",
      pointFormat = paste0("Jahr: {point.x}<br/>",value_title,": {point.y:.2f}")  # ✅ Tooltip: Series Name, Year, Rounded Value
    )

  # Add each `bfs_nr_gemeinde` as a separate series
  for (i in seq_len(nrow(series_list))) {
    hc <- hc %>%
      hc_add_series(
        name = names(gemeinde_names)[gemeinde_names==series_list$bfs_nr_gemeinde[i]],  # ✅ Series name
        data = series_list$data[[i]],  # ✅ [year, value] pairs
        color = series_list$color[i]
      )
  }

  hc %>%
    hc_exporting(enabled = TRUE)
}


#' Create a population pyramid chart
#'
#' Generates a Highcharts population pyramid to show age and gender distribution.
#'
#' @param df A data frame containing the data.
#' @param year The selected year.
#' @param gemeinde The geographical area for the chart.
#' @param colors A character vector specifying colors for male and female groups.
#' @return A `highchart` object representing the population pyramid.
create_highchart_pyramid <- function(df, year,gemeinde, colors = c("#1d6aa2", "#f5a951")) {

  # ✅ Convert Male values to negative for left-side alignment
  df <- df %>%
    filter(bfs_nr_gemeinde==gemeinde) %>%
    pivot_wider(names_from = sex,values_from = value) %>%
    mutate(`Männlich` = -`Männlich`) %>%
    arrange(ageclass_code ) %>%
    filter(jahr ==year)     # ✅ Keep Age in natural order (young → old)

  max_value <- max(abs(c(df$`Männlich`, df$Weiblich)))  # ✅ Get max value for proper centering

  # ✅ Initialize Highchart
  hc <- highchart() %>%
    hc_chart(type = "bar", inverted = TRUE) %>%

    # ✅ X-Axis: Age groups centered at `x = 0`
    hc_xAxis(
      categories = df$ageclass,
      title = list(text = NULL),
      reversed = FALSE,
      lineWidth = 1,
      lineColor = "black"
    ) %>%

    # ✅ Y-Axis: Labels positioned **exactly at `x = 0`**
    hc_yAxis(
      title = list(text = "Anzahl"),
      opposite = FALSE,  # ✅ Keep axis in the center
      reversedStacks = FALSE,  # ✅ Prevent flipping of bars
      labels = list(formatter = JS("function() { return Math.abs(this.value); }")),
      plotLines = list(
        list(
          color = "black", width = 2, value = 0  # ✅ Black vertical line at `x = 0`
        )
      )
    ) %>%

    hc_plotOptions(series = list(
      stacking = "normal",
      groupPadding = 0.05,  # ✅ Small space between bars
      pointPadding = 0.05,  # ✅ Small space inside groups
      borderWidth = 0
    )) %>%

    hc_tooltip(shared = FALSE, formatter = JS(
      "function() {
        return '<b>' + this.series.name + ', age ' + this.point.category + '</b><br/>' +
               'Population: ' + Highcharts.numberFormat(Math.abs(this.point.y), 0);
      }"
    )) %>%

    hc_legend(reversed = TRUE) %>%
    hc_add_series(name = "Männlich", data = df$`Männlich`, color = colors[1]) %>%
    hc_add_series(name = "Weiblich", data = df$Weiblich, color = colors[2]) %>%
    hc_exporting(enabled = TRUE)

  return(hc)
}
