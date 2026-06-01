#' fct_berichte_highchart.R  (migrated from highcharter → echarts4r)
#'
#' Chart creation helpers for the Berichte tab.
#' All functions return echarts4r widgets.


#' Generate a colour mapping for categories (unchanged utility)
#'
#' @param categories Character vector of category labels.
#' @return Named vector: colour names → category values.
generate_color_mapping <- function(categories) {
  colors <- c("#1d6aa2", "#f5a951", "#add9a1", "#a73559",
              "#96c2e8", "#5399a0", "#bebebe")
  if (length(categories) > length(colors)) {
    colors <- c(colors,
                colorRampPalette(RColorBrewer::brewer.pal(7, "Set3"))(
                  length(categories) - length(colors)))
  }
  colors <- colors[seq_along(categories)]
  setNames(categories, colors)
}


#' Create a donut (pie) chart with echarts4r
#'
#' @param df Data frame with columns `filter1` and `value`.
#' @param color_mapping Named vector from [generate_color_mapping()].
#' @return An echarts4r widget.
create_donut_chart <- function(df, color_mapping) {
  df_plot <- df %>%
    mutate(color = names(color_mapping)[match(filter1, color_mapping)])

  df_plot %>%
    echarts4r::e_charts(filter1) %>%
    echarts4r::e_pie(
      value,
      radius     = c("40%", "70%"),
      label      = list(show = FALSE),
      emphasis   = list(label = list(show = TRUE, formatter = "{b}: {d}%"))
    ) %>%
    echarts4r::e_color(names(color_mapping)) %>%
    echarts4r::e_legend(orient = "vertical", right = 0) %>%
    echarts4r::e_tooltip(
      trigger   = "item",
      formatter = "{b}: {c} ({d}%)"
    ) %>%
    echarts4r::e_toolbox_feature("saveAsImage")
}


#' Create a categorical comparison chart (bar or line) with echarts4r
#'
#' @param df Data frame with columns `filter1`, `value`, `bfs_nr_gemeinde`.
#' @param gemeinde_names Named vector: name → bfs_nr.
#' @param chart_type `"line"` or `"bar"` (default `"line"`).
#' @param value_title Y-axis label.
#' @return An echarts4r widget.
create_report_chart_categories <- function(df, gemeinde_names,
                                           chart_type  = "line",
                                           value_title = "Wert") {
  colors <- c("#1d6aa2", "#f5a951")

  series_ids <- unique(df$bfs_nr_gemeinde)

  # Build one data frame per series and merge on filter1 categories
  categories <- unique(df$filter1)

  base <- data.frame(filter1 = categories, stringsAsFactors = FALSE)
  for (i in seq_along(series_ids)) {
    sid   <- series_ids[i]
    sname <- names(gemeinde_names)[gemeinde_names == sid]
    sdata <- df %>% filter(bfs_nr_gemeinde == sid) %>% select(filter1, value)
    base  <- base %>% left_join(sdata, by = "filter1")
    names(base)[ncol(base)] <- sname
  }

  chart <- base %>% echarts4r::e_charts(filter1)
  for (i in seq_along(series_ids)) {
    sname <- names(gemeinde_names)[gemeinde_names == series_ids[i]]
    if (chart_type == "line") {
      chart <- chart %>% echarts4r::e_line_(sname, color = colors[i])
    } else {
      chart <- chart %>% echarts4r::e_bar_(sname, color = colors[i])
    }
  }

  chart %>%
    echarts4r::e_y_axis(name = value_title) %>%
    echarts4r::e_tooltip(trigger = "axis") %>%
    echarts4r::e_legend() %>%
    echarts4r::e_toolbox_feature("saveAsImage")
}


#' Create a time-series chart (line) with echarts4r
#'
#' @param df Data frame with columns `jahr`, `value`, `bfs_nr_gemeinde`.
#' @param gemeinde_names Named vector: name → bfs_nr.
#' @param chart_type `"line"` or `"bar"`.
#' @param value_title Y-axis label.
#' @param filter_value Optional filter for `filter1` column.
#' @return An echarts4r widget.
create_report_chart_year <- function(df, gemeinde_names,
                                     chart_type   = "line",
                                     value_title  = "Wert",
                                     filter_value = NULL) {
  if (!is.null(filter_value)) df <- df %>% filter(filter1 == filter_value)

  colors     <- c("#1d6aa2", "#f5a951")
  series_ids <- unique(df$bfs_nr_gemeinde)

  df <- df %>% mutate(jahr = as.character(as.integer(jahr))) %>% arrange(jahr)

  base <- data.frame(jahr = unique(df$jahr), stringsAsFactors = FALSE)
  for (i in seq_along(series_ids)) {
    sid   <- series_ids[i]
    sname <- names(gemeinde_names)[gemeinde_names == sid]
    sdata <- df %>% filter(bfs_nr_gemeinde == sid) %>%
      mutate(value = round(value, 2)) %>%
      select(jahr, value)
    base  <- base %>% left_join(sdata, by = "jahr")
    names(base)[ncol(base)] <- sname
  }

  chart <- base %>% echarts4r::e_charts(jahr)
  for (i in seq_along(series_ids)) {
    sname <- names(gemeinde_names)[gemeinde_names == series_ids[i]]
    if (chart_type == "line") {
      chart <- chart %>% echarts4r::e_line_(sname, color = colors[i])
    } else {
      chart <- chart %>% echarts4r::e_bar_(sname, color = colors[i])
    }
  }

  chart %>%
    echarts4r::e_x_axis(name = "Jahr") %>%
    echarts4r::e_y_axis(name = value_title) %>%
    echarts4r::e_tooltip(trigger = "axis") %>%
    echarts4r::e_legend() %>%
    echarts4r::e_toolbox_feature("saveAsImage")
}


#' Create a population pyramid with echarts4r
#'
#' @param df Data frame with columns `bfs_nr_gemeinde`, `jahr`, `ageclass`,
#'   `ageclass_code`, `sex`, `value`.
#' @param year Selected year (character or numeric).
#' @param gemeinde The `bfs_nr_gemeinde` to display.
#' @param colors Length-2 colour vector for Männlich / Weiblich.
#' @return An echarts4r widget.
create_highchart_pyramid <- function(df, year, gemeinde,
                                     colors = c("#1d6aa2", "#f5a951")) {
  df <- df %>%
    filter(bfs_nr_gemeinde == gemeinde, jahr == year) %>%
    tidyr::pivot_wider(names_from = sex, values_from = value) %>%
    arrange(ageclass_code)

  # Männlich values are negative so they appear on the left
  df <- df %>% mutate(`Männlich` = -`Männlich`)

  df %>%
    echarts4r::e_charts(ageclass) %>%
    echarts4r::e_bar(`Männlich`, stack = "pyramid",
                     color = colors[1],
                     label = list(show = FALSE)) %>%
    echarts4r::e_bar(`Weiblich`, stack = "pyramid",
                     color = colors[2],
                     label = list(show = FALSE)) %>%
    echarts4r::e_flip_coords() %>%
    echarts4r::e_y_axis(
      axisLabel = list(
        formatter = htmlwidgets::JS("function(v){ return Math.abs(v); }")
      )
    ) %>%
    echarts4r::e_tooltip(
      trigger   = "axis",
      formatter = htmlwidgets::JS("function(params){
        var out = params[0].axisValue + '<br/>';
        params.forEach(function(p){
          out += p.marker + p.seriesName + ': ' +
                 Math.abs(p.value) + '<br/>';
        });
        return out;
      }")
    ) %>%
    echarts4r::e_legend() %>%
    echarts4r::e_toolbox_feature("saveAsImage")
}
