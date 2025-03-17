# Functions to create Tab 2

create_tab_2 <- function(input_scatter){
  data <- data.frame(x=runif(input_scatter),y = runif(input_scatter))

  renderPlot(
    data |>
      ggplot(aes(x,y)) +
      geom_point()
  )
}
