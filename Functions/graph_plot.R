#' graph_plot
#' 
#' @param data Data frame that needs svymean calculated
#' 
#' @returns Data frame with new named LA column


graph_plot <- function(data, 
                       var_name, 
                       var_value, 
                       plot_title,
                       colour_fill = sg_colour_values["dark-blue"], 
                       colour_error = sg_colour_values[4]) {
  
  # Filter dynamically based on variable name & value
  data_filtered <- data %>% 
    filter(.data[[var_name]] == var_value)
  
  # Build plot for summary data
  p <- ggplot(data_filtered, aes(x = Year, y = perc)) +
    geom_point(size = 3, colour = colour_fill) +
    geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci),
                  width = 0.5, colour = colour_error) +
    labs(
      x = NULL,
      y = NULL,
      title = plot_title
    ) +
    theme_minimal()
  
  # Print automatically
  print(p)
  
  # Return the object so you can still use ggsave()
  invisible(p)
}