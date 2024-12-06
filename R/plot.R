# All ggplot functions and their wrappers:




simple_ts_plot <- function(agg_data, demean) {
  # Create the plot
  if (demean) {
    ts_plot <- ggplot2::ggplot(agg_data, ggplot2::aes(
      x = .data$time, y = .data$demean_Y,
      color = factor(.data$group),
      group = .data$group
    )) +
      ggplot2::geom_line(linewidth = 0.5) +
      ggplot2::geom_point(size = 1) +
      ggplot2::labs(
        title = "Groups over time",
        x = "Time Period",
        y = "Average Outcome",
        color = "Group"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
  } else {
    ts_plot <- ggplot2::ggplot(agg_data, ggplot2::aes(
      x = .data$time, y = .data$mean_Y,
      color = factor(.data$group),
      group = .data$group
    )) +
      ggplot2::geom_line(linewidth = 0.5) +
      ggplot2::geom_point(size = 1) +
      ggplot2::labs(
        title = "Groups over time",
        x = "Time Period",
        y = "Average Outcome",
        color = "Group"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
  }

  return(ts_plot)
}



#' Create a simple time series plot of the average Y over time by group.
#'
#' @param data A data frame containing the data.
#' @param Y The name of the column in `data` that contains the outcome variable (default is `"Y"`).
#' @param time The name of the column in `data` that contains the time periods (default is `"time"`).
#' @param group The name of the column in `data` that contains the treatment groups (default is `"group"`).
#' @param demean Logical. If `TRUE`, the outcome variable Y will be demeaned in period 1 (default is `FALSE`).
#' @param return_data Logical. If `TRUE`, the function will return the aggregated data instead of the plot object (default is `FALSE`).
#' @return Returns a ggplot object of the average Y over time by group, or the aggregated data if `return_data` is set to `TRUE`.
#' @export
#'
time_series_plot <- function(data, Y = "Y", time = "time", group = "group", demean = FALSE, return_data = FALSE) {
  # Ensure 'time' and 'Y' are numeric
  if (!is.numeric(data$time)) {
    stop("'time' column must be numeric.")
  }
  if (!is.numeric(data$Y)) {
    stop("'Y' column must be numeric.")
  }

  # Aggregate data
  agg_data <- aggregate_plot_data(data)

  # return only the data if return data is true:
  if (return_data) {
    return(agg_data)
  }

  # Get plot:
  plot <- simple_ts_plot(agg_data, demean)
  return(plot)
}
