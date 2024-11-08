# All ggplot functions and their wrappers:



#' Create simple time series plot:
#'
#' @param agg_data Aggregated plot Data
#'
#' @return A ggplot object
#'
#' @keywords Internal
simple_ts_plot <- function(agg_data, demean) {
  # Create the plot
    if(demean){
      ts_plot <- ggplot2::ggplot(agg_data, ggplot2::aes(x = .data$time, y = .data$demean_Y,
                                                        color = factor(.data$group),
                                                        group = .data$group)) +
        ggplot2::geom_line(linewidth = 0.5) +
        ggplot2::geom_point(size = 1) +
        ggplot2::labs(title = "Groups over time",
                      x = "Time Period",
                      y = "Average Outcome",
                      color = "Group") +
        ggplot2::theme_minimal() +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
    }
    else{
    ts_plot <- ggplot2::ggplot(agg_data, ggplot2::aes(x = .data$time, y = .data$mean_Y,
                                                    color = factor(.data$group),
                                                    group = .data$group)) +
    ggplot2::geom_line(linewidth = 0.5) +
    ggplot2::geom_point(size = 1) +
    ggplot2::labs(title = "Groups over time",
                  x = "Time Period",
                  y = "Average Outcome",
                  color = "Group") +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
    }

  return(ts_plot)
}


#' Create a simple time series plot of the average Y over time by group.
#'
#' @param Y The outcome variable.
#' @param time Time period of the respective data point.
#' @param group The treatment group of the respective data point.
#' @param return_data If true, the function will return the aggregated data instead of the plot object.
#' @param demean If true, Y will be demeaned in period 1.
#' @return Returns a ggplot object, or the aggregated data if return_data is set to TRUE.
#' @export
#'
time_series_plot <- function(Y, time, group, demean = FALSE, return_data = FALSE){
  required_columns <- c("time", "group", "Y")

  if (!(length(Y) == length(time) && length(time) == length(group))) {
    stop("All columns must have the same length")
  }

  # Create df
  data <- data.frame(Y=Y, time = time, group = group)

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
  if (return_data){
    return(agg_data)
  }

  # Get plot:
  plot <- simple_ts_plot(agg_data, demean)
  return(plot)
}
