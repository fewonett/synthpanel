

#' Create a simple linear time trend
#'
#' @param time_periods The number of time periods over which the time trend should
#' be generated.
#' @param slope The slope of the linear function
#'
#' @return A numerical vector
#' @export
#'
#' @examples
#' time_periods <- 5
#' slope <- 0.5
#' trend_linear(time_periods, slope)
#'
trend_linear <- function(time_periods, slope) {
  time_trend <- slope*c(1:time_periods)
  return(time_trend)
}




#' Create a linear time trend with normally distributed error nois in each period.
#'
#' @param time_periods The number of time periods over which the time trend should
#' be generated.
#' @param slope The slope of the linear function
#' @param noise_mean The mean of the normal distribution. Should be 0 in most cases.
#' @param noise_sd The standard deviation of the normal distribution. The higher it is, the
#' more volatile the trend
#'
#' @return A numerical vector of length time_periods
#' @export
#'
#' @examples
#' time_periods <- 5
#' slope <- 0.5
#' noise_mean <- 0
#' noise_sd <- 0.5
#' trend_linear_noisy(time_periods, slope, noise_mean, noise_sd)
#'
trend_linear_noisy <- function(time_periods, slope, noise_mean, noise_sd){
  noise_term <- stats::rnorm(time_periods, noise_mean, noise_sd)
  time_series <- slope * c(1:time_periods)
  return(time_series + noise_term)
}



#' Create a time trend based on drifting brownian motion.
#'
#' @param time_periods The number of time periods over which the time trend should
#' be generated.
#' @param s_d The standard deviation of the normal distribution. The higher it is, the
#' more volatile the trend.
#'
#' @return A numerical vector of length time_periods
#' @export
#' @examples
#' time_periods <- 5
#' noise_sd <- 0.5
#' trend_brownian_motion(time_periods)
#'
trend_brownian_motion <- function(time_periods, drift = 0, s_d = 1) {
  # Generate n-1 random normal increments with mean dirft and standard deviation s_d
  increments <- stats::rnorm(time_periods, mean = drift, sd = s_d)

  # Brownian motion is the cumulative sum of these increments, starting at 0
  brownian_motion <- cumsum(increments)

  return(brownian_motion)
}









