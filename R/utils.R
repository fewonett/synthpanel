# Mostly internal functions that do not fit into any of the other files


#' Aggregate DGP data to prepare for simple time series plot
#'
#' @param data The output dataset of DGP function
#' @keywords Internal
#' @return Aggregated data ready for ggplot use

aggregate_plot_data <- function(data) {
  # Ensure the relevant columns exist

  # Proceed with the pipeline if checks are passed
  agg_data <- data %>%
    dplyr::group_by(.data$time, .data$group) %>%
    dplyr::summarize(mean_Y = mean(.data$Y), .groups = 'drop') %>%
    dplyr::group_by(.data$group) %>%
    # Demean by first period value within each group
    dplyr::mutate(demean_Y = .data$mean_Y - mean(.data$mean_Y[.data$time == 1]))

  return(agg_data)
}



#' @keywords Internal
sum_matrices <- function(treatment_matrix, unit_vector, error_vector) {
  final_matrix <- merge(treatment_matrix, unit_vector, by = c("unit", "time"))
  final_matrix <- merge(final_matrix, error_vector, by = c("unit", "time"))

  # Adjust Y by adding treatment_effect and error
  final_matrix$Y <- final_matrix$Y + final_matrix$treatment_effect + final_matrix$error

  return(final_matrix)
}


#' @keywords Internal
split_groups <- function (nobs, group_number){
  # Define minimum number of "members" in each group
  min_members <- floor(nobs/group_number)
  # Create baseline vector with minimum member number for each group
  group_vec <- rep(min_members, group_number)
  # The first n elements in the vector that 1 another observation should be added
  # to
  obs_to_add <- nobs %% group_number
  if (obs_to_add > 0){
    group_vec[1:obs_to_add] <- group_vec[1:obs_to_add] + 1
  }
  return(group_vec)
}



