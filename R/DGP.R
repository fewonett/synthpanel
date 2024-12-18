# This file contains the user facing functions responsible for the main DGP process.



treatment_dgp_hetero <- function(number_units, time_periods, group_number,
                                 group_share, time_of_treatment, group_te, exposure_list) {
  groups <- rep(1:group_number, group_share)

  treatment_matrix <- data.frame(
    unit = rep(1:number_units, each = time_periods),
    time = rep(1:time_periods, number_units),
    group = rep(groups, each = time_periods)
  )

  group_indices <- treatment_matrix$group

  # Create full length time of treatment and group treatment effect vectors
  treatment_start_vec <- time_of_treatment[group_indices]
  group_te_vec <- group_te[group_indices]
  treatment_matrix$time_of_treatment <- treatment_start_vec

  is_treated <- treatment_start_vec != -1
  time_since_treatment <- treatment_matrix$time - treatment_start_vec
  # Create exposure vector
  exposure_periods <- ifelse(is_treated & time_since_treatment >= 0, time_since_treatment + 1, 0)


  # Fill shorter exposure vectors up to maximum length. If vector is too short
  # (shorter than the periods the respective group is exposed), the function will
  # fill in the last entry in the exposure factor.
  # NOTE: Right now DGP_full() forces to pass vectors of max_exposure length and the
  # following code block is therefore not strictly necessary. In the future
  # this may be adjusted for easier usage.
  max_exposure <- max(exposure_periods)
  exposure_vectors_padded <- lapply(exposure_list, function(x) {
    max_val <- x[length(x)]
    length(x) <- max_exposure
    x[is.na(x)] <- max_val
    return(x)
  })

  # Group exposure matrix: Column = group, row = exposure
  exposure_matrix <- do.call(cbind, exposure_vectors_padded)

  # Assign exposure factor as vector of 0s
  exposure_factors <- numeric(nrow(treatment_matrix))
  # Gather indices of positive exposure observations
  exposure_positive <- exposure_periods > 0

  # Now gather indices for retrieving relevant values from the exposure matrix
  exposure_indices <- exposure_periods[exposure_positive]
  group_indices_positive <- group_indices[exposure_positive]

  # Create vector of exposure factors
  exposure_factors[exposure_positive] <- exposure_matrix[cbind(exposure_indices, group_indices_positive)]

  # Calculate and assign full treatement effect:
  adjusted_group_te <- group_te_vec * exposure_factors
  treatment_matrix$treatment_effect <- adjusted_group_te

  # Assign variables to the matrix
  treatment_matrix$exposure <- exposure_periods
  treatment_matrix$treat <- as.integer(is_treated & treatment_matrix$time >= treatment_start_vec)
  treatment_matrix$ever_treat <- as.integer(is_treated)

  return(treatment_matrix)
}




unit_dgp_base <- function(number_units, time_periods, trend_vectors, group_vector_long,
                          group_level, group_share) {
  # Generate Y values based on the trend_vector
  group_number <- length(unique(group_vector_long))
  group_vector <- rep(1:group_number, group_share)
  Y <- unlist(lapply(unique(group_vector), function(g) {
    rep(trend_vectors[[g]], sum(group_vector == g))
  }))

  # Create a vector of level group values
  group_level_vector <- group_level[group_vector_long]

  unit_vector <- data.frame(
    unit = rep(1:number_units, each = time_periods),
    time = rep(1:time_periods, number_units),
    Y = Y + group_level_vector
  )

  return(unit_vector)
}

error_dgp_base <- function(number_units, time_periods, s_d) {
  # Generate error term from normal distribution
  error <- stats::rnorm(number_units * time_periods, mean = 0, sd = s_d)

  error_vector <- data.frame(
    unit = rep(1:number_units, each = time_periods),
    time = rep(1:time_periods, number_units),
    error = error
  )

  return(error_vector)
}


# Simulate error with unit serial correlation:
error_dgp_spread <- function(number_units, time_periods, s_d = 1, spread_dist = "uniform",
                             error_spread = 0) {
  initial_spread <- switch(spread_dist,
    "uniform" = stats::runif(number_units, -error_spread, error_spread),
    "normal" = stats::rnorm(number_units, 0, error_spread),
    stop("Invalid spread_dist value")
  )

  error_matrix <- error_dgp_base(number_units, time_periods, s_d)
  error_matrix$error <- error_matrix$error + initial_spread[error_matrix$unit]

  return(error_matrix)
}


#' A flexible DGP function to generate panel data with differing time trends and treatment
#' effects between groups.
#'
#' @export
#'
#' @param number_units The number of observations in the dataset.
#' @param time_periods The number of time periods over which the observations are
#' observed.
#' @param group_number The number of groups in the sample. A group corresponds to
#' a distinct combination of treatment time and time trend.
#' @param group_te A vector indicating the treatment effect size for each group.
#' The length should equal the number of groups.
#' @param exposure_list A vector indicating the development of the treatment effect
#' after the initial treatment as a fraction of the respective group treatment effect
#' or a list of vectors indicating the development for each group individually.
#' Each vector should be of length time_periods - min(time_of_treatment).
#' A vector c(1,0.5, 0, 0, (...)) would indicate a treatmente effect of 1* group_te in the
#' treatment period, 0.5*group_te in the first period after treatment, and 0 after.
#' @param group_share A vector of length group_number, which indicates how many units
#' are in each group. The list entries should add up to the total number of observations.
#' The default "even" will create equally sized groups.
#' @param trend_vectors A list of vectors indicating the time trend for each group.
#' Each vector should be of length time_periods, and the list of length group_numbers.
#' Alternatively a single vector can be passed - the function will use this time trend for all groups.
#' Vectors of time trends can conveniently be created with the inbuilt trend functions.
#' @param time_of_treatment A vector of length group_number, indicating in which period
#' each group first receives treatment. Never treeated groups are marked with -1.
#' @param group_level The level of Y at time 0. Note that the time trends can also
#' introduce a Y level above 0, but the trend fucntions in this package do not.
#' @param s_d The standard deviation of the normal distribution each individual observation
#' is drawn from. Note this is independent of group level noise, which may be introduced
#' via the trend functions.
#' @param error_spread The time constant error specific to each unit is drawn from
#' a uniform distribution. error_spread defines the upper bound of that distribution.

DGP_full <- function(number_units, time_periods, group_number, group_te, time_of_treatment,
                     trend_vectors, group_level = NULL, exposure_list = NULL, group_share = "even",
                     s_d = 0, error_spread = 0) {
  # Error handling for invalid inputs and transformation of default values:
  if (!is.numeric(number_units) || length(number_units) != 1 || number_units <= 0 || number_units %% 1 != 0) {
    stop("The parameter 'number_units' must be a single positive integer.")
  }

  if (!is.numeric(time_periods) || length(time_periods) != 1 || time_periods <= 0 || time_periods %% 1 != 0) {
    stop("The parameter 'time_periods' must be a single positive integer.")
  }

  if (!is.numeric(group_number) || length(group_number) != 1 || group_number <= 0 || group_number %% 1 != 0) {
    stop("The parameter 'group_number' must be a single positive integer.")
  }

  if (!is.numeric(group_te) || length(group_te) != group_number) {
    stop("The parameter 'group_te' must be a numeric vector of length equal to 'group_number'.")
  }

  if (isTRUE(group_share == "even")) {
    # The split groups function can be found in utils.R
    group_share <- split_groups(number_units, group_number)
  } else if (!is.numeric(group_share) || length(group_share) != group_number) {
    stop("The parameter 'group_share' must be a numeric vector of length equal to 'group_number'.")
  } else if (sum(group_share) != number_units) {
    stop("The groups in group_share do not sum up to the total number of observations")
  }

  if (missing(time_of_treatment)) {
    stop("The parameter 'time_of_treatment' is missing.")
  }

  if (!is.numeric(time_of_treatment) || length(time_of_treatment) != group_number) {
    stop("The parameter 'time_of_treatment' must be a numeric vector of length equal to 'group_number'.")
  }

  if (any(time_of_treatment == 0) || any(time_of_treatment %% 1 != 0) || any(time_of_treatment < -1)) {
    stop("All elements in 'time_of_treatment' besides the never treated must be positive integers.")
  }

  if (any(time_of_treatment > time_periods)) {
    stop("Elements in 'time_of_treatment' cannot be greater than 'time_periods'.")
  }
  if ((!is.list(trend_vectors)) & length(trend_vectors) == time_periods) {
    trend_vectors <- replicate(group_number, trend_vectors, simplify = FALSE)
  }

  if (!is.list(trend_vectors) || length(trend_vectors) != group_number) {
    stop("The parameter 'trend_vectors' must be a vector of length time_periods or a list containing one such vector for each group.")
  }

  for (i in seq_along(trend_vectors)) {
    if (!is.numeric(trend_vectors[[i]]) || length(trend_vectors[[i]]) != time_periods) {
      stop(paste0("Each element in 'trend_vectors' must be a numeric vector of length equal to 'time_periods'. Element at index ", i, " is invalid."))
    }
  }

  max_length <- time_periods - min(time_of_treatment[time_of_treatment > 0]) + 1
  if (is.null(exposure_list)) {
    exposure_te <- rep(1, max_length)
    exposure_list <- replicate(group_number, exposure_te, simplify = FALSE)
  } else if (is.vector(exposure_list) & length(exposure_list) == max_length) {
    exposure_list <- replicate(group_number, exposure_list, simplify = FALSE)
  }

  if ((!is.list(exposure_list) || length(exposure_list) != group_number)) {
    stop("Exposure_te must be a list of vectors for each group or an individual vector")
  }
  for (i in seq_along(exposure_list)) {
    if (!is.numeric(exposure_list[[i]]) || length(exposure_list[[i]]) != max_length) {
      stop(paste0("Each element in 'exposure_list' must be a numeric vector of length equal to
                  time_periods - first treatment time + 1. Element at index ", i, " is invalid."))
    }
  }


  if (is.null(group_level)) {
    group_level <- rep(0, group_number)
  } else if (!is.numeric(group_level) || length(group_level) != group_number) {
    stop("The parameter 'group_level' must be a numeric vector of length equal to 'group_number'.")
  }

  if (!is.numeric(s_d) || length(s_d) != 1 || s_d < 0 || s_d %% 1 != 0) {
    stop("The parameter 's_d' must be a single positive integer.")
  }

  if (group_number > number_units) {
    stop("Group number is greater than number of observations. There will be empty groups.")
  }
  # Actual function begins here
  treatment_matrix <- treatment_dgp_hetero(
    number_units, time_periods,
    group_number, group_share, time_of_treatment,
    group_te, exposure_list
  )

  group_vector_long <- treatment_matrix$group
  unit_vector <- unit_dgp_base(
    number_units, time_periods, trend_vectors,
    group_vector_long, group_level, group_share
  )

  if (error_spread == 0) {
    error_vector <- error_dgp_base(number_units, time_periods, s_d)
  } else {
    error_vector <- error_dgp_spread(number_units, time_periods, s_d = s_d, error_spread = error_spread)
  }
  # Sum matrices function is in utils.R
  final_matrix <- sum_matrices(treatment_matrix, unit_vector, error_vector)
  final_matrix$sd <- s_d
  return(final_matrix)
}
