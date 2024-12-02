
#' Calculate dynamic weights
#'
#' @param data The dataset.
#' @param group The name of the group variable column
#' @param time The name of the time variable column
#' @param treat_indicator The name of the treatment indicator column
#'
#' @return A numerical vector that contains the weights for each observation in the passed dataset
#' @export
#'
weights_dyn <- function(data, group, time, treat_indicator){

  data <- data %>%
    dplyr::group_by(.data[[group]]) %>%
    dplyr::mutate(
      first_treat = ifelse(
        any(.data[[treat_indicator]] == 1),
        min(.data[[time]][.data[[treat_indicator]] == 1]),
        0
      )
    ) %>%
    dplyr::group_by(.data[[time]]) %>%
    dplyr::mutate(
      exposure = ifelse(
        .data[[treat_indicator]] == 0,
        0,
        1 + .data[[time]] - .data$first_treat
      )
    ) %>%
    dplyr::ungroup()

  data <- data %>%
    dplyr::group_by(.data$exposure) %>%
    dplyr::mutate(
      exposure_count = max(cumsum(.data[[treat_indicator]]))
    ) %>%
    dplyr::ungroup()

  data <- data %>%
    dplyr::group_by(.data[[group]], .data$exposure) %>%
    dplyr::mutate(
      w_es = ifelse(.data$exposure_count == 0, 1, 1 / .data$exposure_count)
    ) %>%
    dplyr::ungroup()

  data$w_es <- data$w_es / max(data$exposure)

  return(data$w_es)
}

#' Calculate group weights
#'
#' @param data The dataset.
#' @param unit The unit variable name.
#' @param group The name of the group variable column
#' @param time The name of the time variable column
#' @param treat_indicator The name of the treatment indicator column
#'
#' @return A numerical vector that contains the weights for each observation in the passed dataset
#' @export
#'

weights_group <- function(data, unit, group, time, treat_indicator){

  n_treated_units <- data %>%
    dplyr::filter(.data[[treat_indicator]] == 1) %>%
    dplyr::summarise(n_treated_units = dplyr::n_distinct(.data[[unit]])) %>%
    dplyr::pull(n_treated_units)
  n_treated_units <- max(cumsum(n_treated_units))

  data <- data %>%
    dplyr::mutate(max_time = max(.data[[time]])) %>%
    dplyr::group_by(.data[[group]]) %>%
    dplyr::mutate(first_treat = ifelse(any(.data[[treat_indicator]] == 1),
                                       min(.data[[time]][.data[[treat_indicator]] == 1]),
                                       0)) %>%
    dplyr::mutate(w_g = ifelse(.data[[treat_indicator]] == 0,
                               1,
                               1 / (1 + .data$max_time - .data$first_treat))) %>%
    dplyr::ungroup()

  data$w_g <- data$w_g / n_treated_units
  return(data$w_g)
}
