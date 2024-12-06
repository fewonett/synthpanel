#' Calculates the true treatment effect on a group x time level
#'
#' @export
#'
#' @param data A data frame containing the data.
#' @param treatment_effect The name of the column in `data` that contains the true individual treatment effects (default `"treatment_effect"`).
#' @param group The name of the column in `data` that contains the group indicators (default `"group"`).
#' @param time The name of the column in `data` that contains the time indicators (default `"time"`).
#' @return A data frame that presents mean treatment effect by time x group.
group_time_te <- function(data, treatment_effect = "treatment_effect", group = "group", time = "time") {
  # Check inputs
  attgt <- data %>%
    dplyr::group_by(.data[[group]], .data[[time]]) %>%
    dplyr::summarise(mean_te = mean(.data[[treatment_effect]]))
  return(attgt)
}


#' Calculates the true treatment effect on a group level
#'
#' @export
#'
#' @param data A data frame containing the data.
#' @param treatment_effect The name of the column in `data` that contains the true individual treatment effects (default `"treatment_effect"`).
#' @param group The name of the column in `data` that contains the group indicators (default `"group"`).
#' @param time The name of the column in `data` that contains the time indicators (default `"time"`).
#' @return A data frame that presents mean treatment effect by group.
group_te <- function(data, treatment_effect = "treatment_effect", group = "group", time = "time") {
  te_time_group <- group_time_te(data, treatment_effect, group, time)
  # Remove rows with mean_te 0, in treatment groups
  te_time_group <- te_time_group %>%
    dplyr::group_by(.data$group) %>%
    dplyr::filter(!(.data$mean_te == 0 & any(.data$mean_te != 0))) %>%
    dplyr::ungroup()

  group_te_table <- te_time_group %>%
    dplyr::group_by(.data$group) %>%
    dplyr::summarise(mean_te = mean(.data$mean_te))
  return(group_te_table)
}


#' Calculates the average treatment effect of the treated (ATT)
#'
#' @export
#'
#' @param data A data frame containing the data.
#' @param treatment_effect The name of the column in `data` that contains the true individual treatment effects (default `"treatment_effect"`).
#' @param group The name of the column in `data` that contains the group indicators (default `"group"`).
#' @param time The name of the column in `data` that contains the time indicators (default `"time"`).
#' @return A numeric value representing the ATT_group.
att_group <- function(data, treatment_effect = "treatment_effect", group = "group", time = "time") {
  group_count <- as.data.frame(table(data[[group]]))
  colnames(group_count)[1] <- "group"
  group_te_table <- group_te(data, treatment_effect, group, time)
  # Remove groups with mean_te == 0
  group_te_table <- group_te_table[group_te_table$mean_te != 0, ]
  # Merge the group counts:
  group_te_table <- merge(group_te_table, group_count, by.x = "group", by.y = "group", all.x = FALSE)
  # Calculate the weighted average:
  ATT <- sum((group_te_table$mean_te * group_te_table$Freq)) / sum(group_te_table$Freq)
  return(ATT)
}

#' Calculates the simple average treatment effect of the treated (ATT_simple)
#'
#' @export
#'
#' @param data A data frame containing the data.
#' @param treatment_effect The name of the column in `data` that contains the true individual treatment effects (default `"treatment_effect"`).
#' @param treat The name of the column in `data` that indicates whether the unit is treated (1) or not (0) in a given time period (default `"treat"`).
#' @return A numeric value representing the ATT_simple.
att_simple <- function(data, treatment_effect = "treatment_effect", treat = "treat") {
  # Remove untreated units
  att_frame <- data %>%
    dplyr::filter(.data[[treat]] == 1)
  att_simple <- mean(att_frame[[treatment_effect]])
  return(att_simple)
}
