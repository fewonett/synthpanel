

#' Calculates the true treatment effect on a group x time level
#'
#' @export
#'
#' @param treatment_effect The vector of true individual treatment effects as generated
#' by the DGP functions.
#' @param group The group indicator vector as produced by the DGP functions.
#' @param time The time indicator vector as produced by the DGP functions.
#' @return A data frame that presents mean treatment effect by time x group.

group_time_te <- function(treatment_effect, group, time){
  # Check inputs
  a <- data.frame(treatment_effect = treatment_effect, time = time, group = group)
  b <- a %>%
    dplyr::group_by(group, time) %>%
    dplyr::summarise(mean_te = mean(treatment_effect))
  return(b)

}

#' Calculates the true treatment effect on a group level
#'
#' @export
#'
#' @param treatment_effect The vector of true individual treatment effects as generated
#' by the DGP functions.
#' @param group The group indicator vector as produced by the DGP functions.
#' @param time The time indicator vector as produced by the DGP functions.
#' @return A data frame that presents mean treatment effect by group.


group_te <- function(treatment_effect, group, time){
  te_time_group <- group_time_te(treatment_effect, group, time)
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
#' @param treatment_effect The vector of true individual treatment effects as generated
#' by the DGP functions.
#' @param group The group indicator vector as produced by the DGP functions.
#' @param time The time indicator vector as produced by the DGP functions.
#' @return An integer representing the ATT.

att_group <- function(treatment_effect, group, time){
  group_count <- as.data.frame(table(group))
  colnames(group_count)[1] <- "group"
  group_te_table <- group_te(treatment_effect, group, time)
  # Remove groups with mean_te == 0
  group_te_table <- group_te_table[group_te_table$mean_te != 0,]
  # Merge the group counts:
  group_te_table <- merge(group_te_table, group_count, by.x = "group", by.y = "group", all.x = FALSE)
  # Calculate the weighted average:
  ATT <- sum((group_te_table$mean_te * group_te_table$Freq))/sum(group_te_table$Freq)
  return(ATT)
}

#' Calculates the ATT_simple
#' @export
#'
#' @param treatment_effect The vector of true individual treatment effects as generated
#' by the DGP functions.
#' @param treat The treatment indicator: 1 when if respective unit is treated in that
#' time perios, 0 otherwise.
#' @return An integer representing the ATT_simple.

att_simple <- function(treatment_effect, treat){

  att_frame <- data.frame(treatment_effect = treatment_effect, treat = treat)
  # Remove untreated units
  att_frame <- att_frame %>%
    dplyr::filter(.data$treat == 1)
  att_simple <- mean(att_frame$treatment_effect)
  return(att_simple)
}



