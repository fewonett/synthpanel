get_borus <- function(data, Y, first_treat, time, unit, cluster_by = NA, weights = NA){
  if(is.na(cluster_by)){
    cluster_by <- unit
  }
  if(any(is.na(weights))){
    runtime <- system.time({
      att_est <- didimputation::did_imputation(data, yname = Y,gname = first_treat,
                                               tname = time, idname = unit, cluster_var = cluster_by)
    })["elapsed"]
    runtime <- as.numeric(runtime)

  }else{
    runtime <- system.time({
      att_est <- didimputation::did_imputation(data, yname = Y,gname = first_treat,
                                               tname = time, idname = unit, wname =weights,
                                               cluster_var = cluster_by)
    })["elapsed"]
    runtime <- as.numeric(runtime)

  }


  frame <- data.frame(estimator = "borus" ,
                      att_est = att_est$estimate,
                      std.err = att_est$std.error,
                      runtime = runtime
  )
  return(frame)
}


get_sunab <- function (data, Y, first_treat, time , unit, cluster_by = NA, weights= NA){
  if(is.na(cluster_by)){
    cluster_by <- unit
  }
  cluster_by <- c(cluster_by)

  if(is.na(weights)){
    runtime <- system.time({
      sunab <- fixest::feols(Y ~ sunab(first_treat, time)
                             | unit + time, data, cluster = cluster_by)

      att_est <- stats::aggregate(sunab, "att")
    })["elapsed"]
  } else{
    formula <- stats::as.formula(paste("~", weights))
    runtime <- system.time({
      sunab <- fixest::feols(Y ~ sunab(first_treat, time)
                             | unit + time, data, cluster = cluster_by, weights = formula)

      att_est <- stats::aggregate(sunab, "att")
    })["elapsed"]
  }

  runtime <- as.numeric(runtime)
  frame <- data.frame(estimator = "sunab" ,
                      att_est = att_est[1],
                      std.err = att_est[2],
                      runtime = runtime
  )
  return(frame)
}


get_chaise <- function(data, Y, treat, time, unit, group, weights = NA){
  # Calculate number of effects to compute:
  first_t <- data %>%
    dplyr::filter(.data[[treat]] == 1) %>%
    dplyr::slice_min(.data[[time]])
  first_t <- min(first_t[[time]])
  number_effects <- max(data[[time]]) - first_t +1

  if(is.na(weights)){

    runtime <- system.time({
      chaise <- DIDmultiplegtDYN::did_multiplegt_dyn(data, Y, unit, time, treat, effects = number_effects, graph_off = TRUE)
    })["elapsed"]
  }else{
    runtime <- system.time({
      chaise <- DIDmultiplegtDYN::did_multiplegt_dyn(data, Y, unit, time, treat, effects = number_effects, weight = weights, graph_off = TRUE)
    })["elapsed"]
  }
  runtime <- as.numeric(runtime)

  frame <- data.frame(estimator = "chaise" ,
                      att_est = chaise$results$ATE[1],
                      std.err = chaise$results$ATE[2],
                      runtime = runtime
  )
  return(frame)
}


get_wool <- function(data, Y, first_treat, time, unit, group, cluster_by = NA, control = "notyet", aggregation = "simple"){

  if(is.na(cluster_by)){
    cluster_by <- unit
  }

  formula <- stats::as.formula(paste("~", cluster_by))
  runtime <-system.time({
    wool =
      etwfe::etwfe(
        fml  = Y ~ 0,
        tvar = time,
        gvar = first_treat,
        data = data,
        vcov = formula,
        cgroup = control
      )
    wool <- etwfe::emfx(wool, type = aggregation)
  })["elapsed"]

  runtime <- as.numeric(runtime)

  frame <- data.frame(estimator = "wool" ,
                      att_est = wool$estimate,
                      std.err = wool$std.error,
                      runtime = runtime
  )

  return(frame)
}


get_twfe <- function(data, Y, treat, time, unit, cluster_by){
  if(is.na(cluster_by)){
    cluster_by <- unit
  }
  cluster_by <- c(cluster_by)

  runtime <- system.time({
    twfe <- fixest::feols(Y ~ treat | unit + time,
                          data=data, cluster = cluster_by)
  })["elapsed"]
  runtime <- as.numeric(runtime)


  frame <- data.frame(estimator = "twfe" ,
                      att_est = as.numeric((twfe$coefficients)),
                      std.err = as.numeric(twfe$se[1]),
                      runtime = runtime
  )
  return(frame)
}


get_gardner <- function(data, Y, treat, time, unit, group, cluster_by = NA, weights = NA){

  if(is.na(cluster_by)){
    cluster_by <- unit
  }
  if(is.na(weights)){

    runtime <- system.time({
      gardner <- did2s::did2s(
        data,
        yname = Y, first_stage = ~ 0 | unit + time,
        second_stage = ~ treat, treatment = treat,
        cluster_var = cluster_by
      )
    })["elapsed"]
  }else{
    runtime <- system.time({
      gardner <- did2s::did2s(
        data,
        yname = Y, first_stage = ~ 0 | unit + time,
        second_stage = ~ treat, treatment = treat,
        cluster_var = cluster_by,
        weights = weights
      )
    })["elapsed"]
  }
  runtime <- as.numeric(runtime)


  frame <- data.frame(estimator = "gardner",
                      att_est = gardner$coeftable[1],
                      std.err = gardner$coeftable[2],
                      runtime = runtime
  )
  return(frame)
}


get_cs <- function(data, Y, first_treat, time, unit, control ="notyettreated",
                   aggregation = "simple", cluster_by = NA){

  if(is.na(cluster_by)){
    cluster_by <- unit
  }

  runtime <- system.time({
    cs_res <- did::att_gt(yname = Y,
                          tname = time,
                          idname = unit,
                          gname = first_treat,
                          data = data,
                          clustervars = c(cluster_by),
                          control_group = control)

    agg <- did::aggte(cs_res, type = aggregation)
  })["elapsed"]

  runtime <- as.numeric(runtime)

  frame <- data.frame(estimator = "cal_sa",
                      att_est = agg$overall.att,
                      std.err = agg$overall.se,
                      runtime = runtime
  )
  return(frame)
}


#' Compute all estimates
#'
#' @param data A dataframe containing the panel dataset
#' @param ests A vector containing all desired estimators. Defaults to all.
#' Allternatively spcify as a subset of: c("twfe", "borus", "csa", "chaise", "gardner", "sunab", "wool")
#' @param Y Name of the column containing the outcome variable
#' @param treat Name of the column containing the treatment indicator
#' @param time Name of the column containing the time period
#' @param unit Name of the column containing the unit identifier
#' @param group Name of the column containing the group identifier. In the context
#' of this package a group is a set of units receiving treatment and the same time
#' and experiencing the same treatment effects. You may specify several groups receiving
#' treatment at the same time.
#' @param control Define which control groups should be used in estimators which allow
#' to differentiate: "notyettreated" or "nevertreated".
#' @param aggregation Which treatment effect aggregation to compute: "simple", "group",
#' or "dynamic".
#' @param cluster_by Name of the column containing the variable to cluster by. By
#' default standard errors a clustered at the unit level.
#' @param iteration If specified will add a identifier to the outcome dataset, to
#' keep track in repeated estimation.
#' @export
get_all_ests <- function(data,
                         ests = "all",
                         Y = "Y",
                         treat = "treat",
                         time = "time",
                         unit = "unit",
                         group = "group",
                         control = "notyettreated",
                         aggregation = "simple",
                         cluster_by = NA,
                         iteration = NA){

  if (isTRUE(ests == "all")){
    ests <- c("twfe", "borus", "csa", "chaise", "gardner", "sunab", "wool")
  }


  if(is.na(cluster_by)){
    cluster_by <-"unit"
  }
  if(aggregation == "simple"){
    weights <-NA
    aggregation_wool <- "simple"
  } else if(aggregation == "group"){
    data$weights <- synthpanel::weights_group(data, unit,group, time, treat)
    aggregation_wool <- "group"
    weights <- "weights"
  }else if(aggregation == "dynamic"){
    data$weights <- synthpanel::weights_dyn(data, group, time, treat)
    weights <- "dynamic"
    aggregation_wool <- "event"
  }

  if(isTRUE(control == "nevertreated")){
    control_wool <- "never"
  } else{
    control_wool <- "notyet"
  }

  data <- data %>%
    dplyr::group_by(.data[[group]]) %>%
    dplyr::mutate(first_treat = ifelse(any(.data[[treat]] == 1), min(.data[[time]][.data[[treat]] == 1]), 0))

  # Create results dataframe




  res_list <- list()
  i <- 1
  if("twfe" %in% ests){

    twfe <- get_twfe(data, Y, treat, time, unit, cluster_by)
    res_list[[i]] <- twfe
    i <- i + 1
  }

  if("borus" %in% ests){
    borus <- get_borus(data, Y, "first_treat", time, unit, cluster_by, weights)
    res_list[[i]] <- borus
    i <- i + 1
  }

  if("csa" %in% ests){
    csa <- get_cs(data, Y, "first_treat", time, unit,
                  control,
                  aggregation,
                  cluster_by)
    res_list[[i]] <- csa
    i <- i + 1
  }

  if("chaise" %in% ests){
    chaise <- get_chaise(data, Y, treat, time, unit, group, weights)
    res_list[[i]] <- chaise
    i <- i + 1
  }

  if ("gardner" %in% ests){
    gardner <- get_gardner(data, Y, treat, time, unit, group, cluster_by, weights)
    res_list[[i]] <- gardner
    i <- i + 1
  }

  if ("sunab" %in% ests){
    sunab <- get_sunab(data, Y, "first_treat", time, unit, cluster_by, weights)
    res_list[[i]] <- sunab
    i <- i + 1
  }

  if ("wool" %in% ests){
    wool <- get_wool(data, Y, "first_treat", time, unit, group, cluster_by,
                     control_wool,
                     aggregation_wool)
    res_list[[i]] <- wool
    i <- i + 1
  }
  res <- do.call(rbind, res_list)
  res$n <- length(unique(data[[unit]]))
  res$num_periods <- length(unique(data[[time]]))
  if(!is.na(iteration)){
    res$iteration <- iteration
  }



  return(res)
}


