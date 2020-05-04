#' Run one replicate of the model
#'
#' Run one replicate of the model; a replicate is a water_year/chinook_run/scenario/sim_type combination
#'
#' @md
#' @param water_year    Water year (1997-2011)
#' @param chinook_run   Run timing classification: Fall, LateFall, Winter, Spring
#' @param sim_type      Simulation type: deterministic or stochastic
#'
#' @export
#'
#'

run_one_rep <- function(water_year, chinook_run = c("Fall", "LateFall", "Spring", "Winter"),
                        scenario = c("Exg", "Alt01", "Alt04b", "Alt04", "Alt05", "Alt06"),
                        sim_type = c("deterministic", "stochastic")){

  chinook_run <- match.arg(chinook_run)
  scenario <- match.arg(scenario)
  sim_type <- match.arg(sim_type)

  wy_all <- annual_abundance[["WaterYear"]]
  if (!(water_year %in% wy_all))
    stop(paste("water_year must be between", min(wy_all), "and", max(wy_all)))

  if (length(water_year) > 1)
    stop("water_year must have length = 1")

  ocean_year_type <- ifelse(runif(1) < simulation_parameters[["ocean_year_probability"]],
                            "length", "intercept")

  model_days <- get_wy_model_days(water_year)
  knights_abun <- initial_cohort_abundance(water_year, chinook_run, sim_type)
  knights_fl <- initial_cohort_fork_length(model_days, chinook_run, sim_type)
  knights_ww <- length_weight(knights_fl)

  # no travel time, mortality, or growth from Knights Landing to Fremont Weir
  entrain_list <- entrainment(model_days, knights_abun, scenario, sim_type)

  # Sacramento route
  sac <- data.frame(KnightsDay = model_days,
                    KnightsAbun = knights_abun,
                    KnightsFL = knights_fl,
                    KnightsWW = knights_ww,
                    stringsAsFactors = FALSE)

  sac[["FremontAbun"]] <- entrain_list[["Sac"]]
  # assign CohortID to include even days with zero entry or entrainment
  # keeps CohortID synced between Sac and Yolo
  sac[["CohortID"]] <- 1:nrow(sac)
  sac <- sac[sac[["FremontAbun"]] > 0,]

  if (nrow(sac) > 0){
    sac_passage_list <- passage(sac[["KnightsDay"]], sac[["FremontAbun"]], sac[["KnightsFL"]],
                                route = "Sac", scenario, sim_type)

    sac[["ChippsAbun"]] <- sac_passage_list[["ChippsAbun"]]
    sac[["ChippsDay"]] <- sac[["KnightsDay"]] + sac_passage_list[["PassageTime"]]
    sac[["ChippsFL"]] <- weight_length(passage_growth(sac[["KnightsWW"]],
                                                      sac[["KnightsDay"]],
                                                      sac_passage_list[["PassageTime"]],
                                                      route = "Sac"))

    sac[["AdultReturns"]] <- ocean_survival(sac[["ChippsAbun"]],
                                            sac[["ChippsFL"]],
                                            ocean_year_type,
                                            sim_type)
  }else{
    sac = data.frame()
  }

  # Yolo route
  yolo <- data.frame(KnightsDay = model_days,
                     KnightsAbun = knights_abun,
                     KnightsFL = knights_fl,
                     KnightsWW = knights_ww,
                     stringsAsFactors = FALSE)

  yolo[["FremontAbun"]] <- entrain_list[["Yolo"]]
  # assign CohortID to include even days with zero entry or entrainment
  # keeps Cohort ID synced between Sac and Yolo
  yolo[["CohortID"]] <- 1:nrow(yolo)
  yolo <- yolo[yolo[["FremontAbun"]] > 0,]

  if (nrow(yolo) > 0){
    fd <- flood_duration[[scenario]][["Value"]][yolo[["KnightsDay"]]]
    yolo[["RearingTimeAdj"]] <- rearing_time_adj(rearing_time_max(fd, sim_type),
                                                 yolo[["KnightsDay"]])

    yolo[["RearingAbun"]] <- rearing_abundance(yolo[["FremontAbun"]],
                                               yolo[["KnightsFL"]],
                                               sim_type)

    yolo[["PostRearingAbun"]]  <-  yolo[["RearingAbun"]] * rearing_survival(yolo[["RearingTimeAdj"]], sim_type)

    yolo[["PostRearingWW"]] <- rearing_growth(wet_weight = yolo[["KnightsWW"]],
                                              model_day = yolo[["KnightsDay"]],
                                              duration = yolo[["RearingTimeAdj"]])

    # yolo passage for rearing individuals
    yolo_passage_rear <- passage(yolo[["KnightsDay"]] + yolo[["RearingTimeAdj"]],
                                 yolo[["PostRearingAbun"]],
                                 weight_length(yolo[["PostRearingWW"]]),
                                 route = "Yolo", scenario, sim_type)

    # yolo passage for non-rearing individuals
    yolo_passage_non <- passage(yolo[["KnightsDay"]],
                                yolo[["FremontAbun"]] - yolo[["RearingAbun"]],
                                yolo[["KnightsFL"]],
                                route = "Yolo", scenario, sim_type)

    yolo[["ChippsAbunRear"]] <- yolo_passage_rear[["ChippsAbun"]]
    yolo[["ChippsAbunNon"]] <- yolo_passage_non[["ChippsAbun"]]

    yolo[["ChippsDayRear"]] <- yolo[["KnightsDay"]] + yolo[["RearingTimeAdj"]] + yolo_passage_rear[["PassageTime"]]
    yolo[["ChippsDayNon"]] <- yolo[["KnightsDay"]] + yolo_passage_non[["PassageTime"]]

    yolo[["ChippsFLRear"]] <- weight_length(passage_growth(wet_weight = yolo[["PostRearingWW"]],
                                                           model_day = yolo[["KnightsDay"]]+ yolo[["RearingTimeAdj"]],
                                                           duration = yolo_passage_rear[["PassageTime"]],
                                                           route = "Yolo"))
    yolo[["ChippsFLNon"]] <- weight_length(passage_growth(wet_weight = yolo[["KnightsWW"]],
                                                          model_day = yolo[["KnightsDay"]],
                                                          duration = yolo_passage_non[["PassageTime"]],
                                                          route = "Yolo"))

    yolo[["AdultReturnsRear"]] <- ocean_survival(yolo[["ChippsAbunRear"]],
                                                 yolo[["ChippsFLRear"]],
                                                 ocean_year_type,
                                                 sim_type)
    yolo[["AdultReturnsNon"]] <- ocean_survival(yolo[["ChippsAbunNon"]],
                                                yolo[["ChippsFLNon"]],
                                                ocean_year_type,
                                                sim_type)
  }else{
    yolo = data.frame()
  }

  return(list("Sac" = sac, "Yolo" = yolo))
}

