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

## probably would have take a different approach to storing output data if I was planning to incorporate Delta rearing from beginning
## approach that I'm using in Yolo section is very prone to typos
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
  # assign CohortID to also include days with zero entry or entrainment
  # keeps CohortID synced between Sac and Yolo
  sac[["CohortID"]] <- 1:nrow(sac)
  sac <- sac[sac[["FremontAbun"]] > 0,]

  if (nrow(sac) > 0){

    # Sacramento River passage
    sac_passage_list <- passage(model_day = sac[["KnightsDay"]],
                                abundance = sac[["FremontAbun"]],
                                fork_length = sac[["KnightsFL"]],
                                route = "Sac", scenario, sim_type)

    sac[["DeltaDay"]] <- sac[["KnightsDay"]] + sac_passage_list[["PassageTime"]]
    sac[["DeltaAbun"]] <- sac_passage_list[["Abundance"]]

    # Delta rearing

    sac[["DeltaRearingTime"]] <- rearing_time_delta(model_day = sac[["DeltaDay"]],
                                                    passage_time = sac_passage_list[["PassageTime"]],
                                                    fork_length = sac[["KnightsFL"]],
                                                    scenario, sim_type)

    sac[["ChippsAbun"]] <- rearing_survival(model_day = sac[["DeltaDay"]],
                                            abundance = sac[["DeltaAbun"]],
                                            duration = sac[["DeltaRearingTime"]],
                                            location = "Delta",
                                            scenario, sim_type)

    sac[["ChippsFL"]] <- weight_length(rearing_growth(wet_weight = sac[["KnightsWW"]],
                                                      model_day = sac[["DeltaDay"]],
                                                      duration = sac[["DeltaRearingTime"]],
                                                      location = "Delta"))

    # Ocean

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
  # assign CohortID to also include days with zero entry or entrainment
  # keeps Cohort ID synced between Sac and Yolo
  yolo[["CohortID"]] <- 1:nrow(yolo)
  yolo <- yolo[yolo[["FremontAbun"]] > 0,]

  if (nrow(yolo) > 0){

    # Yolo Bypass rearing

    # YoloRearingAbun is proportion of cohorts that "decide" to rear on the Yolo Bypass multipled by abundance
    yolo[["YoloRearingAbun"]] <- rearing_abundance(yolo[["FremontAbun"]], yolo[["KnightsFL"]], sim_type)
    yolo[["YoloRearingTime"]] <- rearing_time_yolo(yolo[["KnightsDay"]], scenario, sim_type)

    yolo[["PostYoloRearingAbun"]]  <- rearing_survival(model_day = yolo[["KnightsDay"]],
                                                       abundance = yolo[["YoloRearingAbun"]],
                                                       duration = yolo[["YoloRearingTime"]],
                                                       location = "Yolo",
                                                       scenario, sim_type)

    yolo[["PostYoloRearingFL"]] <- weight_length(rearing_growth(wet_weight = yolo[["KnightsWW"]],
                                                                model_day = yolo[["KnightsDay"]],
                                                                duration = yolo[["YoloRearingTime"]],
                                                                location = "Yolo"))

    # Yolo Bypass passage

    # yolo passage for rearing individuals
    yolo_passage_rear <- passage(model_day = yolo[["KnightsDay"]] + yolo[["YoloRearingTime"]],
                                 abundance = yolo[["PostYoloRearingAbun"]],
                                 fork_length = yolo[["PostYoloRearingFL"]],
                                 route = "Yolo", scenario, sim_type)

    # yolo passage for non-rearing individuals
    yolo_passage_no_rear <- passage(model_day = yolo[["KnightsDay"]],
                                    abundance = yolo[["FremontAbun"]] - yolo[["YoloRearingAbun"]],
                                    fork_length = yolo[["KnightsFL"]],
                                    route = "Yolo", scenario, sim_type)

    yolo[["DeltaAbun_YoloRear"]] <- yolo_passage_rear[["Abundance"]]
    yolo[["DeltaAbun_YoloNoRear"]] <- yolo_passage_no_rear[["Abundance"]]

    yolo[["DeltaDay_YoloRear"]] <- yolo[["KnightsDay"]] + yolo[["YoloRearingTime"]] + yolo_passage_rear[["PassageTime"]]
    yolo[["DeltaDay_YoloNoRear"]] <- yolo[["KnightsDay"]] + yolo_passage_no_rear[["PassageTime"]]

    # Delta rearing

    yolo[["DeltaRearingTime_YoloRear"]] <- rearing_time_delta(model_day = yolo[["DeltaDay_YoloRear"]],
                                                              passage_time = yolo_passage_rear[["PassageTime"]],
                                                              fork_length = yolo[["PostYoloRearingFL"]],
                                                              scenario, sim_type)
    yolo[["DeltaRearingTime_YoloNoRear"]] <- rearing_time_delta(model_day = yolo[["DeltaDay_YoloNoRear"]],
                                                                passage_time = yolo_passage_no_rear[["PassageTime"]],
                                                                fork_length = yolo[["KnightsFL"]],
                                                                scenario, sim_type)

    yolo[["ChippsAbun_YoloRear"]] <- rearing_survival(model_day = yolo[["DeltaDay_YoloRear"]],
                                                      abundance = yolo[["DeltaAbun_YoloRear"]],
                                                      duration = yolo[["DeltaRearingTime_YoloRear"]],
                                                      location = "Delta",
                                                      scenario, sim_type)
    yolo[["ChippsAbun_YoloNoRear"]] <- rearing_survival(model_day = yolo[["DeltaDay_YoloNoRear"]],
                                                        abundance = yolo[["DeltaAbun_YoloNoRear"]],
                                                        duration = yolo[["DeltaRearingTime_YoloNoRear"]],
                                                        location = "Delta",
                                                        scenario, sim_type)

    yolo[["ChippsFL_YoloRear"]] <- weight_length(rearing_growth(wet_weight = length_weight(yolo[["PostYoloRearingFL"]]),
                                                                model_day = yolo[["DeltaDay_YoloRear"]],
                                                                duration = yolo[["DeltaRearingTime_YoloRear"]],
                                                                location = "Delta"))
    yolo[["ChippsFL_YoloNoRear"]] <- weight_length(rearing_growth(wet_weight = yolo[["KnightsWW"]],
                                                                  model_day = yolo[["DeltaDay_YoloNoRear"]],
                                                                  duration = yolo[["DeltaRearingTime_YoloNoRear"]],
                                                                  location = "Delta"))

    # Ocean

    yolo[["AdultReturns_YoloRear"]] <- ocean_survival(yolo[["ChippsAbun_YoloRear"]],
                                                      yolo[["ChippsFL_YoloRear"]],
                                                      ocean_year_type,
                                                      sim_type)
    yolo[["AdultReturns_YoloNoRear"]] <- ocean_survival(yolo[["ChippsAbun_YoloNoRear"]],
                                                        yolo[["ChippsFL_YoloNoRear"]],
                                                        ocean_year_type,
                                                        sim_type)
  }else{
    yolo = data.frame()
  }

  return(list("Sac" = sac, "Yolo" = yolo))
}

