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
  scenaro <- match.arg(scenario)
  sim_type <- match.arg(sim_type)

  wy_all <- annual_abundance[["WaterYear"]]
  if (!(water_year %in% wy_all))
    stop(paste("water_year must be between", min(wy_all), "and", max(wy_all)))

  if (length(water_year) > 1)
    stop("water_year must have length = 1")

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

  sac <- sac[sac[["FremontAbun"]] > 0,]

  sac_passage_list <- passage(sac[["KnightsDay"]], sac[["FremontAbun"]], sac[["KnightsFL"]],
                              route = "Sac", scenario, sim_type)

  sac[["ChippsAbun"]] <- sac_passage_list[["ChippsAbun"]]
  sac[["ChippsDay"]] <- sac_passage_list[["ChippsDay"]]

  sac[["AdultReturns"]] <- ocean_survival(sac[["KnightsWW"]],
                                          sac[["ChippsAbun"]],
                                          sim_type)

  # Yolo route
  yolo <- data.frame(KnightsDay = model_days,
                     KnightsAbun = knights_abun,
                     KnightsFL = knights_fl,
                     KnightsWW = knights_ww,
                     stringsAsFactors = FALSE)

  yolo[["FremontAbun"]] <- entrain_list[["Yolo"]]

  yolo <- yolo[yolo[["FremontAbun"]] > 0,]

  fd <- flood_duration[[scenario]][["Value"]][yolo[["KnightsDay"]]]
  yolo[["RearingTimeAdj"]] <- rearing_time_adj(rearing_time_max(fd, sim_type),
                                               yolo[["KnightsWW"]],
                                               yolo[["KnightsDay"]])

  yolo[["RearingAbun"]] <- yolo[["FremontAbun"]] * rearing_survival(yolo[["RearingTimeAdj"]],
                                                                    sim_type)

  yolo[["RearingWW"]] <- rearing_growth(yolo[["KnightsWW"]],
                                        yolo[["KnightsDay"]],
                                        yolo[["RearingTimeAdj"]])

  yolo_passage_list <- passage(yolo[["KnightsDay"]] + yolo[["RearingTimeAdj"]],
                               yolo[["RearingAbun"]], weight_length(yolo[["RearingWW"]]),
                               route = "Yolo", scenario, sim_type)

  yolo[["ChippsAbun"]] <- yolo_passage_list[["ChippsAbun"]]
  yolo[["ChippsDay"]] <- yolo_passage_list[["ChippsDay"]]
  yolo[["AdultReturns"]] <- ocean_survival(yolo[["RearingWW"]],
                                           yolo[["ChippsAbun"]],
                                           sim_type)

  return(list("Sac" = sac, "Yolo" = yolo))
}

