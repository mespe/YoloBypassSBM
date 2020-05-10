#' Calculate SAR
#'
#' Calculate SAR as the juvenile abundance at Knights Landing divided by adult ocean returners
#'
#' @md
#' @param sim_name       Name of simulation, e.g., 'Baseline' would be used to match files named as Baseline-Reps-1-50.rds, Baseline-Reps-51-100.rds, etc.
#'
#' @export
#'

calculate_sar = function(sim_name) {
  require(dplyr)
  require(lubridate)

  extracted_list <- extract_results(sim_name,
                                    sac_columns = c("Rep", "WaterYear", "Run", "CohortID",
                                                    "KnightsAbun", "KnightsDay", "AdultReturns"),
                                    yolo_columns = c("Rep", "WaterYear", "Run", "CohortID",
                                                     "KnightsAbun", "KnightsDay",
                                                     "AdultReturns_YoloRear", "AdultReturns_YoloNoRear"))

  df_raw <- extracted_list[["Sac"]] %>%
    rename(SacReturns = AdultReturns) %>%
    full_join(extracted_list[["Yolo"]] %>%
                mutate(YoloReturns = AdultReturns_YoloRear + AdultReturns_YoloNoRear),
              by = c("Rep", "WaterYear", "Run", "CohortID", "KnightsAbun", "KnightsDay")) %>%
    mutate(KnightsDate = freeport_flow[["Date"]][KnightsDay],
           WaterYear = as.numeric(WaterYear)) %>%
    mutate(AdultReturns = SacReturns + YoloReturns,
           AdultReturns = ifelse(is.na(AdultReturns), 0, AdultReturns))

  df_wyt <- df_raw %>%
    left_join(water_year_type, by = "WaterYear") %>%
    group_by(Rep, WaterYearType, Run) %>%
    summarise(KnightsAbun = sum(KnightsAbun),
              AdultReturns = sum(AdultReturns)) %>%
    mutate(SAR = AdultReturns/KnightsAbun) %>%
    group_by(WaterYearType, Run) %>%
    summarise_quantiles("SAR")

  df_month <- df_raw %>%
    mutate(KnightsMonth = month(KnightsDate)) %>%
    group_by(Rep, KnightsMonth, Run) %>%
    summarise(KnightsAbun = sum(KnightsAbun),
              AdultReturns = sum(AdultReturns)) %>%
    mutate(SAR = AdultReturns/KnightsAbun) %>%
    group_by(KnightsMonth, Run) %>%
    summarise_quantiles("SAR") %>%
    mutate(KnightsMonthAbb = factor(month.abb[KnightsMonth],
                                    levels = month.abb[c(10:12,1:9)]))

  list("Month" = df_month, "WaterYearType" = df_wyt)

}

