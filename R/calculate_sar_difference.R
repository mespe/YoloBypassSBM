#' Calculate SAR difference
#'
#' Calculate difference in SAR between Yolo Bypass and Sacramento River routes where SAR is defined
#' as the juvenile abundance at Fremont Weir divided by adult ocean returners
#'
#' @md
#' @param sim_name       Name of simulation, e.g., 'Baseline' would be used to match files named as Baseline-Reps-1-50.rds, Baseline-Reps-51-100.rds, etc.
#'
#' @export
#'

calculate_sar_difference = function(sim_name) {
  
  summary_helper <- function(gdf){
    gdf %>% # gdf = grouped data frame
      summarise(SacFremont = sum(SacFremont),
                YoloFremont = sum(YoloFremont),
                SacReturns = sum(SacReturns),
                YoloReturns = sum(YoloReturns)) %>%
      # if fish aren't using both routes, then no comparison to make
      filter(YoloFremont > 0 & SacFremont > 0) %>%
      mutate(SacSAR = SacReturns/SacFremont,
             YoloSAR = YoloReturns/YoloFremont,
             Diff = YoloSAR - SacSAR)
  }

  extracted_list <- extract_results(sim_name,
                                    sac_columns = c("Rep", "WaterYear", "Run", "CohortID",
                                                    "KnightsAbun", "KnightsDay", "FremontAbun", "AdultReturns"),
                                    yolo_columns = c("Rep", "WaterYear", "Run", "CohortID",
                                                     "KnightsAbun", "KnightsDay", "FremontAbun",
                                                     "AdultReturns_YoloRear", "AdultReturns_YoloNoRear"))

  df_raw <- extracted_list[["Sac"]] %>%
    rename(SacFremont = FremontAbun, SacReturns = AdultReturns) %>%
    full_join(extracted_list[["Yolo"]] %>%
                mutate(YoloReturns = AdultReturns_YoloRear + AdultReturns_YoloNoRear) %>%
                rename(YoloFremont = FremontAbun),
              by = c("Rep", "WaterYear", "Run", "CohortID", "KnightsAbun", "KnightsDay")) %>%
    mutate(KnightsDate = freeport_flow[["Date"]][KnightsDay],
           WaterYear = as.numeric(WaterYear)) %>%
    mutate_at(c("SacFremont", "SacReturns", "YoloFremont", "YoloReturns"), ~ifelse(is.na(.), 0, .))

  df_wyt <- df_raw %>%
    left_join(water_year_type, by = "WaterYear") %>%
    group_by(Rep, WaterYearType, Run) %>%
    summary_helper() %>%
    group_by(WaterYearType, Run) %>%
    summarise_quantiles("Diff")

  df_month <- df_raw %>%
    mutate(KnightsMonth = month(KnightsDate)) %>%
    group_by(Rep, KnightsMonth, Run) %>%
    summary_helper() %>%
    group_by(KnightsMonth, Run) %>%
    summarise_quantiles("Diff") %>%
    mutate(KnightsMonthAbb = factor(month.abb[KnightsMonth],
                                    levels = month.abb[c(10:12,1:9)]))

  list("Month" = df_month, "WaterYearType" = df_wyt)

}

