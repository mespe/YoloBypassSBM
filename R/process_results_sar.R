#' Process results for SAR difference
#'
#' Process results for difference in SAR between Yolo Bypass and Sacramento River routes where SAR is defined
#' as the juvenile abundance at Fremont Weir divided by adult ocean returners
#'
#' @md
#' @param sim_name       Name of simulation
#' @param type           Type of summary: `monthly`, `water_year_type` , or `both`. `monthly` and `water_year_type` return tibbles. `both` returns a list of two tibbles.
#'
#' @export
#'

process_results_sar = function (sim_name, type = c("monthly", "water_year_type", "both")) {
  type = match.arg(type)

  extract_data <- function(data_list){
    tibble(Rep = data_list[["Sac"]][["Rep"]],
           WaterYear = as.numeric(data_list[["Sac"]][["WaterYear"]]),
           Run = data_list[["Sac"]][["Run"]],
           Scenario = data_list[["Sac"]][["Scenario"]],
           CohortID = data_list[["Sac"]][["CohortID"]],
           KnightsDate = freeport_flow[["Exg"]][["Date"]][data_list[["Sac"]][["KnightsDay"]]],
           SacFremont = data_list[["Sac"]][["FremontAbun"]],
           YoloFremont = data_list[["Yolo"]][["FremontAbun"]],
           SacReturns = data_list[["Sac"]][["AdultReturns"]],
           YoloReturns = data_list[["Yolo"]][["AdultReturnsRear"]] + data_list[["Yolo"]][["AdultReturnsNon"]])
  }

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

  file_names <- list.files(path = "results", pattern = sim_name, full.names = TRUE)
  data_list <- lapply(file_names, function (x) extract_data(readRDS(x)))
  df_raw <- bind_rows(data_list)

  if (type %in% c("water_year_type", "both")){
    df_water_year_type <- df_raw %>%
      left_join(water_year_type) %>%
      group_by(Rep, WaterYearType, Run, Scenario) %>%
      summary_helper() %>%
      group_by(WaterYearType, Run, Scenario) %>%
      summarise(Lwr = quantile(Diff, probs = 0.025),
                Median = median(Diff),
                Upr = quantile(Diff, probs = 0.975))
  }

  if (type %in% c("monthly", "both")){
    df_monthly <- df_raw %>%
      mutate(KnightsMonth = month(KnightsDate)) %>%
      group_by(Rep, KnightsMonth, Run, Scenario) %>%
      summary_helper() %>%
      group_by(KnightsMonth, Run, Scenario) %>%
      summarise(Lwr = quantile(Diff, probs = 0.025),
                Median = median(Diff),
                Upr = quantile(Diff, probs = 0.975)) %>%
      mutate(KnightsMonthAbb = month.abb[KnightsMonth],
             KnightsMonthAbb = factor(KnightsMonthAbb, levels = month.abb[c(10:12,1:9)]))
  }

  if (type == "monthly") return(df_monthly)
  if (type == "water_year_type") return(df_water_year_type)
  if (type == "both") return(list("Monthly" = df_monthly, "WaterYearType" = df_water_year_type))

}

