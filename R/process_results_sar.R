#' Process results for SAR difference
#'
#' Process results for difference in SAR between Yolo Bypass and Sacramento River routes where SAR is defined
#' as the juvenile abundance at Fremont Weir divided by adult ocean returners
#'
#' @md
#' @param sim_name       Name of simulation
#' @param type           Type of summary: monthly, yearly, or both. Monthly and yearly return tibbles. Both returns a list of two tibbles.
#'
#' @export
#'

process_results_sar = function (sim_name, type = c("monthly", "yearly", "both")) {
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
      mutate(SacSAR = SacReturns/SacFremont,
             SacSAR = ifelse(SacFremont == 0, 0, SacSAR),
             YoloSAR = YoloReturns/YoloFremont,
             YoloSAR = ifelse(YoloFremont == 0, 0, YoloSAR),
             Diff = YoloSAR - SacSAR)
  }

  file_names <- list.files(path = "results", pattern = sim_name, full.names = TRUE)
  data_list <- lapply(file_names, function (x) extract_data(readRDS(x)))
  df_raw <- bind_rows(data_list)

  if (type %in% c("yearly", "both")){
    df_yearly <- df_raw %>%
      group_by(Rep, WaterYear, Run, Scenario) %>%
      summary_helper() %>%
      group_by(WaterYear, Run, Scenario) %>%
      summarise(Lwr = quantile(Diff, probs = 0.025),
                Median = median(Diff),
                Upr = quantile(Diff, probs = 0.975))
  }

  if (type %in% c("monthly", "both")){
    df_monthly <- df_raw %>%
      mutate(KnightsMonth = month(KnightsDate)) %>%
      group_by(Rep, FremontMonth, Run, Scenario) %>%
      summary_helper() %>%
      group_by(FremontMonth, Run, Scenario) %>%
      summarise(Lwr = quantile(Diff, probs = 0.025),
                Median = median(Diff),
                Upr = quantile(Diff, probs = 0.975)) %>%
      mutate(FremontMonthAbb = month.abb[FremontMonth],
             FremontMonthAbb = factor(FremontMonthAbb, levels = month.abb[c(10:12,1:9)]))
  }

  if (type == "monthly") return(df_monthly)
  if (type == "yearly") return(df_yearly)
  if (type == "both") return(list("Monthly" = df_monthly, "Yearly" = df_yearly))

}

