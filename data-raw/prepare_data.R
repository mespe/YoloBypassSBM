## code to prepare datasets in this package
library(fitdistrplus) # loads MASS which includes select so needs to be loaded before tidyverse
library(tidyverse)
library(readxl)
library(cfs.misc)
library(imputeTS)

# Cohort data template ----------------------------------------------

route_list <- list(Abun = list(Fremont = NA_real_,
                               Rio = NA_real_,
                               Chipps = NA_real_,
                               Ocean = NA_real_),
                   FL = list(Fremont = NA_real_,
                             Rio = NA_real_,
                             Chipps = NA_real_),
                   Time = list(Knights = NA_real_,
                               Fremont = c("Passage" = NA_real_, "Residence" = NA_real_),
                               Rio = NA_real_),
                   Growth = list(Knights = NA_real_,
                                 Fremont = c("Passage" = NA_real_, "Residence" = NA_real_),
                                 Rio = NA_real_),
                   Surv = list(Knights = NA_real_,
                               Fremont = c("Passage" = NA_real_, "Residence" = NA_real_),
                               Rio = NA_real_,
                               Chipps = NA_real_))

cohort_data_template <- list(Run = NA_character_,
                             ModelDay = c("Knights" = NA_real_,
                                          "Fremont" = NA_real_,
                                          "Rio" = NA_real_,
                                          "Chipps" = NA_real_),
                             Knights = c("Abun" = NA_real_, "FL" = NA_real_),
                             Yolo = route_list,
                             Sac = route_list)
usethis::use_data(cohort_data_template, overwrite = TRUE)

# Telemetry model parameters ----------------------------------------------

telemetry_parameters <- readRDS("data-raw/TelemetryModelParameters.rds")
usethis::use_data(telemetry_parameters, overwrite = TRUE)

# Annual abundance ----------------------------------------------

annual_abundance <- read_csv("data-raw/AnnualAbundance.csv") %>%
  select(Run, WaterYear, Abundance) %>%
  mutate(Abundance = round(Abundance)) %>%
  spread(key = Run, value = Abundance)
usethis::use_data(annual_abundance, overwrite = TRUE)

# Entry timing ----------------------------------------------

klt_raw <- read_excel("data-raw/KnightsLandingCPUE.xlsx")

knights_landing_timing <- klt_raw %>%
  select(Date = DATE, paste("DAILY", c("FRC", "SRC", "WRC", "LFRC"), "%")) %>%
  gather(key = RunCode, value = Prop, `DAILY FRC %`:`DAILY LFRC %`) %>%
  left_join(tibble(RunCode = paste("DAILY", c("FRC", "SRC", "WRC", "LFRC"), "%"),
                   Run = c("Fall", "Spring", "Winter", "LateFall"))) %>%
  full_join(tibble(Date = seq(from = min(klt_raw$DATE, na.rm = TRUE),
                              to = max(klt_raw$DATE, na.rm = TRUE),
                              by = "days"))) %>%
  mutate(WaterYear = water_year(Date),
         Run = ifelse(is.na(Run), "Drop", Run)) %>%
  select(Run, Date, WaterYear, Prop) %>%
  spread(key = Run, value = Prop) %>%
  arrange(Date) %>%
  select(-Drop) %>%
  replace_na(list(Fall = 0, Spring = 0, Winter = 0, LateFall = 0)) %>%
  mutate(ModelDay = 1:length(Fall))
usethis::use_data(knights_landing_timing, overwrite = TRUE)

# Fork length ----------------------------------------------

tabs <- excel_sheets("data-raw/KLRST_99_TO_2012_FL_WW_WEEK_TRAPID_TMH.xlsx")
tabs <- tabs[tabs != "KEY"]
fl_list_99_12 <- lapply(tabs, read_excel, path = "data-raw/KLRST_99_TO_2012_FL_WW_WEEK_TRAPID_TMH.xlsx")
fl_df_99_12 <- bind_rows(fl_list_99_12) %>%
  filter(SPECIES == "CS" & RACE %in% 1:4) %>%
  select(DATE, RACE, FL, WW) %>%
  arrange(DATE, RACE)

fl_list_97_99 <- list()
for (i in c("97CSFL", "KLFL98", "KLFL99")){
  fl_list_97_99[[i]] <- read_excel(file.path("data-raw", paste0(i, ".xlsx"))) %>%
    filter(RACE %in% 1:4) %>%
    select(DATE, FL, RACE)
}
fl_df_97_99 <- bind_rows(fl_list_97_99)

fl_df <- bind_rows(fl_df_97_99, fl_df_99_12) %>%
  arrange(DATE) %>%
  mutate(WaterYear = water_year(DATE)) %>%
  rename(CODE = RACE) %>%
  left_join(tibble("CODE" = 1:4, "RACE" = c("Fall", "Spring", "Winter", "LateFall"))) %>%
  filter(FL > 0 & FL < 300) %>%  #  drop very large fish; presumably yearlings or data entry errors
  select(WaterYear, Date = DATE, ForkLength = FL, Run = RACE) %>%
  mutate(Date = as.character(Date))

fl_dist_param_list <- list()
ctr <- 1
for (i in unique(fl_df$Run)){
  fl_df_run <- filter(fl_df, Run == i)
  for (j in unique(fl_df_run$Date)){
    fl_df_run_date <- filter(fl_df_run, Date == j)
    if (nrow(fl_df_run_date) > 3 && length(unique(fl_df_run_date$ForkLength)) > 1){
      fit_lnorm <- fitdist(fl_df_run_date$ForkLength, "lnorm")
      fl_dist_param_list[[ctr]] <- tibble(Run = i,
                                          Date = j,
                                          MeanLog = fit_lnorm$estimate[["meanlog"]],
                                          SDLog = fit_lnorm$estimate[["sdlog"]])
      ctr <- ctr + 1
    }
  }
}
fl_dist_param <- bind_rows(fl_dist_param_list)

timing_fl <- knights_landing_timing %>%
  gather(key = Run, value = Prop, Fall:Winter) %>%
  mutate(Date = as.character(Date)) %>%
  left_join(fl_dist_param) %>%
  arrange(Run, Date) %>%
  group_by(Run) %>%
  mutate(ImputeMeanLog = na_ma(MeanLog),
         ImputeSDLog = na_ma(SDLog),
         Date = as.Date(Date))

knights_landing_fl_params <- list()
for (i in unique(timing_fl$Run)){
  temp <- filter(timing_fl, Run == i) %>%
    arrange(Date)
  knights_landing_fl_params[[i]] <- list(Date = temp$Date,
                                         MeanLog = temp$ImputeMeanLog,
                                         SDLog = temp$ImputeSDLog)
}
usethis::use_data(knights_landing_fl_params, overwrite = TRUE)

# Proportion entrained at Fremont Weir

flow_files <- list.files(path = "data-raw", pattern = "Flow", full.names = TRUE)
fremont_weir_proportion <- list()
for (i in flow_files){
  scenario <-  gsub("data-raw/", "", gsub("Flow.csv", "", i))
  temp <- read_csv(i)
  fremont_weir_proportion[[scenario]] <- list(Date = temp$Date,
                                              Proportion = temp$PropFremont)
}
usethis::use_data(fremont_weir_proportion, overwrite = TRUE)


# Suitable habitat ----------------------------------------------

# need to decide how these data will be used in package (e.g., running average) before adding to package


