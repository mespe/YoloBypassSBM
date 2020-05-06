## code to prepare datasets in this package
library(fitdistrplus) # loads MASS which includes select so needs to be loaded before tidyverse
library(tidyverse)
library(readxl)
library(cfs.misc)
library(imputeTS)

# Simulation parameters ----------------------------------------------

simulation_parameters <- list(name = "demo",
                              sim_type = "deterministic",
                              reps = 1:1,
                              random_seed = 1,
                              water_years = 1997:2011,
                              chinook_runs = c("Fall", "LateFall", "Spring", "Winter"),
                              scenarios = "Exg",
                              # scenarios = c("Alt01", "Alt04b", "Alt04", "Alt05", "Alt06", "Exg"),
                              # ocean_year_probability describes probability of ocean survival being based on fork length relationship
                              ocean_year_probability = 1)
usethis::use_data(simulation_parameters, overwrite = TRUE)

# Length-weight parameters ----------------------------------------------
# Tiffan et al 2014

length_weight_parameters <- c("a" = 0.000004, "b" = 3.2252)
usethis::use_data(length_weight_parameters, overwrite = TRUE)

# Water year type ----------------------------------------------

water_year_type <- cfs.misc::get_water_year_type() %>%
  filter(WaterYear %in% 1997:2011) %>%
  left_join(tibble(SAC = c("W", "AN", "BN", "D", "C"),
                   WaterYearType = c("Wet", "Above normal", "Below Normal", "Dry", "Critical"))) %>%
  mutate(WaterYearType = factor(WaterYearType, levels = c("Wet", "Above normal", "Below Normal", "Dry", "Critical"))) %>%
  select(WaterYear, WaterYearType)
usethis::use_data(water_year_type, overwrite = TRUE)

# Ocean return parameters ----------------------------------------------

ocean_survival_parameters <- list("length" = readRDS("data-raw/OceanSurvivalParameters_Length.rds"),
                                  "intercept" = readRDS("data-raw/OceanSurvivalParameters_Intercept.rds"))
usethis::use_data(ocean_survival_parameters, overwrite = TRUE)

# Telemetry model parameters ----------------------------------------------

telemetry_parameters <- readRDS("data-raw/TelemetryModelParameters.rds")
usethis::use_data(telemetry_parameters, overwrite = TRUE)

# Rearing time parameters ----------------------------------------------

rt_delta <- readRDS("data-raw/RearingTimeParameters_Delta.rds")
rt_yolo <- readRDS("data-raw/RearingTimeParameters_Yolo.rds")
# thresh is the temperature threshold (ºC); fish stop rearing on first day with mean temp > thresh
rt_delta[["thresh"]] <- 22
rt_yolo[["thresh"]] <- 22
rearing_time_parameters <- list("Delta" = rt_delta, "Yolo" = rt_yolo)
usethis::use_data(rearing_time_parameters, overwrite = TRUE)

# Rearing survival parameters ----------------------------------------------

# default value of daily rearing survival (under deterministic simulation) and
# lower and upper limits of uniform distribution of daily survival values
rs_delta <- c("survival" = 0.99, "min" = 0.98, "max" = 1)
rs_yolo <- c("min" = 0, "max" = 1, "inflection" = 74, "steepness" = -6)
rearing_survival_parameters <- list("Delta" = rs_delta, "Yolo" = rs_yolo)
usethis::use_data(rearing_survival_parameters, overwrite = TRUE)

# Growth parameters ----------------------------------------------
# from Perry et al 2015
# b = allometric growth exponent
# d and g = shape parameters of the Ratkowsky model
# TL = lower temperature limit for growth
# TU = upper temperature limit for growth

growth_parameters <- c("b" = 0.338, "d" = 0.415, "g" = 0.315,
                       "TL" = 1.833, "TU" = 24.918, "thresh" = 22)
usethis::use_data(growth_parameters, overwrite = TRUE)

# Rearing abundance ----------------------------------------------

rearing_proportion_parameters <- c("min" = 0, "max" = 1,
                                   "inflection" = 100, "steepness" = 15)
usethis::use_data(rearing_proportion_parameters, overwrite = TRUE)

# Annual abundance ----------------------------------------------

annual_abundance <- read_csv("data-raw/AnnualAbundance.csv") %>%
  select(Run, WaterYear, Abundance) %>%
  mutate(Abundance = round(Abundance)) %>%
  spread(key = Run, value = Abundance)
usethis::use_data(annual_abundance, overwrite = TRUE)

# Flood duration ----------------------------------------------

flood_duration <- readRDS("data-raw/FloodDuration.rds")
usethis::use_data(flood_duration, overwrite = TRUE)

# Toe Drain temperature ----------------------------------------------

td <- readRDS("data-raw/ToeDrainTemp.rds")
toe_drain_temperature <- list(Date = td$date,
                              DOY = td$doy,
                              Value = td$temp)
usethis::use_data(toe_drain_temperature, overwrite = TRUE)

# Floodplain temperature difference ----------------------------------------------

ftd <- readRDS("data-raw/FloodplainTemperatureDifference.rds")

floodplain_temperature_difference <- list(DOY = ftd$DOY,
                                          Value = ftd$Diff)
usethis::use_data(floodplain_temperature_difference, overwrite = TRUE)

# Floodplain temperature ----------------------------------------------

temp_diff <- readRDS("data-raw/FloodplainTemperatureDifference.rds")

td <- readRDS("data-raw/ToeDrainTemp.rds") %>%
  left_join(temp_diff, by = c("doy" = "DOY")) %>%
  mutate(value = temp + Diff)

fpt <- readRDS("data-raw/FreeportTemp.rds") %>%
  mutate(DOY = lubridate::yday(date)) %>%
  left_join(temp_diff) %>%
  mutate(value = temp + Diff)

floodplain_temperature <- list("Yolo" = list("Date" = td[["date"]],
                                             "Value" = td[["value"]]),
                               "Delta" = list("Date" = fpt[["date"]],
                                              "Value" = fpt[["value"]]))
usethis::use_data(floodplain_temperature, overwrite = TRUE)

# Freeport temperature ----------------------------------------------

fpt <- readRDS("data-raw/FreeportTemp.rds")
freeport_temperature <- list(Date = fpt$date,
                             DOY = lubridate::yday(fpt$date),
                             Value = fpt$temp)
usethis::use_data(freeport_temperature, overwrite = TRUE)

# Entry timing ----------------------------------------------

klt_raw <- read_excel("data-raw/KnightsLandingCPUE.xlsx")

knights_landing_timing <- klt_raw %>%
  select(Date = DATE, paste("DAILY", c("FRC", "SRC", "WRC", "LFRC"), "%")) %>%
  gather(key = RunCode, value = Percent, `DAILY FRC %`:`DAILY LFRC %`) %>%
  left_join(tibble(RunCode = paste("DAILY", c("FRC", "SRC", "WRC", "LFRC"), "%"),
                   Run = c("Fall", "Spring", "Winter", "LateFall"))) %>%
  full_join(tibble(Date = seq(from = min(klt_raw$DATE, na.rm = TRUE),
                              to = max(klt_raw$DATE, na.rm = TRUE),
                              by = "days"))) %>%
  mutate(WaterYear = water_year(Date),
         Run = ifelse(is.na(Run), "Drop", Run),
         Prop = Percent/100) %>%
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

# fit lognormal distribution on every day with sufficient data
# best approach?
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

# impute MeanLog and SDLog parameters for days where data was insufficient to estimate
# here I used moving average to impute; with temperature I used linear interpolation
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

# Freeport flow and Proportion entrained at Fremont Weir ----------------------------------------------

flow_files <- list.files(path = "data-raw", pattern = "Flow", full.names = TRUE)
fremont_weir_proportion <- list()
freeport_flow <- list()
for (i in flow_files){
  scenario <-  gsub("data-raw/", "", gsub("Flow.csv", "", i))
  temp <- read_csv(i)
  fremont_weir_proportion[[scenario]] <- list(Date = temp$Date,
                                              Value = temp$PropFremont)
  freeport_flow[[scenario]] <- list(Date = temp$Date,
                                    Value = temp$Freeport)
}
usethis::use_data(fremont_weir_proportion, overwrite = TRUE)
usethis::use_data(freeport_flow, overwrite = TRUE)



