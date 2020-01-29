#' Cohort data template
#'
#' A nested list of values tracked as a cohort emigrates through the model system.
#'
#' @format Abbreviations and names used in the nested list:
#' \describe{
#'   \item{Run}{Run timing classification: Fall, LateFall, Winter, Spring}
#'   \item{ModelDay}{Model day where 1 is 1996-10-02, 365 is 1997-10-01, 730 is 1998-10-01, etc.}
#'   \item{Knights}{Knights Landing is where cohorts enter the model}
#'   \item{Yolo}{Yolo Bypass route}
#'   \item{Sac}{Sacramento River route}
#'   \item{Fremont}{Fremont Weir is where cohorts are routed into the Yolo Bypass route}
#'   \item{Rio}{Rio Vista is where cohorts from the Yolo Bypass rejoin the Sacramento River}
#'   \item{Chipps}{Chipps Island is where cohorts enter the estuary}
#'   \item{Ocean}{Ocean is used to denote cohort abundance after returning from the ocean}
#'   \item{Abun}{Cohort abundance}
#'   \item{FL}{Cohort fork length}
#'   \item{Time}{Time (days) through a reach. From the Fremont Weir to Rio Vista, time is separated into passage and residence.}
#'   \item{Growth}{Total growth (mm) while emigrating through a reach. From the Fremont Weir to Rio Vista, growth is separated into passage and residence.}
#'   \item{Surv}{Survival through a reach. From the Fremont Weir to Rio Vista, survival is separated into passage and residence.}
#' }
#'
"cohort_data_template"
