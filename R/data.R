#' Annual abundance of outmigrants
#'
#' A dataset containing the annual outmigrant abundance that enters the model
#' in the Sacramento River at Knights Landing for each juvenile Chinook Salmon run
#' in each year of the 15-yr model period.
#'
#' @format A data frame with 15 rows and 5 columns:
#' \describe{
#'   \item{WaterYear}{defined as Oct 1st to Sept 30th}
#'   \item{Winter}{winter-run annual abundance}
#'   \item{Spring}{spring-run annual abundance}
#'   \item{Fall}{fall-run annual abundance}
#'   \item{LateFall}{late-fall-run annual abundance}
#' }
#'
"annual_abundance"

#' Cohort data template
#'
#' A nested list of values tracked as a cohort emigrates through the model system. List structure is arguably too complex because not
#' easily described here. Recommend running `str(cohort_data_template)` in the console to understand the structure.
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

#' Proportion of fish entrained onto the Yolo Bypass at Fremont Weir
#'
#' A dataset containing the predicted proportion of fish entering the Yolo Bypass
#' under six scenarios for each day of the 15-yr model period.
#'
#' @format A list comprised of 6 lists (one for each scenario) that contain 2 vectors of 5843 elements:
#' \describe{
#'   \item{Date}{calendar date}
#'   \item{Proportion}{predicted proportion of fish entering the Yolo Bypass}
#' }
#'
"fremont_weir_proportion"

#' Knights Landing fork length parameters
#'
#' A dataset containing the parameters used to draw random fork lengths for cohorts entering the model
#' in the Sacramento River at Knights Landing on each day of the 15-yr model period.
#'
#' @format A list comprised of 4 lists (one for each run: Fall, LateFall, Spring, Winter) that contain 3 vectors (Date, MeanLog, SDLog) of 5382 elements:
#' \describe{
#'   \item{Date}{calendar date}
#'   \item{MeanLog}{meanlog parameter of lognormal distribution}
#'   \item{SDLog}{sdlog parameter of lognormal distribution}
#' }
#'
"knights_landing_fl_params"

#' Knights Landing entry timing
#'
#' A dataset containing the proportion of the population that enters the model
#' in the Sacramento River at Knights Landing on each day of the 15-yr model period.
#'
#' @format A data frame with 21528 rows and 7 columns:
#' \describe{
#'   \item{Date}{calendar date}
#'   \item{WaterYear}{water year associated with Date; defined as Oct 1st to Sept 30th}
#'   \item{Winter}{winter-run daily entry proportion}
#'   \item{Spring}{spring-run daily entry proportion}
#'   \item{Fall}{fall-run daily entry proportion}
#'   \item{LateFall}{late-fall-run daily entry proportion}
#'   \item{ModelDay}{model day where 1 is 1996-10-02, 365 is 1997-10-01, 730 is 1998-10-01, etc.}
#' }
#'
"knights_landing_timing"
