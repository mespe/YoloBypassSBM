## Started out with good intentions of having good test coverage
## but writing tests slipped to a lower priority
## also, because many of the functions are just looking up data,
## the tests are not robust to changes in the underlying data

# dates ----------------------------------------------------------

expect_equal(get_water_year(c(1, 365, 730)), c(1997, 1998, 1999))
expect_equal(get_yday(c(1, 365, 730)), c(276, 274, 274))
expect_error(get_yday(0))
expect_equal(get_wy_yday(c(1, 365, 730)), c(2, 1, 1))
expect_error(get_wy_yday(5383))

# initialize cohorts ----------------------------------------------------------

expect_error(initialize_cohort("Summer"))
expect_equal(floor(initial_cohort_fork_length(100, "Winter")), 95)
expect_equal(floor(initial_cohort_fork_length(100, "Spring")), 46)
expect_equal(floor(initial_cohort_fork_length(1000, "LateFall")), 34)
expect_equal(floor(initial_cohort_fork_length(1200, "LateFall")), 128)

expect_error(initial_cohort_abundance(1990))
expect_equal(sum(initial_cohort_abundance(2000, "Fall")),
             annual_abundance[["Fall"]][annual_abundance[["WaterYear"]] == 2000])
expect_equal(sum(initial_cohort_abundance(2001, "LateFall")),
             annual_abundance[["LateFall"]][annual_abundance[["WaterYear"]] == 2001])
expect_equal(sum(initial_cohort_abundance(2002, "Winter")),
             annual_abundance[["Winter"]][annual_abundance[["WaterYear"]] == 2002])
expect_equal(sum(initial_cohort_abundance(2003, "Spring")),
             annual_abundance[["Spring"]][annual_abundance[["WaterYear"]] == 2003])

# entrainment ----------------------------------------------------------

expect_equal(sum(unlist(entrainment(100, 1000, "Exg"))), 1000)
expect_equal(sum(unlist(entrainment(100, 10000, "Exg"))), 10000)
expect_equal(round(entrainment(120, 10000, "Exg")$Yolo), 8735)
expect_equal(round(entrainment(120, 10000, "Exg")$Sac), 1265)

# rearing ----------------------------------------------------------

expect_equal(round(rearing_probability(40), 2), 0.97)
expect_equal(rearing_probability(80), 0.5)
expect_equal(round(rearing_probability(120), 2), 0.12)

expect_equal(rearing_status(80), 1)
expect_equal(rearing_status(81), 0)

expect_equal(round(mean(rearing_status(rep(40, 1e5), "stochastic")), 2), 0.97)
expect_equal(round(mean(rearing_status(rep(80, 1e5), "stochastic")), 2), 0.50)
expect_equal(round(mean(rearing_status(rep(120, 1e5), "stochastic")), 2), 0.12)

# survival ----------------------------------------------------------

# rearing
expect_equal(round(rearing_survival(1), 2), 0.97)
expect_equal(round(rearing_survival(10), 2), 0.74)
expect_equal(round(rearing_survival(20), 2), 0.54)

# passage
expect_equal(round(passage_survival(70, 60000, 100000, "Sac"), 3), 0.490)
expect_equal(round(passage_survival(70, 60000, 100000, "Yolo"), 3), 0.339)
expect_equal(round(passage_survival(150, 60000, 100000, "Sac"), 3), 0.694)
expect_equal(round(passage_survival(150, 60000, 100000, "Yolo"), 3), 0.547)
expect_equal(round(passage_survival(70, 30000, 100000, "Sac"), 3), 0.324)
expect_equal(round(passage_survival(70, 30000, 100000, "Yolo"), 3), 0.203)

# travel time ----------------------------------------------------------

expect_equal(round(travel_time(70, 60000, "Sac"), 1), 4.6)
expect_equal(round(travel_time(70, 60000, "Yolo"), 1), 6.6)

# inv_logit ----------------------------------------------------------

expect_equal(inv_logit(-Inf), 0)
expect_equal(inv_logit(Inf), 1)
expect_equal(inv_logit(0), 0.5)
expect_equal(round(inv_logit(-0.5), 2), 0.38)
expect_equal(round(inv_logit(0.5), 2), 0.62)
