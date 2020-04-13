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

# survival ----------------------------------------------------------

# rearing
expect_equal(round(rearing_survival(1), 2), 0.97)
expect_equal(round(rearing_survival(10), 2), 0.74)
expect_equal(round(rearing_survival(20), 2), 0.54)

# ocean
expect_equal(round(ocean_survival(30), 4), 0.0011)
expect_equal(round(ocean_survival(70), 4), 0.0028)
expect_equal(round(ocean_survival(100), 4), 0.0057)

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

# entrainment ----------------------------------------------------------

ent_exg_80 <- entrainment(80, 1000, "Exg")
expect_equal(ent_exg_80$Yolo + ent_exg_80$Sac, 1000)
ent_alt6_80 <- entrainment(80, 1000, "Alt06")
expect_equal(ent_alt6_80$Yolo + ent_alt6_80$Sac, 1000)

# inv_logit ----------------------------------------------------------

expect_equal(inv_logit(-Inf), 0)
expect_equal(inv_logit(Inf), 1)
expect_equal(inv_logit(0), 0.5)
expect_equal(round(inv_logit(-0.5), 2), 0.38)
expect_equal(round(inv_logit(0.5), 2), 0.62)
