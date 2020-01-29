
# dates ----------------------------------------------------------

expect_equal(get_water_year(c(1, 365, 730)), c(1997, 1998, 1999))
expect_equal(get_yday(c(1, 365, 730)), c(276, 274, 274))
expect_error(get_yday(0))
expect_equal(get_wy_yday(c(1, 365, 730)), c(2, 1, 1))
expect_error(get_wy_yday(5383))

# initialize cohorts ----------------------------------------------------------

expect_error(initialize_cohort("Summer"))
set.seed(101)
expect_equal(floor(initial_cohort_fork_length("Winter", 100)), 89)
set.seed(102)
expect_equal(floor(initial_cohort_fork_length("Spring", 100)), 46)
set.seed(103)
expect_equal(floor(initial_cohort_fork_length("LateFall", 1000)), 33)
set.seed(104)
expect_equal(floor(initial_cohort_fork_length("LateFall", 1200)), 125)

expect_error(initial_cohort_abundance("Fall", 1990))
expect_equal(sum(initial_cohort_abundance("Fall", 2000)),
             annual_abundance[["Fall"]][annual_abundance[["WaterYear"]] == 2000])
expect_equal(sum(initial_cohort_abundance("LateFall", 2001)),
             annual_abundance[["LateFall"]][annual_abundance[["WaterYear"]] == 2001])
expect_equal(sum(initial_cohort_abundance("Winter", 2002)),
             annual_abundance[["Winter"]][annual_abundance[["WaterYear"]] == 2002])
expect_equal(sum(initial_cohort_abundance("Spring", 2003)),
             annual_abundance[["Spring"]][annual_abundance[["WaterYear"]] == 2003])

# survival ----------------------------------------------------------

set.seed(1234)
rs1 <- rearing_survival(10, 0.95, 0.999)
expect_true(rs1 > 0.634 && rs1 < 0.635)
set.seed(567)
rs2 <- rearing_survival(20, 0.5, 1)
expect_true(rs2 > 0.064 && rs2 < 0.065)
set.seed(89)
os1 <- ocean_survival(70)
expect_true(os1 > 0.00035 && os1 < 0.00036)

# entrainment ----------------------------------------------------------

set.seed(816)
expect_equal(entrainment("Exg", 500, 1000), c(Yolo = 778, Sac = 222))
