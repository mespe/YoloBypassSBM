library(YoloBypassSBM)

simulation_parameters$reps = 1:100
simulation_parameters$name = "baseline"

Rprof(file.path("prof", "baseline.out"))
cat("baseline", "\t", system.time({run_simulation()}), file = "timings")
Rprof(NULL)
