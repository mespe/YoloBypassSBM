library(YoloBypassSBM)

# For saving
git_hash = system("git log --pretty=format:'%h' -n 1", intern = TRUE)
simulation_parameters$reps = 1:100
simulation_parameters$name = git_hash

# For profiling
Rprof(file.path("prof", paste0(git_hash,".out")))
cat(git_hash, "\t", system.time({run_simulation()}),"\n",
    file= "timings", append=TRUE)
Rprof(NULL)

## Test
a = readRDS(file.path("results", "baseline-Reps-1-100.rds"))
b = readRDS(file.path("results", paste0(git_hash, "-Reps-1-100.rds")))

all.equal(a,b)

