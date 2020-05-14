library(CodeAnalysis)
library(codetools)

ff = list.files("R", full.names = TRUE)

e = loadCode("R")
bs = ls("package:base")
globals = lapply(ls(e), function(fn){
    setdiff(findGlobals(get(fn, e)), bs)
    })

names(globals) = ls(e)
tt = getGlobals(get("run_one_rep", e))
tt$variables
unique(tt$functions)
names(tt)
