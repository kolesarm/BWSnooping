## Critical values
## TODO: replicate how this table was generated
## cvs-num.sim-sample.size-num.gridpts.RData
load("cvs-90000-40000-1500.Rdata")

devtools::use_data(cvs, overwrite=TRUE, internal=TRUE)

## snoopingcvs <- DFSnoopingCV(S=1000, T=1000, grid.length=500)

snoopingcvs <- DFSnoopingCV(10000, 10000, 500)

devtools::use_data(snoopingcvs, overwrite=TRUE, internal=FALSE)
