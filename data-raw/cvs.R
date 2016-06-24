## Critical values

## 40 mins
## system.time(snoopingcvs <- DFSnoopingCV(S=20000, T=1000, grid.length=1000))
## 4hrs
system.time(snoopingcvs <- DFSnoopingCV(S=40000, T=4000, grid.length=1000))
## 90000-40000-1500

devtools::use_data(snoopingcvs, overwrite=TRUE, internal=FALSE)
