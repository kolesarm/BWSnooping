## Critical values

## 40 mins
## system.time(snoopingcvs <- DFSnoopingCV(S=20000, T=1000, ngr=1000))
## 4hrs
## system.time(snoopingcvs <- DFSnoopingCV(S=40000, T=4000, ngr=1000))

system.time(snoopingcvs <- DFSnoopingCV(S=60000, T=10000, ngr=1000))
devtools::use_data(snoopingcvs, overwrite=TRUE, internal=FALSE)
