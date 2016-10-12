## ---- include=FALSE, cache=FALSE-----------------------------------------
library("knitr")
knitr::opts_knit$set(self.contained = FALSE)
knitr::opts_chunk$set(tidy = TRUE, collapse=TRUE, comment = "#>",
                      tidy.opts=list(blank=FALSE, width.cutoff=75))

## ---- echo=TRUE----------------------------------------------------------
library("BWSnooping")
SnoopingCV(bwratio=6.2, kernel="triangular", boundary=TRUE, order=1, alpha=0.01)

## ---- echo=TRUE----------------------------------------------------------
SnoopingCV(bwratio=102, kernel="triangular", boundary=FALSE, order=0, alpha=0.05)

## ---- results="asis"-----------------------------------------------------
nb <- TableSnoopingCVNearBd(bwratios=c(1, 2, 3), kernel="triangular", db=c(1, 2, 10), order=1)

knitr::kable(nb$table.twosided, row.names=FALSE, digits=2,
    caption="Local linear regression near a boundary")
knitr::kable(nb$table.onesided, row.names=FALSE, digits=2,
    caption="Local linear regression near a boundary")

## ------------------------------------------------------------------------

## The function DFSnoopingCV may long time to compute, the package has results
## stored for DFSnoopingCV(S=60000, T=10000, ngr=1000)
## under the data frame snoopingcvs

r <- SnoopingTablesGraphs(snoopingcvs)

## ---- results="asis"-----------------------------------------------------
t1 <- subset(r$table.twosided, boundary==TRUE & order==0)[, -c(1, 2)]
knitr::kable(t1, row.names=FALSE, digits=2,
    caption="Boundary Nadaraya-Watson regression")

t2 <- subset(r$table.twosided, boundary==FALSE & order==0)[, -c(1, 2)]
knitr::kable(t2, row.names=FALSE, digits=2,
    caption="Interior Nadaraya-Watson regression")

t3 <- subset(r$table.twosided, boundary==TRUE & order==1)[, -c(1, 2)]
knitr::kable(t3, row.names=FALSE, digits=2,
    caption="Boundary local linear regression")

t4 <- subset(r$table.twosided, boundary==FALSE & order==1)[, -c(1, 2)]
knitr::kable(t4, row.names=FALSE, digits=2,
    caption="Interior local linear regression")

t5 <- subset(r$table.twosided, boundary==TRUE & order==2)[, -c(1, 2)]
knitr::kable(t5, row.names=FALSE, digits=2,
    caption="Boundary local quadratic regression")

t6 <- subset(r$table.twosided, boundary==FALSE & order==2)[, -c(1, 2)]
knitr::kable(t6, row.names=FALSE, digits=2,
    caption="Interior local quadratic regression")

## ---- results="asis"-----------------------------------------------------
o1 <- subset(r$table.onesided, boundary==TRUE & order==0)[, -c(1, 2)]
knitr::kable(o1, row.names=FALSE, digits=2,
    caption="Boundary Nadaraya-Watson regression")

o2 <- subset(r$table.onesided, boundary==FALSE & order==0)[, -c(1, 2)]
knitr::kable(o2, row.names=FALSE, digits=2,
    caption="Interior Nadaraya-Watson regression")

o3 <- subset(r$table.onesided, boundary==TRUE & order==1)[, -c(1, 2)]
knitr::kable(o3, row.names=FALSE, digits=2,
    caption="Boundary local linear regression")

o4 <- subset(r$table.onesided, boundary==FALSE & order==1)[, -c(1, 2)]
knitr::kable(o4, row.names=FALSE, digits=2,
    caption="Interior local linear regression")

o5 <- subset(r$table.onesided, boundary==TRUE & order==2)[, -c(1, 2)]
knitr::kable(o5, row.names=FALSE, digits=2,
    caption="Boundary local quadratic regression")

o6 <- subset(r$table.onesided, boundary==FALSE & order==2)[, -c(1, 2)]
knitr::kable(o6, row.names=FALSE, digits=2,
    caption="Interior local quadratic regression")

## ----message=FALSE, dev="tikz", out.width="0.7\\textwidth"---------------
r$cv.interior

## ----message=FALSE, dev="tikz", out.width="0.7\\textwidth"---------------
r$cv.boundary

## ----message=FALSE, dev="tikz", out.width="0.7\\textwidth"---------------
r$cov.interior

## ----message=FALSE, dev="tikz", out.width="0.7\\textwidth"---------------
r$cov.boundary

## ----message=FALSE, dev="tikz", out.width="0.7\\textwidth"---------------
p <- PlotEVSnoopingCV()
p+ggplot2::ylab("Critical value")+ggplot2::xlab("$\\overline{h}/\\underline{h}$")

