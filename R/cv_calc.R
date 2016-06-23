#' Snooping-robust critical values
#'
#' Look up appropriate adjusted critical value or coverage of an unadjusted
#' confidence band
#' @param h.ratio ratio of maximum to minimum bandwidth, number between 1 and 100
#' @param kernel TODO uniform, triangular, and epanechnikov, luniform, quniform
#' for local constant (Nadaraya-Watson), local linear or local quadratic
#' @param order TODO
#' @param onesided Logical specifying whether the critical value corresponds to
#' a one-sided confidence interval (\code{FALSE} by default)
#' @param level number specifying confidence level, \code{0.95} by default,
#' other choices are \code{0.9} and \code{0.99}.
#'
#' The function looks up appropriate critical value form a table of pre-computed
#' values.
#' @examples
#' ## look up appropriate 99% critical value for a regression
#' ## discontinuity design using a triangular kernel, with ratio of maximum to
#' ## minimum bandwidths equal to 6.2
#' LookUpCV(6.2, 'ltriangular', onesided=FALSE, level=0.99)
#' @return critical value
#' @export
LookUpCV <- function(ratio, kernel, order, onesided=FALSE,
                     coverage=FALSE, level=0.95) {

    cv <- cvs[which.max(cvs$h>= h.ratio & cvs$level==level &
                        cvs$kernel==kernel), ]
    if(abs(cv$h-h.ratio > 0.1))
              warning("Cannot look up cv adjustment precisely")
    if(coverage & onesided) {
        cv$c.onesided
    } else if (coverage) {
        cv$c.twosided
    } else if (onesided) {
        cv$onesided
    } else {
        cv$twosided
    }
}

################################################################

#' Calculate one- and two-sided critical values for values of bandwidth in
#' \code{h.grid}
#' @param S number of observations per simulation
#' @param T number of simulations
#' @param h.grid grid, subset of unit interval
#' @param kernel TODO (kernel type)
#' @param alphas vector of levels for which to compute critical values
#' @keywords internal
KernelCVs <- function(S, T, h.grid, kernel, alphas) {

    set.seed(7)
    ngr <- length(h.grid)                # number of grid points

    ## compute sup_{h<h'<hmax} Hhat and sup_{h<h'<hmax} |Hhat|, where
    ## hmax=max(h.grid) for all h in hgrid
    sup.H <- matrix(nrow=S, ncol=ngr)     # sup.H[m,h]=sup_{h<h'<1} Hhat(h')
    sup.absH <- matrix(nrow=S, ncol=ngr)

    k <- Kern(outer((1:T)/T, 1/h.grid), kernel) # k[i,h]=k(i/Th)

    for (m in 1:S){
        sums <- colSums(rnorm(T)*k)            # sum over i
        Hhat <- sums / sqrt(T*kKernC[kernel, "nu0"]*h.grid) # Hhat(h)=Hhat[h]

        sup.H[m, ] <- cummax(Hhat)
        sup.absH[m, ] <- cummax(abs(Hhat))
    }

    ## Compute the critical values and return as data frame
    onesided <- apply(sup.H, 2, function(u) quantile(u, 1-alphas))
    twosided <- apply(sup.absH, 2, function(u) quantile(u, 1-alphas))

    ## Compute coverage when using unadjusted critical values
    c.onesided <- sapply(qnorm(1-alphas),
                         function(c) colMeans(sup.H < c))
    c.twosided <- sapply(qnorm(1-alphas/2),
                         function(c) colMeans(sup.absH < c))

    data.frame(h=rep(1/h.grid, times=length(alphas)),
               level=rep(1-alphas, each=ngr),
               onesided=as.vector(t(onesided)),
               twosided=as.vector(t(twosided)),
               c.onesided=as.vector(c.onesided),
               c.twosided=as.vector(c.twosided), kernel=kernel)
}



## Calculate critical values based on approximation to Gaussian process in
## corollary to main result. For our method to work, we require that a kernel
## have bounded support. In the paper it's [-A, A], here we normalize that to
## [-1,1] without much loss of generality

## The tables and graphs in the paper can be replicated by running
## rm(list=ls());source("cval_calc.R");system.time(SimCvals(1000, 1000, 100))
## (num.sim=9000, sample.size=9000, num.gridpts=900) takes 25mins
## (num.sim=20000, sample.size=10000, num.gridpts=200) takes 800s

library(ggplot2)
library(reshape)
library(directlabels)

source("helper_functions/kernels.R")


CVs <- function(S, T, ngr) {
    ## Compute one- and two-sided 90%, 95% and 99% critical values for different
    ## kernels and bandwidth ratios. S is number of simulation draws, T number
    ## of samples from the Gaussian process in each simulation draw. ngr is
    ## number of grid points for h

    alphas <- c(.1, 0.05, 0.01);        # levels for critical values

    ## make a decreasing log grid for h values
    hmin <- 0.01                         # look at bandwidths in [hmin,1]
    h.grid <- exp(seq(log(1), log(hmin), length.out=ngr))

    ## data frame of one- and two-sided critical values
    cvs <- data.frame()
    for (k in kKernNames)             # kKernNames defined in kernels.R
        cvs <- rbind(cvs, KernelCVs(S, T, h.grid, kernel=k, alphas))

    cvs
}

CVsTablesGraphs <- function(cvs, h.print, savestring="", xmax=10){
    ## given a data frame of critical values, return a table for select
    ## bandwidths and plot them

    h.approx <- cvs$h[sapply(h.print, function(h) which.max(cvs$h>=h))]

    PrintTable <- function(value){
        cat(sprintf("\n%s\n", value))
        print(round(cast(df, h~kernel, value=value), digits=2))
    }

    for (l in unique(cvs$level)) {
        df <- subset(cvs, h %in% h.approx & level==l)

        cat(sprintf("\n\nlevel=%f", l))
        PrintTable("onesided")
        PrintTable("twosided")
        PrintTable("c.onesided")
        PrintTable("c.twosided")

        ## save a plot of the critical values
        p <- qplot(x=h, y=twosided, data=subset(cvs, level==l & h<=xmax),
                   geom="line", color=kernel, linetype=kernel) +
                       xlim(c(0.5, xmax+2)) +
                labs(x="h_{max}/h_{min}", y="Critical value") + theme_bw()

        pdf(paste("figures/cvs-twosided", l, "-",
                  savestring, "-", xmax, ".pdf", sep=""))
        print(direct.label(p+theme(legend.position = "none"),
                           list(cex=1, dl.trans(x=x+0.1), "last.qp")))
        dev.off()

        ## save a plot of coverage
        p <- qplot(x=h, y=c.twosided, data=subset(cvs, level==l & h<=xmax),
                   geom="line", color=kernel, linetype=kernel) +
                       xlim(c(0.5, xmax+2)) +
                labs(x="h_{max}/h_{min}", y="Coverage") + theme_bw()

        pdf(paste("figures/coverage-twosided", l, "-", savestring,
                  "-", xmax, ".pdf", sep=""))
        print(direct.label(p+theme(legend.position = "none"),
                           list(cex=1, dl.trans(x=x+0.1), "last.qp")))
        dev.off()
    }
}

SimCvals <- function(num.sim, sample.size, num.gridpts){
    ## compute critical values by simulation and graph and table them

    cvs <- CVs(num.sim, sample.size, num.gridpts)

    savestring <- paste(num.sim, "-", sample.size, "-", num.gridpts, sep="")
    save(file=paste("data/cvs-", savestring, ".Rdata", sep=""), list=ls())

    ## print a table of critical values and graph them
    ## which values to print
    h.print <- c(seq(from=1, to=2, by=0.2), 3:10, 20:50, 99.9)
    CVsTablesGraphs(cvs, h.print, savestring=savestring)
}

## Diagnostics: when the ratio is one, then the critical value should be
## qnorm(1-alpha). Sample mean and variance of Hhat for each h should be about 0
## and 1. Plot realizations of the process
