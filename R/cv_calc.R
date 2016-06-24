#' Snooping-adjusted critical values for bandwidth ratios on a grid.
#'
#' Calculate one- and two-sided critical values \eqn{c_{1-\alpha}(t;k)} for
#' values of \eqn{t} in \code{1/grid} based on evaluating the Gaussian process
#' \eqn{\hat{\mathbb{H}}(h)} at values of \eqn{h} equal to \code{grid}.
#' @param S number of draws of the Gaussian process \eqn{\hat{\mathbb{H}}(h)}
#' @param T number of draws from a normal distribution in each draw of the
#'     Gaussian process
#' @param grid a vector of values inside the unit interval specifying values of
#'     \eqn{h} at which to evaluate the Gaussian process.
#' @param kernel Kernel function \eqn{k(u)} supported on [-1,1] that takes a
#'     vector or a matrix as an argument \eqn{u}.
#' @param alpha A vector of values determining the confidence level
#'     \eqn{1-\alpha} at which to compute critical values
#' @keywords internal
GridSnoopingCV <- function(S, T, grid, kernel, alpha=c(0.1, 0.05, 0.01)) {

    set.seed(7)
    ngr <- length(grid)                # number of grid points
    ts <- sort(1/grid)

    ## compute sup.H[m, s]=sup_{s<h<hmax}{Hhat_{m}(h)} and
    ## sup_{s<h<hmax}{|Hhat_{m}(h)|}, where hmax=max(grid) for all s in grid
    sup.H <- sup.absH <- matrix(nrow=S, ncol=ngr)
    ku <- kernel(outer((1:T)/T, ts)) # k[i, h]=k(i/Th)

    for (m in 1:S) {
        ## In case all k(i/hT) evaluate to zero, set sum to 1e-10
        Y <- stats::rnorm(T)
        Hhat <- colSums(Y*ku)/ sqrt(pmax(colSums(ku^2), 1e-10))
        sup.H[m, ] <- cummax(Hhat)
        sup.absH[m, ] <- cummax(abs(Hhat))
    }
    onesided <- apply(sup.H, 2, function(u) stats::quantile(u, 1-alpha))
    twosided <- apply(sup.absH, 2, function(u) stats::quantile(u, 1-alpha))

    ## Compute coverage when using unadjusted critical values
    c.onesided <- sapply(stats::qnorm(1-alpha),
                         function(c) colMeans(sup.H < c))
    c.twosided <- sapply(stats::qnorm(1-alpha/2),
                         function(c) colMeans(sup.absH < c))

    data.frame(t=rep(ts, times=length(alpha)),
               level=rep(1-alpha, each=ngr),
               onesided=as.vector(t(onesided)),
               twosided=as.vector(t(twosided)),
               ua.onesided=as.vector(c.onesided),
               ua.twosided=as.vector(c.twosided))
}


#' Data frame of snooping-adjusted critical values for common kernels
#'
#' Calculates a data frame of snooping-adjusted critical values for uniform,
#' triangular, and epanechnikov kernels and local constant, linear, and
#' quadratic regression in the interior and on the boundary. Also calculates
#' coverage of confidence intervals without snooping adjustment.
#' @param grid.length length of grid on which to evaluate the Gaussian process
#'     \eqn{\hat{\mathbb{H}}(h)}. Critical values are computed for ratios of
#'     maximum to minumum bandwidth equal to each value in the grid.
#' @inheritParams GridSnoopingCV
#' @return data frame in the same format as the \code{\link{snoopingcvs}} data
#'     frame, which was generated using this funcion.
#' @export
DFSnoopingCV <- function(S, T, grid.length) {

    ## make a decreasing log grid for h values
    grid <- exp(seq(log(1), log(0.01), length.out=grid.length))
    d <- expand.grid(kernel=c("uniform", "triangular", "epanechnikov"),
                     boundary=c(TRUE, FALSE),
                     order=0:2, stringsAsFactors = FALSE)

    cvs <- data.frame()
    for (j in seq_len(nrow(d))) {
        k <- EqKern(kernel=d$kernel[j],
                    boundary=d$boundary[j], order=d$order[j])
        cvs <- rbind(cvs, data.frame(kernel=d$kernel[j], boundary=d$boundary[j],
                                     order=d$order[j],
                                     GridSnoopingCV(S, T, grid, k)))
    }

    cvs
}


#' Tables and graphs of snooping-robust critical values
#'
#' Generate tables of snooping-adjusted critical values as well as ggplot2
#' objects that plot them
#' @param cvs Data frame returned by \code{\link{DFSnoopingCV}}
#' @param bwr.print values of bandwidth ratios to print in summary tables
#' @param maxratio maximum ratio of maximum to minimum bandwidth to plot
#' @param orders vector of orders of local polynomial to plot
#' @return A list with two components: a list of tables and a list of figures
#' @export
SnoopingTablesGraphs <- function(cvs, maxratio=10,
                                 orders=0:2,
    bwr.print=c(seq(1, 2, by=0.2), 3:10, 20, 50, 99.9)) {

    ## boundary, order; one and two-sided
    t.print <- cvs$t[sapply(bwr.print, function(h) which.max(cvs$t>=h))]

    subs <- cvs[cvs$t %in% t.print, ]
    one <- reshape2::dcast(subs, boundary+order+t~kernel+level,
                           value.var="onesided")
    two <- reshape2::dcast(subs, boundary+order+t~kernel+level,
                           value.var="twosided")
    names(one)[-c(1:3)] <- names(two)[-c(1:3)] <-
        as.vector(outer(unique(cvs$level), c("u", "t", "e"), paste0))

    ## plot 95\% CVs
    cvs$what <- as.vector(cvs$kernel)
    cvs$what[cvs$order==1] <- paste0(cvs$kernel[cvs$order==1], " (ll)")
    cvs$what[cvs$order==2] <- paste0(cvs$kernel[cvs$order==2], " (lq)")
    subs <- cvs[cvs$order %in% orders & cvs$level==0.95 &
                    cvs$t<=maxratio, -c(1, 3, 5)]

    subs$y <- subs$twosided
    pit <- baseplot(data=subset(subs, boundary==FALSE), ylab="Critical value",
                    xlim=c(1, maxratio+2), method="last.qp")
    pbt <- baseplot(data=subset(subs, boundary==TRUE), ylab="Critical value",
                    xlim=c(1, maxratio+2), method="last.qp")
    subs$y <- subs$ua.twosided
    pic <- baseplot(data=subset(subs, boundary==FALSE), ylab="Critical value",
                    xlim=c(1, maxratio+2), method="last.qp")
    pbc <- baseplot(data=subset(subs, boundary==TRUE), ylab="Critical value",
                    xlim=c(1, maxratio+2), method="last.qp")

    list(cv.interior=pit, cv.boundary=pbt, cov.interior=pic, cov.boundary=pbc,
         table.onesided=one, table.twosided=two)
}


#' Snooping-adjusted critical value
#'
#' Look up appropriate snooping-adjusted critical value or coverage of an
#' unadjusted confidence band in a table of pre-computed critical values. If no
#' pre-computed value is found, calculate appropriate critical value by Monte
#' Carlo simulation.
#' @param bwratio ratio of maximum to minimum bandwidth, number greater than 1
#' @param kernel Either one of \code{"uniform"}, \code{"triangular"}, or
#'     \code{"epanechnikov"}, or else an (equivalent) kernel function
#' @param boundary,order Logical specifying whether regression is in the
#'     interior or on the boundary, and an integer specifying order of local
#'     polynomial. If \code{kernel} is \code{"uniform"}, \code{"triangular"}, or
#'     \code{"epanechnikov"}, the appropriate boundary or interior equivalent
#'     kernel is used. If \code{kernel} is a function, these options are
#'     ignored.
#' @param onesided Logical specifying whether the critical value corresponds to
#'     a one-sided confidence interval.
#' @param coverage Return coverage of unadjusted CIs instead of a critical
#'     value?
#' @param alpha number specifying confidence level, \code{0.05} by default.
#' @inheritParams GridSnoopingCV
#' @examples
#' ## look up appropriate 99% critical value for a regression
#' ## discontinuity design using a triangular kernel and local linear regression,
#' ## with ratio of maximum to minimum bandwidths equal to 6.2
#' SnoopingCV(6.2, "triangular", boundary=TRUE, order=1, alpha=0.01)
#' ## Values greater than 100 one will need to be computed:
#' SnoopingCV(110, "triangular", boundary=TRUE, order=1, alpha=0.01)
#' ## Equivalently, specify equivalent kernel explicitly
#' SnoopingCV(110, function(u) 6*(1 - 2*u) * (1 - u) * (u<=1), alpha=0.01)
#' @return critical value
#' @export
SnoopingCV <- function(bwratio, kernel, boundary, order, onesided=FALSE,
                       coverage=FALSE, alpha=0.95, S=10000, T=1000) {

    sub <- data.frame()
    if (is.character(kernel)) {
        sub <- snoopingcvs[snoopingcvs$t>=bwratio &
                           snoopingcvs$t<=(bwratio+0.1) &
                           snoopingcvs$level==(1-alpha) &
                           snoopingcvs$boundary==boundary &
                           snoopingcvs$kernel==kernel &
                           snoopingcvs$order==order, ]
    }

    if (nrow(sub)>=1) {
        sub <- subset(sub, t==min(sub$t))
    } else {
        ## Either we're not close enough, or else kernel not a function
        if (is.character(kernel))
            kernel <- EqKern(kernel, boundary, order)

        message("Computing critical value by Monte Carlo simulation")
        grid <- exp(seq(log(1), log(1/bwratio), length.out=100))
        sub <- GridSnoopingCV(S=S, T=T, grid, kernel,
                              alpha=alpha)[length(grid), ]
    }

    if(coverage & onesided) {
        sub$ua.onesided
    } else if (coverage) {
        sub$ua.twosided
    } else if (onesided) {
        sub$onesided
    } else {
        sub$twosided
    }
}
