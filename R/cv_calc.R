#' Snooping-robust critical values for bandwidth ratios on a grid.
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
        Hhat <- colSums(stats::rnorm(T)*ku)/ sqrt(pmax(colSums(ku), 1e-10))
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


#' Definitions of equivalent kernels, same as in RDHonest package
#' @keywords internal
EqKern <- function(kernel = "uniform", boundary = TRUE, order = 0) {
    ## support
    su <- function(u) (u <= 1) * (u >= -1 + boundary)
    if (order == 0 && boundary == TRUE) {
        switch(kernel,
               uniform = function(u) su(u),
               triangular = function(u) 2 * (1 - u) * su(u),
               epanechnikov = function(u) (3 / 2) * (1 - u^2) * su(u))
    } else if (order == 1 && boundary == TRUE) {
        switch(kernel,
               uniform = function(u) (4 - 6*u) * su(u),
               triangular = function(u) 6*(1 - 2*u) * (1 - u) * su(u),
               epanechnikov = function(u) 6/19 * (16-30*u) * (1-u^2) * su(u))
    } else if (order == 2 && boundary == TRUE) {
        switch(kernel,
               uniform = function(u) (9 - 36*u + 30*u^2) * su(u),
               triangular = function(u) 12 * (1-5*u+5*u^2) * (1-u) * su(u),
               epanechnikov = function(u) 1/8 * (85 - 400*u + 385*u^2) *
                   (1-u^2) * su(u))
    } else if (order == 0 && boundary == FALSE) {
        switch(kernel,
               uniform = function(u) su(u) / 2,
               triangular = function(u) (1 - abs(u)) * su(u),
               epanechnikov = function(u) (3 / 4) * (1 - u^2) * su(u))
    } else if (order == 1 && boundary == FALSE) {
        switch(kernel,
               uniform = function(u) su(u) / 2,
               triangular = function(u) (1 - abs(u)) * su(u),
               epanechnikov = function(u) 3/4 * (1 - u^2) * su(u))
    } else if (order == 2 && boundary == FALSE) {
        switch(kernel,
               uniform = function(u) (9 - 15 * u^2) / 8 * su(u),
               triangular = function(u) 6/7 * (2-5*u^2) * (1-abs(u)) * su(u),
               epanechnikov = function(u) 15/32 * (3-7*u^2) * (1-u^2) * su(u))
    }
}


#' Calculate snooping-robust critical values for common kernels
#'
#' Calaculates a data frame of snooping-robust critical values for uniform,
#' triangular and epanechnikov kernels and local constant, local, and quadratic
#' regression in the interior and on the boundary.
#' @param grid.length length of grid on which to evaluate the Gaussian process
#'     \eqn{\hat{\mathbb{H}}(h)}
#' @inheritParams GridSnoopingCV
#' @export
DFSnoopingCV <- function(S, T, grid.length) {

    ## make a decreasing log grid for h values
    grid <- exp(seq(log(1), log(0.01), length.out=grid.length))
    d <- expand.grid(kernel=c("uniform", "triangular", "epanechnikov"),
                     boundary=c(TRUE, FALSE),
                     order=0:2, stringsAsFactors = FALSE)

    cvs <- data.frame()
    for (j in seq_len(nrow(d))) {
        k <- function(u) EqKern(kernel=d$kernel[j],
                                boundary=d$boundary[j], order=d$order[j])(u)
        cvs <- rbind(cvs, data.frame(kernel=d$kernel[j], boundary=d$boundary[j],
                                     order=d$order[j],
                                     GridSnoopingCV(S, T, grid, k)))
    }

    cvs
}


#' Plotting critical values
#' @keywords internal
baseplot <- function(data, ylab, xlim=NULL, method=NULL) {
    p <- ggplot2::qplot(x=t, y=y, data=data, geom="line",
                        color=what, linetype=what) +
        ggplot2::xlab("$h_{\\max}/h_{\\min}$") + ggplot2::ylab(ylab) +
        ggplot2::theme_classic() +
        ggplot2::theme(legend.position = "none",
                       strip.background=ggplot2::element_rect(color = "white",
                           linetype=NULL),
                       axis.line.x=ggplot2::element_line(color="black",
                           size=0.2, linetype="solid"),
                       axis.line.y=ggplot2::element_line(color="black",
                           size=0.2, linetype="solid"))

    if(!is.null(xlim))
        p <- p + ggplot2::coord_cartesian(xlim = xlim)

    if (!is.null(method))
        p <-  p + directlabels::geom_dl(ggplot2::aes(label=what),
                                        data=data, method=method)

}



#' Tables and graphs of snooping-robust critical values
#'
#' Print tables of snooping-adjusted critical values and return ggplot2 object
#' for plotting them
#' @param cvs Data frame returned by \code{\link{DFSnoopingCV}}
#' @param bwr.print values of bandwidth ratios to print in summary tables
#' @param maxratio maximum ratio of maximum to minimum bandwidth to plot
#' @export
SnoopingTablesGraphs <- function(cvs, maxratio=10,
    bwr.print=c(seq(1, 2, by=0.2), 3:10, 20, 50, 99.9)) {

    ## boundary, order; one and two-sided
    t.print <- cvs$t[sapply(bwr.print, function(h) which.max(cvs$t>=h))]

    PrintTable <- function(bd, order) {
        subs <- cvs[cvs$boundary==bd & cvs$order==order & cvs$t %in% t.print,
                    -c(2, 3, 8, 9)]
        one <- reshape2::dcast(subs, t~kernel+level, value.var="onesided")
        two <- reshape2::dcast(subs, t~kernel+level, value.var="twosided")
        names(one)[-1] <- names(two)[-1] <-
            as.vector(outer(unique(cvs$level), c("u", "t", "e"), paste0))

        cat("\nBoundary:", bd, "Order: ", order, "\nOnesided:\n")
        pander::pandoc.table(one, digits=3, style="simple")
        cat("Twosided\n")
        pander::pandoc.table(two, digits=3, style="simple")
    }

    d <- expand.grid(bd=c(TRUE, FALSE), order=0:2, stringsAsFactors=FALSE)
    for (j in seq_len(nrow(d))) PrintTable(d$bd[j], d$order[j])

    ## plot 95\% CVs
    cvs$what <- as.vector(cvs$kernel)
    cvs$what[cvs$order==1] <- paste0(cvs$kernel[cvs$order==1], " (ll)")
    subs <- cvs[cvs$order<2 & cvs$level==0.95 & cvs$t<=maxratio, -c(1, 3, 5)]

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

    list(cv.interior=pit, cv.boundary=pbt, cov.interior=pic, cov.boundary=pbc)
}


################################################################

#' Snooping-adjusted critical value
#'
#' Look up appropriate adjusted critical value or coverage of an unadjusted
#' confidence band
#' @param ratio ratio of maximum to minimum bandwidth, number between 1 and 100
#' @param kernel TODO uniform, triangular, and epanechnikov, luniform, quniform
#' for local constant (Nadaraya-Watson), local linear or local quadratic
#' @param order TODO
#' @param onesided Logical specifying whether the critical value corresponds to
#' a one-sided confidence interval (\code{FALSE} by default)
#' @param coverage report coverage of unadjusted CIs instead?
#' @param level number specifying confidence level, \code{0.95} by default,
#' other choices are \code{0.9} and \code{0.99}.
#'
#' The function looks up appropriate critical value form a table of pre-computed
#' values.
#' @examples
#' ## look up appropriate 99% critical value for a regression
#' ## discontinuity design using a triangular kernel, with ratio of maximum to
#' ## minimum bandwidths equal to 6.2
#' SnoopingCV(6.2, 'ltriangular', onesided=FALSE, level=0.99)
#' @return critical value
#' @export
SnoopingCV <- function(ratio, kernel, order, onesided=FALSE,
                     coverage=FALSE, level=0.95) {

    cv <- cvs[which.max(cvs$h>= ratio & cvs$level==level &
                        cvs$kernel==kernel), ]
    if(abs(cv$h-ratio > 0.1))
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
