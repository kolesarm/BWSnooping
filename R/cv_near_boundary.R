#' Snooping-adjusted critical values for estimation near a boundary.
#'
#' Calculate one- and two-sided critical values \eqn{c_{1-\alpha}(t;k)} for
#' values of \eqn{t} in \code{bwratios} based on evaluating the Gaussian process
#' \eqn{\hat{\mathbb{H}}(h)} at \code{ngr} values of \eqn{h} in the interval
#' \eqn{[1/t,1]}.
#' @param order Order of local linear regression
#' @param db Local distance to boundary, equal to \eqn{x_{0}/\underline{h}},
#'     where \eqn{x_{0}} is point of interest.
#' @param ngr number of grid points at which to evaluate the Gaussian process
#' @param bwratios Bandwidth ratios of maximum to minimum bandwidth for whcih to
#'     compute critical values
#' @inheritParams GridSnoopingCV
#' @keywords internal
SnoopingCVNearBd <- function(S, T, bwratios, kernel, order, db, ngr,
                             alpha=c(0.1, 0.05, 0.01)) {

    set.seed(7)
    ## get original kernel
    if (is.character(kernel)) kernel <- EqKern(kernel, FALSE, 0)
    ## equivalent kernel
    ek <- function(u, c0) {
        M <- sapply(0:(2*order), function(j)
            stats::integrate(function(u) u^j * kernel(u), -c0, 1)$value)
        ## Transform to a matrix
        M <- sapply(0:order, function(j) M[(j+1):(order+j+1)])

        solve(M, t(outer(u, 0:order, "^")))[1, ] * kernel(u)
    }

    ## Values of kernel for a given bandwidth ratio t
    ku <- function(t) {
        ## grid points at which to evaluate Gaussian process
        ss <- 1/exp(seq(log(1), log(t), length.out=ngr))
        ## ku[i, s]
        sapply(ss, function(s) ek(((db+t)*(1:T)/T - db) / (t*s), db/(t*s)) )
    }
    ## Precompute them for the bw ratios we look at
    kut <- lapply(bwratios, ku)

    sup.H <- sup.absH <- matrix(nrow=S, ncol=length(bwratios))
    for (m in 1:S) {
        ## In case all k(i/hT) evaluate to zero, set sum to 1e-10
        Y <- stats::rnorm(T)
        That <- function(j) colSums(Y*kut[[j]]) /
                                pmax(sqrt(colSums(kut[[j]]^2)), 1e-10)
        sups <- function(Ts) c(max(Ts), max(abs(Ts)))
        maxs <- sapply(seq_len(length(bwratios)), function(j) sups(That(j)))
        sup.H[m, ] <- maxs[1, ]
        sup.absH[m, ] <- maxs[2, ]
    }
    dfcv(bwratios, sup.H, sup.absH, alpha)
}


#' Table of snooping-adjusted critical values for estimation near a boundary
#'
#' Generate a table of two-sided snooping-adjusted critical values for a given kernel in a
#' local polynomial regression near a boudnary point
#' @param alpha Determines confidence level \eqn{1-\alpha} at which to compute
#'     critical values
#' @param db Local distance to boundary, equal to \eqn{x_{0}/\underline{h}},
#'     where \eqn{x_{0}} is point of interest.
#' @param kernel Either one of \code{"uniform"}, \code{"triangular"}, or
#'     \code{"epanechnikov"}, or else an (original) kernel function. Function
#'     computes appropriate equivalent kernel function
#' @param bwratios Bandwidth ratios of maximum to minimum bandwidth for which to
#'     compute critical values
#' @param order order of local polynomial
#' @inheritParams GridSnoopingCV
#' @return A table of snooping-adjusted critical values
#' @export
TableSnoopingCVNearBd <- function(S, T, ngr, bwratios, db, kernel="triangular",
                                  order=1, alpha=0.05) {
    df <- data.frame()
    for (dist in db) {
        r <- SnoopingCVNearBd(S, T, bwratios, kernel, order, dist, ngr, alpha)
        df <- rbind(df, data.frame(c=dist, r[, c(1, 3, 4)]))
    }

    list(table.onesided=reshape2::dcast(df, t~c,  value.var="onesided"),
         table.twosided=reshape2::dcast(df, t~c,  value.var="twosided"))
}
