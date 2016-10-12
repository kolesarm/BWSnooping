#' Critical values based on extreme value approximation
#'
#' Calculate one- and two-sided critical values based on extreme value
#' approximation to the limiting distribution as the ratio of maximum to minimum
#' bandwidth diverges to infinity. Not recommended to use in practice (the
#' critical values provided by \code{\link{SnoopingCV}} are more accurate), only
#' for illustration.
#'
#' @param kernel Either one of \code{"uniform"}, \code{"triangular"}, or
#'     \code{"epanechnikov"}, or else an (equivalent) kernel function supported
#'     on \eqn{[-1,1]}.
#' @inheritParams SnoopingCV
#' @export
EVSnoopingCV <- function(bwratio, kernel, boundary, order, onesided=FALSE,
                       alpha=0.05){

    if (is.character(kernel))
        kernel <- EqKern(kernel, boundary, order)

    if (kernel(1)!=0) {
        c1 <- kernel(1)^2 * pi^(-1/2) * (1/2) /
                stats::integrate(function(u) kernel(u)^2, 0, 1)$value

        b <- function(t) log(c1)+ (log(pmax(log(log(t)), 1))) / 2
    } else {
        integrand <- function(u)
        (((kernel(u+1e-8)-kernel(u))/1e-8)*u+kernel(u)/2)^2

        c2 <-  stats::integrate(integrand, 0, 1)$value /
                  stats::integrate(function(u) kernel(u)^2, 0, 1)$value
        c2 <- sqrt(c2)/(2*pi)

        b <- function(t) log(c2) * (t-t+1)   # get dimension right
    }

    c3 <- if(onesided) 1 else 1/2
    ll <- pmax(1, log(log(bwratio)))

    (-log(-c3*log(1-alpha)) + b(bwratio)) / sqrt(2*ll)+sqrt(2*ll)
}

#' Compare critical values based on Gaussian and extreme value approximation
#'
#' @return figure comparing critical values
#' @export
PlotEVSnoopingCV <- function() {
    orders <- 0:1
    bwratios <- seq(20, 99, by=1)
    d <- expand.grid(kernel=c("uniform", "triangular", "epanechnikov"),
                     order=orders, stringsAsFactors=FALSE)
    df <- data.frame()
    for (j in seq_len(nrow(d))) {
        ev <- EVSnoopingCV(bwratio=bwratios, kernel=d[j, 1], boundary=TRUE,
                           order=d[j, 2])
        cv <- sapply(bwratios, function(t)
            SnoopingCV(t, kernel=d[j,1], boundary=TRUE, order=d[j, 2]))
        m <- rep(c("Gaussian", "Extreme Value"), each=length(bwratios))
        df <- rbind(df, data.frame(kernel=d[j, 1], order=d[j, 2],
                                   boundary=TRUE, method=m,
                                   cv=c(cv, ev), t=bwratios))
    }


    ggplot2::qplot(x=t, y=cv, data=df, geom="line", color=method,
                   linetype=method) +
        ggplot2::facet_grid(kernel~order) + ggplot2::theme_bw()
}
