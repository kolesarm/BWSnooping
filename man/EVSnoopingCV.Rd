% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extreme_value.R
\name{EVSnoopingCV}
\alias{EVSnoopingCV}
\title{Critical values based on extreme value approximation}
\usage{
EVSnoopingCV(bwratio, kernel, boundary, order, onesided = FALSE,
  alpha = 0.05)
}
\arguments{
\item{bwratio}{ratio of maximum to minimum bandwidth, number greater than 1}

\item{kernel}{Either one of \code{"uniform"}, \code{"triangular"}, or
\code{"epanechnikov"}, or else an (equivalent) kernel function supported
on \eqn{[-1,1]}.}

\item{boundary}{Logical specifying whether regression is in the
interior or on the boundary, and an integer specifying order of local
polynomial. If \code{kernel} is \code{"uniform"}, \code{"triangular"}, or
\code{"epanechnikov"}, the appropriate boundary or interior equivalent
kernel is used. If \code{kernel} is a function, these options are
ignored.}

\item{order}{Logical specifying whether regression is in the
interior or on the boundary, and an integer specifying order of local
polynomial. If \code{kernel} is \code{"uniform"}, \code{"triangular"}, or
\code{"epanechnikov"}, the appropriate boundary or interior equivalent
kernel is used. If \code{kernel} is a function, these options are
ignored.}

\item{onesided}{Logical specifying whether the critical value corresponds to
a one-sided confidence interval.}

\item{alpha}{number specifying confidence level, \code{0.05} by default.}
}
\description{
Calculate one- and two-sided critical values based on extreme value
approximation to the limiting distribution as the ratio of maximum to minimum
bandwidth diverges to infinity. Not recommended to use in practice (the
critical values provided by \code{\link{SnoopingCV}} are more accurate), only
for illustration.
}
