#' Definitions of equivalent kernels, same as in RDHonest package
#' @keywords internal
EqKern <- function(kernel = "uniform", boundary = TRUE, order = 0) {

    if(!(kernel %in% c("uniform", "triangular", "epanechnikov")))
        warning("I don't know how to compute equivalent '",
                kernel, "' kernel.")

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


#' Plotting critical values
#' @keywords internal
baseplot <- function(data, ylab, xlim=NULL, method=NULL) {
    p <- ggplot2::qplot(x=t, y=y, data=data, geom="line",
                        color=what, linetype=what) +
        ggplot2::xlab("$\\overline{h}/\\underline{h}$") + ggplot2::ylab(ylab) +
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
    p
}
