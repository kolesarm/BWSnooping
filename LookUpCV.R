LookUpCV <- function(h.ratio, kernel, onesided=FALSE, coverage=FALSE, level=0.95){
    ## look up appropriate adjusted critical value or coverage of an unadjusted
    ## confidence band

    load('cvs-90000-40000-1500.Rdata') # coad cvs data frame
    cv <- cvs[which.max(cvs$h>= h.ratio & cvs$level==level &
                        cvs$kernel==kernel),]
    if(abs(cv$h-h.ratio>0.1))
              warning('Cannot look up cv adjustment precisely')
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
