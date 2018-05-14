##' estimate the pe by growth
##'
##' estimate the pe by growth
##' @title estimate the pe by growth
##' @param growth , the growth rate in the following years,
##'    eg c(0.5,0.3,0.3) means 0.5, 0.30 ,0.30 growth in the following
##'    3 years
##' @param r return rate , 0.10 by default
##' @param fg forever growth , 0.05 by default
##' @return 
##' @author xuyang
##' @export
##' @examples pebyg(c(0.5,0.3,0.3))
##'   forever growth : pebyg(c(0.05),fg=0.05)
pebyg <- function(growth,r=0.1, fg=0.05)
{
    ## to c(1.5, 1.3 , 1.3)
    cf = growth + 1
    cf = cumprod(cf)
    ##forever growth
    inflow = cf[length(cf)]*(1+fg)/(1-(1+fg)/(1+r))
    cf = c(0, cf, inflow)
    tvm::npv(r,cf=cf)
}

##' calc pe according to the growth
##'
##' the growth is based on the latest year,  c(0.3,0.6,0.9)
##' will mean the real income is (1.3,1.6,1.9) in the following
##' 3 years. 
##' @title pebyg2 
##' @param growth 
##' @param ... 
##' @return 
##' @author xuyang
##' @export
pebyg2 <- function(growth, ...)
{
    gg <- growth + 1
    gg <- c(1,gg)
    realgrowth <- diff(gg)/gg[-length(gg)]
    pebyg(realgrowth,...)
}
 
