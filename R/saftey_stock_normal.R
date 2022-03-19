#' saftey_stock_normal
#'
#' Calculating saftey stock based on the cycle service level.
#'
#' Calculating saftey stock  based on  the cycle service level in   an assumed normal distribution.
#'
#'
#' @param annualdemand numeric,annual Expected  demand of the SKU .
#' @param annualstandarddeviation numeric,  standard  deviation of the SKU during season.
#'
#' @param  leadtimeinweeks  leadtime in weeks or order.
#' @param  csl  cycle service level requested
#' @param na.rm Logical, remove na if TRUE
#' @importFrom stats dnorm
#' @importFrom stats lm
#' @importFrom stats median
#' @importFrom stats optim
#' @importFrom stats optimize
#' @importFrom stats pnorm
#' @importFrom stats ppois
#' @importFrom stats predict
#' @importFrom stats qnorm
#' @importFrom stats qpois
#' @importFrom stats sd
#' @return a dataframe that contains calculations of K the cost per item short metric noting that condition must me less than 1.
#' @author "haytham omar  email: <haytham@rescaleanalytics.com>"
#' @note this is the second version of the inventorize package, all the fucntions are  without
#' any academic contribution from my side, the aim is to facilitate and ease much of the bookkeeping that is endured during stock analysis.
#' @export
#' @examples
#' saftey_stock_normal(annualdemand=8000,annualstandarddeviation=600,
#' leadtimeinweeks=4,csl=0.92,na.rm=TRUE)

saftey_stock_normal <-
  function(annualdemand,annualstandarddeviation,leadtimeinweeks,csl,na.rm=TRUE){

    demandleadtime<- annualdemand *leadtimeinweeks/52
    sigmadl<- annualstandarddeviation* sqrt(leadtimeinweeks/52)
    safteyfactor<- qnorm(csl)
    safteystock<-safteyfactor*sigmadl
    quantityinstock<- demandleadtime+safteystock
    allpar<- data.frame("demandleadtime"= demandleadtime,"sigmadl"=sigmadl,"safteyfactor"=safteyfactor,"cyclestock+safteystock"=quantityinstock)
    return(allpar)
  }



