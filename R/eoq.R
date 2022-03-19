
#' eoq
#'
#' economic order quantity.
#'
#' @param annualdemand numeric,annual demand of the SKU.
#' @param  orderingcost, numeric ordeing cost of the SKU
#' @param  purchasecost ,numeric, purchase cost per item
#' @param holdingrate    numeric holding rate per item per year.
#' @param na.rm A logical indicating whether missing values should be removed
#'
#' @return the eoq,cycle stock time in years and cycle stock time in weeks.
#' @author "haytham omar  email: <haytham@rescaleanalytics.com>"
#' @note this is the second version of the inventorize package, all the fucntions are  without
#' any academic contribution from my side, the aim is to facilitate and ease much of the bookkeeping that is endured during stock analysis.
#' @importFrom stats dnorm
#' @importFrom stats lm
#' @importFrom stats median
#' @importFrom stats optim
#' @importFrom stats optimize
#' @importFrom stats pnorm
#' @importFrom stats ppois
#' @importFrom stats predict
#' @importFrom stats qnorm
#' @export
#' @examples
#' eoq(annualdemand=5000,orderingcost=400,purchasecost=140,holdingrate=0.2,na.rm=TRUE)






eoq<- function(annualdemand,orderingcost,purchasecost,holdingrate,na.rm=TRUE){

  eoq<-sqrt((annualdemand*2*orderingcost)/(purchasecost*holdingrate))
  T_years<- eoq/annualdemand
  T_weeks<- T_years*52
  return(data.frame(EOQ=eoq,T_years=T_years,T_weeks=T_weeks))
}


