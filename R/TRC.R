


#' TRC
#'
#' Identyfing Total relevant cost.
#'
#'
#'
#' @param  annualdemand numeric annual demand of the SKU.
#' @param orderingcost numeric  ordering cost of the SKU.
#' @param purchasecost numeric purchase cost of the SKU.
#' @param holdingrate numeric holding rate of the SKU.
#' @param na.rm logical, TRUE to remove na.
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
#' @author "haytham omar  email: <haytham@rescaleanalytics.com>"
#' @note this is the second version of the inventorize package, all the fucntions are  without
#' any academic contribution from my side, the aim is to facilitate and ease much of the bookkeeping that is endured during stock analysis.
#' @export
#' @examples
#' TRC(annualdemand=2500,orderingcost=250,purchasecost=98,
#' holdingrate=0.25,na.rm=TRUE)








TRC<- function(annualdemand,orderingcost,purchasecost,holdingrate,na.rm=TRUE){

  relavantcost<- sqrt(2*annualdemand*orderingcost*purchasecost*holdingrate)
  return(relavantcost)
}

