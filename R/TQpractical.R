

#' TQpractical
#'
#' Identyfing Practical ordering quantity based on the economic order quantity.it is assumed that practical
#' order quantity will be always withing 6 % of the economic order quantity in terms od total relevant cost.
#'
#'
#'
#' @param  annualdemand, numeric annual demand of the SKU.
#' @param orderingcost, numeric  ordering cost of the SKU.
#' @param purchasecost numeric purchase cost of the SKU.
#' @param holdingrate numeric holding rate of the SKU.
#' @param na.rm       logical, TRUE.
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
#' @return a dataframe that contains the economic order quantity and the practical order quantity, Tstar (optimum)and Tpractical
#' which is always away from the optimum up to 6%.
#' @author  "haytham omar  email: <haytham@rescaleanalytics.com>"
# 
#' @note this is the second version of the inventorize package, all the fucntions are  without
#' any academic contribution from my side, the aim is to facilitate and ease much of the bookkeeping that is endured during stock analysis.
#' @export
#' @examples
#' TQpractical(annualdemand=1000,orderingcost=100,
#' purchasecost=72,holdingrate=0.25,na.rm=TRUE)








TQpractical<- function(annualdemand,orderingcost,purchasecost,holdingrate,na.rm=TRUE){

  Tsyears<- sqrt((2*orderingcost)/(annualdemand*holdingrate*purchasecost))
  Tstarweeks<- sqrt((2*orderingcost)/(annualdemand*holdingrate*purchasecost))*52
  Qstar<-Tsyears*annualdemand
  Tpractical<- 2^ceiling(log(Tstarweeks/sqrt(2))/log(2))
  Qpractical<- Tpractical/52*annualdemand
  Tpracticalweeks<-Tpractical

  return(data.frame(Ts=Tsyears,Tstarweeks=Tstarweeks,Qstar=Qstar,
                    Tpractical=Tpractical,Tpracticalweeks=Tpracticalweeks,Qpractical=Qpractical))
}




