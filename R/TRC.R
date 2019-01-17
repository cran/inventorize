


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
#' @export
#' @author "haytham omar  email: <h.omar5942@gmail.com>"
#' @note this is the first version of the inventorize package, all the fucntions are common knowlege for supply chain without
#' any academic contribution from my side, the aim is to facilitate and ease much of the bookkeeping that is endured during stock analysis.
#' @examples
#' TRC(annualdemand=2500,orderingcost=250,purchasecost=98,holdingrate=0.25,na.rm=TRUE)








TRC<- function(annualdemand,orderingcost,purchasecost,holdingrate,na.rm=TRUE){

  relavantcost<- sqrt(2*annualdemand*orderingcost*purchasecost*holdingrate)
  return(relavantcost)
}

