

#' eoqsenstivity
#'
#'the rate of increase of total relevant cost compared to the EOQ.
#' @param quantity numeric,quantity ordered every order cycle.
#' @param  quantityoptimal , numeric optimal quantity based on EOQ.
#'
#' @param na.rm A logical indicating whether missing values should be removed
#'
#' @return the rate of increase of total relevant cost compared to the EOQ.
#' @author "haytham omar  email: <haytham@rescaleanalytics.com>"
#' @note this is the second version of the inventorize package, all the fucntions are  without
#' any academic contribution from my side, the aim is to facilitate and ease much of the bookkeeping that is endured during stock analysis.
#' @export
#' @examples
#' eoqsenstivity(quantity=5400,quantityoptimal=6000,na.rm=TRUE)






eoqsenstivity<- function(quantity,quantityoptimal,na.rm=TRUE){
  (1/2)*((quantity/quantityoptimal)+(quantityoptimal/quantity))
}
