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
#'
#' @return a dataframe that contains calculations of K the cost per item short metric noting that condition must me less than 1.
#'
#'@import stats
#' @export
#' @author "haytham omar  email: <h.omar5942@gmail.com>"
#' @note this is the first version of the inventorize package, all the fucntions are basic knowlege for supply chain without
#' any contribution from my side, the aim is to facilitate and ease much of the bookkeeping that is endured during stock analysis.
#' @examples
#' saftey_stock_normal(annualdemand=8000,annualstandarddeviation=600,
#' leadtimeinweeks=4,csl=0.92,na.rm=TRUE)






saftey_stock_normal<- function(annualdemand,annualstandarddeviation,leadtimeinweeks,csl,na.rm=TRUE){

  demandleadtime<- annualdemand *leadtimeinweeks/52
  sigmadl<- annualstandarddeviation* sqrt(leadtimeinweeks/52)
  safteyfactor<- qnorm(csl)
  safteystock<-safteyfactor*sigmadl
  quantityinstock<- demandleadtime+safteystock
  allpar<- data.frame("demandleadtime"= demandleadtime,"sigmadl"=sigmadl,"safteyfactor"=safteyfactor,"quantityinstock"=quantityinstock)
  return(allpar)
}

