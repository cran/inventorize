
#' CSOE
#'
#' Cost per stockout event
#'
#' Calculating K value that corresponds to the cost per stock out event, how much quantity should be put in stock as a minimum.the function solves for optimum K
#' based on the stock out event. It should be noted that the condition(output) should be bigger than 1.
#' other wise set K as per management.
#' @param  quantity, numeric,quantity replinished every cycle.
#' @param demand numeric,annual Expected  demand of the SKU .
#' @param standerddeviation numeric,  standard  deviation of the SKU during season.
#'
#' @param  leadtimeinweeks  numeric,leadtime in weeks of order.
#' @param cost numeric,cost of item.
#' @param costSoe  numeric, estimated cost per stockout event.
#' @param holdingrate numeric, holding rate per item per year,percentage.
#' @param na.rm removes na values if TRUE, TRUE by default
#'
#' @return a dataframe that contains calculations of K and the minimum quantity to be put in stock .
#'
#' @import stats
#' @author "haytham omar  email: <haytham@rescaleanalytics.com>"
#' @note this is the second version of the inventorize package, all the fucntions are  without
#' any academic contribution from my side, the aim is to facilitate and ease much of the bookkeeping that is endured during stock analysis.
#' @export
#' @examples
#' CSOE(quantity=1000,demand=40000,standerddeviation=200,leadtimeinweeks=3,
#' cost=500,costSoe=30000,holdingrate=0.2,na.rm=TRUE)



CSOE<- function(quantity,demand,standerddeviation,leadtimeinweeks,cost,costSoe,holdingrate,na.rm=TRUE){

  DL<- demand* leadtimeinweeks/52
  sigmadl<- standerddeviation *sqrt(leadtimeinweeks/52)
  holdingcost<- holdingrate*cost
  condition<- (demand*costSoe)/(holdingcost*quantity*sigmadl*sqrt(2*pi))
  k<- sqrt(2*log(condition))
  s<- DL+sigmadl*k
  return(data.frame(demandleadtime=DL,sigmadl=sigmadl,condition=condition,k=k,min= s))
}




