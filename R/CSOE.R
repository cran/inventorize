
#' CSOE
#'
#' CSOE<- function(quantity,demand,standerddeviation,leadtimeinweeks,cost,costSoe,holdingrate,na.rm=TRUE)
#'
#' Calculating K value that corresponds to the desired item fill rate.
#'
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
#' @return a dataframe that contains calculations of K the  item fill rate metric.cycle service level and expected unit short.
#'
#' @import stats
#' @author "haytham omar  email: <h.omar5942@gmail.com>"
#' @note this is the first version of the inventorize package, all the fucntions are common knowlege for supply chain without
#' any contribution from my side, the aim is to facilitate and ease much of the bookkeeping that is endured during stock analysis.
#' @export
#'
#' @examples
#' CSOE(quantity=1000,demand=4000,standerddeviation=200,leadtimeinweeks=3,
#' cost=500,costSoe=85,holdingrate=0.2,na.rm=TRUE)








CSOE<- function(quantity,demand,standerddeviation,leadtimeinweeks,cost,costSoe,holdingrate,na.rm=TRUE){

  DL<- demand* leadtimeinweeks/52
  sigmadl<- standerddeviation *sqrt(leadtimeinweeks/52)
  holdingcost<- holdingrate*cost
  condition<- (demand*costSoe)/(holdingcost*quantity*sigmadl*sqrt(2*pi))
  k<- sqrt(2*log(condition))
  s<- DL+sigmadl*k
  return(data.frame(demandleadtime=DL,sigmadl=sigmadl,condition=condition,k=k,min= s))
}




