
#' safteystock_CIS_normal
#'
#' Calculating K value that reduces cost per item short.
#'
#' Calculating K value that reduces cost per item short inventory metric based on an assumed normal distribution.
#' @param  quantity, numeric,quantity replinished every cycle.
#' @param demand numeric,annual Expected  demand of the SKU .
#' @param standerddeviation numeric,  standard  deviation of the SKU during season.
#'
#' @param  leadtimeinweeks  leadtime in weeks or order.
#' @param  cost  numeric,cost of the SKU
#' @param  holdingrate  numeric,,holding charge per item per year.
#' @param  Citemshort numeric, peanlity cost of not satisfying demand if any, if not, zero is placed in the argument.
#' @param na.rm Logical, True to remove na.
#'
#'
#' @return a dataframe that contains calculations of K the cost per item short metric noting that condition must me less than 1.
#'
#'@import stats
#'@author "haytham omar  email: <haytham@rescaleanalytics.com>"
#' @note this is the second version of the inventorize package, all the fucntions are  without
#' any academic contribution from my side, the aim is to facilitate and ease much of the bookkeeping that is endured during stock analysis.
#' @export
#' @examples
#' safteystock_CIS_normal(quantity=3000,demand=50000,standerddeviation=4000,
#' leadtimeinweeks=4,cost=90,Citemshort=15,holdingrate=0.15,na.rm=TRUE)













safteystock_CIS_normal<- function(quantity,demand,standerddeviation,leadtimeinweeks,cost,Citemshort,holdingrate,na.rm=TRUE){

  DL<- demand* leadtimeinweeks/52
  sigmadl<- standerddeviation *sqrt(leadtimeinweeks/52)
  holdingcost<- holdingrate*cost
  condition<- (quantity*holdingcost)/(demand*Citemshort)
  Xpro<-1-condition
  k<- qnorm(Xpro)
  gk<- dnorm(k,0,1)-(k*(1-pnorm(k)))
  eus<- gk*sigmadl
  safteystock<- sigmadl*k
  s<- DL+sigmadl*k
  return(data.frame(DL=DL,sigmadl=sigmadl,condition=condition,k=k,gk=gk,eus=eus,safteystock=safteystock,min=s,Cycleservicelevel=Xpro))
}








