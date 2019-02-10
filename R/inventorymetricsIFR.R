

#' inventorymetricsIFR
#'
#' calculating inventory metrics based on item fillrate.
#'
#' item fill rate is the percentage of demand that is fullfilled directly from the cycle stock,after item fill rate is explicitly  calculated, cost per item short, cost per stock out event and cycle service level
#' are implicitly calculated.
#'
#' @param   fillrate numeric,required percentage of demand that is fullfilled from cycle stock
#' @param demand numeric,annual demand of the SKU.
#' @param standerddeviation numeric, annual standard  deviation
#' @param  quantity, numeric,quantity replinished every cycle.
#' @param  leadtime, numeric,leadtime in weeks
#' @param  cost, numeric cost of the SKU
#' @param  holdingrate ,numeric, holding rate per item/year
#' @param na.rm A logical indicating whether missing values should be removed
#'@import stats
#' @return a dataframe that contains
#' demand leadtime, sigmadl(standard deviation in leadtime), saftey factor k determined
#' based on item fillrate provided, unit normal loss function expected units to be short,cycle service level, fill rate,implied cost
#' per stockout event, saftey stock and suggested reorder point.
#'
#' @author "haytham omar  email: <haytham@rescaleanalytics.com>"
#' @note this is the second version of the inventorize package, all the fucntions are  without
#' any academic contribution from my side, the aim is to facilitate and ease much of the bookkeeping that is endured during stock analysis.
#' @export
#' @examples
#' inventorymetricsIFR(fillrate= 0.90, demand= 35000,standerddeviation=9000,
#' quantity= 5000,leadtime=3 ,cost=50,holdingrate=0.15,na.rm=TRUE)







inventorymetricsIFR<- function(fillrate,demand,standerddeviation,quantity,leadtime,cost,holdingrate,na.rm=TRUE){
  DL<- demand* leadtime/52
  sigmadl<- standerddeviation *sqrt(leadtime/52)
  holdingcost<- holdingrate*cost
  gk<- (quantity/sigmadl)*(1-fillrate)
  f <- function(k)  (dnorm(k,0,1)-(k*(1-pnorm(k)))-gk)
  k<-uniroot(f, lower=0.1, upper=100000000)$root
  eus<- gk*sigmadl
  csl<-pnorm(k)
  CIS<- (quantity*holdingcost)/(demand*(1-csl))
  CSOE<- exp(k^2/2)*(holdingcost*quantity*sigmadl*sqrt(2*pi))*(1/demand)
  safteystock<- k*sigmadl
  reorder_point<- k*sigmadl+DL
  return(data.frame(DL=DL,sigmadl=sigmadl,k=k,gk=gk,eus=eus,csl=csl,fillrate=fillrate,
                    CIS=CIS,CSOE=CSOE,safteystock=safteystock,reorder_point=reorder_point))
}



