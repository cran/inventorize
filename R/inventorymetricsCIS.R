

#' inventorymetricsCIS
#'
#' calculating inventory metrics based on cost per item short.
#'
#' after cost per item short is explicitly  calculated, item fill rate, cost per stock out event and cycle service level
#' are implicitly calculated.
#'
#' @param   CIS numeric,cost per item short determined by management
#' @param demand numeric,annual demand of the SKU.
#' @param standerddeviation numeric, annual standard  deviation
#' @param  quantity, numeric,quantity replinished every cycle.
#' @param  leadtime, numeric,leadtime in weeks
#' @param  cost, numeric cost of the SKU
#' @param  holdingrate ,numeric, holding rate per item/year
#' @param na.rm A logical indicating whether missing values should be removed
#' @importFrom stats dnorm
#' @importFrom stats lm
#' @importFrom stats median
#' @importFrom stats optim
#' @importFrom stats optimize
#' @importFrom stats pnorm
#' @importFrom stats ppois
#' @importFrom stats predict
#' @importFrom stats qnorm
#' @author "haytham omar  email: <haytham@rescaleanalytics.com>"
#' @note this is the second version of the inventorize package, all the fucntions are  without
#' any academic contribution from my side, the aim is to facilitate and ease much of the bookkeeping that is endured during stock analysis.
#' @return a dataframe that contains demand leadtime,sigmadl(standard deviation in leadtime),saftey factor k determined
#' based on cost per itemshort,unit normal loss function,expected units to be short,cycle service level, fill rate,implied cost
#' per stockout event, saftey stock and suggested reorder point.
#' @export
#' @examples
#' inventorymetricsCIS(CIS= 90, demand= 35000,standerddeviation=9000,
#' quantity= 9000,leadtime=3 ,cost=90,holdingrate=0.15,na.rm =TRUE)





inventorymetricsCIS<- function(CIS,demand,standerddeviation,quantity,leadtime,cost,holdingrate,na.rm=TRUE){
  DL<- demand* leadtime/52
  sigmadl<- standerddeviation *sqrt(leadtime/52)
  holdingcost<- holdingrate*cost
  condition<- (quantity*holdingcost)/(demand*CIS)
  Xpro<-1-condition
  k<- qnorm(Xpro)
  csl<-pnorm(k)
  gk<- dnorm(k,0,1)-(k*(1-pnorm(k)))
  eus<- gk*sigmadl
  safteystock<- sigmadl*k
  reorder_point<- DL+sigmadl*k
  CSOE<- exp(k^2/2)*(holdingcost*quantity*sigmadl*sqrt(2*pi))*(1/demand)
  fillrate<- 1- (eus/quantity)
  return(data.frame(DL=DL,sigmadl=sigmadl,k=k,gk=gk,eus=eus,csl=csl,fillrate=fillrate,
                    CIS=CIS,CSOE=CSOE,safteystock=safteystock,reorder_point=reorder_point))
}


