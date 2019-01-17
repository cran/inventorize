

#' safteystock_CSL_normal
#'
#' calculating saftey stock based on cycle service level rate.
#'
#' calculating saftey stock and expected unit short based on the cycle service identified assuming a normal distribution.
#'
#' @param  rate, cycle service level requested.
#' @param quantity quantity ordered every cycle.
#' @param demand numeric,  expected weekly demand of the SKU.
#'
#' @param  standerddeviation numeric weekly standard deviation of the demand.
#' @param leadtime numeric,leadtime of order in weeks.
#' @param na.rm logical with a default of TRUE

#' @import stats
#' @return a dataframe that contains calculations of the expected profit from a newsvendor model based on normal distribution.
#' @author "haytham omar  email: <h.omar5942@gmail.com>"
#' @note this is the first version of the inventorize package, all the fucntions are common knowlege for supply chain without
#' any academic contribution from my side, the aim is to facilitate and ease much of the bookkeeping that is endured during stock analysis.
#'the package relies heavily on the studies that I had in the MIT micromasters program for supply chain.

#' @export
#' @examples
#' safteystock_CSL_normal(0.95,30000,28000,5000,8,na.rm=TRUE)








safteystock_CSL_normal<-function(rate,quantity,demand,standerddeviation,leadtime,na.rm=TRUE){
  Dl<- demand * (leadtime/52)
  sigmadL<-standerddeviation*sqrt(leadtime/52)
  k<-qnorm(rate)
  gk<- dnorm(k,0,1)-(k*(1-pnorm(k)))
  eus<- gk*sigmadL
  fillrate<- 1- (eus/quantity)
  safteystock<- sigmadL*k
  s<- Dl+sigmadL*k
  return(data.frame(k=k,gk=gk,Dl=Dl,sigmadL=sigmadL,eus=eus,min= s,safteystock=safteystock,fillrate=fillrate,cycleservicelevel=rate))
}

