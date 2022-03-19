
#' safteystock_CSL_normal
#'
#' calculating saftey stock based on cycle service level rate.
#'
#' calculating saftey stock and expected unit short based on the cycle service identified assuming a normal distribution.
#'
#' @param  rate, cycle service level requested.
#' @param quantity quantity ordered every cycle.
#' @param demand numeric,  expected annual demand of the SKU.
#'
#' @param  standerddeviation numeric annual standard deviation of the demand.
#' @param leadtime numeric,leadtime of order in weeks.
#' @param na.rm logical with a default of TRUE
#' @importFrom stats dnorm
#' @importFrom stats lm
#' @importFrom stats median
#' @importFrom stats optim
#' @importFrom stats optimize
#' @importFrom stats pnorm
#' @importFrom stats ppois
#' @importFrom stats predict
#' @importFrom stats qnorm
#' @return a dataframe that contains calculations of the expected profit from a newsvendor model based on normal distribution.
#' @author "haytham omar  email: <haytham@rescaleanalytics.com>"
#' @export
#' @examples
#' safteystock_CSL_normal(rate=0.95,quantity=30000,demand=28000,standerddeviation=5000,8,na.rm=TRUE)








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

