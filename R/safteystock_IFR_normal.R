
#' safteystock_IFR_normal
#'
#' Calculating K value corresponding to item fill rate.
#'
#' Calculating K value that corresponds to the desired item fill rate.
#' @param rate numeric, item fill rate.
#' @param  quantity, numeric,quantity replinished every cycle.
#' @param demand numeric,annual Expected  demand of the SKU .
#' @param standerddeviation numeric,  standard  deviation of the SKU during season.
#' @param na.rm Logical, TRUE to remove na.
#'
#' @param  leadtime  leadtime in weeks of order.
#' @importFrom stats dnorm
#' @importFrom stats lm
#' @importFrom stats median
#' @importFrom stats optim
#' @importFrom stats optimize
#' @importFrom stats pnorm
#' @importFrom stats ppois
#' @importFrom stats predict
#' @importFrom stats qnorm
#' @importFrom stats uniroot

#' @return a dataframe that contains calculations of K the  item fill rate metric.cycle service level and expected unit short.
#' @author "haytham omar  email: <h.omar5942@gmail.com>"
#' @note this is the first version of the inventorize package, all the fucntions are basic knowlege for supply chain without
#' any contribution from my side, the aim is to facilitate and ease much of the bookkeeping that is endured during stock analysis.
#' @export
#' @examples
#' safteystock_IFR_normal(rate=0.97,quantity=9000,demand=100000,
#' standerddeviation=5000,leadtime=4,na.rm=TRUE)







safteystock_IFR_normal<-function(rate,quantity,demand,standerddeviation,leadtime,na.rm=TRUE){
  Dl<- demand * (leadtime/52)
  sigmadL<-standerddeviation*sqrt(leadtime/52)
  gk<- (quantity/sigmadL)*(1-rate)
  f <- function(k)  (dnorm(k,0,1)-(k*(1-pnorm(k)))-gk)
  k<-uniroot(f, lower=0.1, upper=100000000)$root
  cycleservicelevel<-pnorm(k)
  eus<- gk*sigmadL
  safteystock<- sigmadL*k
  s<- Dl+sigmadL*k
  return(data.frame(cycleservicelevel=cycleservicelevel,fillrate=rate,gk=gk,Dl=Dl,sigmadL=sigmadL,eus=eus,min= s,safteystock=safteystock))
}











