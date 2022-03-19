
#' EPN_singleperiod
#'
#' calculating expected profit for a newsvendor model.
#'
#' calculating expected profit for a newsvendor model. based on assumed normal distribution demand.
#'
#' @param  quantity, numeric,quantity replinished every cycle.
#' @param mean numeric,Expected  demand of the SKU during season.
#' @param standerddeviation numeric,  standard  deviation of the SKU during season.
#'
#' @param  p  numeric,selling price of the SKU
#' @param  c  numeric,cost of the SKU
#' @param  g  numeric,,salvage or discounted value if sold after season,if there is no salvage , zero is placed in the argument.
#' @param  b numeric, peanlity cost of not satisfying demand if any, if not, zero is placed in the argument.
#' @param  na.rm A logical indicating whether missing values should be removed
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
#'

#' @author "haytham omar  email: <haytham@rescaleanalytics.com>"
#' @note this is the second version of the inventorize package, all the fucntions are  without
#' any academic contribution from my side, the aim is to facilitate and ease much of the bookkeeping that is endured during stock analysis.
#' @export
#' @examples
#' EPN_singleperiod(quantity=40149,mean= 32000,standerddeviation= 11000,p=24,c=10.9,g=7,b=0,na.rm=TRUE)




EPN_singleperiod<-function(quantity,mean,standerddeviation,p,c,g,b,na.rm=TRUE){

  k<- (quantity-mean)/standerddeviation
  gk<- dnorm(k,0,1)-(k*(1-pnorm(k)))
  eus<- gk*standerddeviation
  expectedprofit<- (p-g)*mean-(c-g)*quantity-(p-g+b)*eus
  expectedcost<-(c-g)*quantity
  expectedshortagecost<-(p-g+b)*eus
  expectedrevnue<-(p)*mean
  e_sold_fullprice<- mean-eus
  sold_discount<-quantity-(mean-eus)
  return(data.frame(quantity=quantity,demand=mean,sd=standerddeviation,unitshort=eus,shortagecost=expectedshortagecost
                    ,cost=expectedcost,revenue=expectedrevnue,
                    profit=expectedprofit,soldatfullprice=e_sold_fullprice,sold_discount=sold_discount))
}



