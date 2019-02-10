
#' MPP_singleperiod
#'
#' Maximum profit from a newsvendor model based on a poisson distribution.
#'
#' calculating expected profit for a newsvendor model. based on assumed poisson distribution demand based on the critical ration.
#'

#'
#' @param lambda numeric,  mean of the demand based on poisson distribution.
#'
#' @param  p  numeric,selling price of the SKU
#' @param  c  numeric,cost of the SKU
#' @param  g  numeric,,salvage or discounted value if sold after season,if there is no salvage , zero is placed in the argument.
#' @param  b numeric, peanlity cost of not satisfying demand if any, if not, zero is placed in the argument.
#' @param  na.rm A logical indicating whether missing values should be removed
#' @import stats

#' @return a dataframe that contains calculations of the maximum expected profit from a newsvendor model based on poisson distribution.
#' @author "haytham omar  email: <haytham@rescaleanalytics.com>"
#' @note this is the second version of the inventorize package, all the fucntions are  without
#' any academic contribution from my side, the aim is to facilitate and ease much of the bookkeeping that is endured during stock analysis.
#' @export
#' @examples
#' MPP_singleperiod(lambda= 32000,p=24,
#' c=10.9,g=7,b=0,na.rm=TRUE)








MPP_singleperiod<-function(lambda,p,c,g,b,na.rm=TRUE){

  CR<- (p-c+b)/(p-c+b+c-g)
  quantity<- qpois(CR,lambda)
  eus<- lambda-(lambda*ppois(quantity-1,lambda))-quantity*(1-ppois(quantity,lambda))
  expectedprofit<- (p-g+b)*(quantity*ppois(quantity-1,lambda))- (quantity*ppois(quantity,lambda))
  + (p-c+b)*quantity-(b*lambda)
  expectedunitsold<- lambda*ppois(quantity-1,lambda)+quantity*(1-ppois(quantity,lambda))
  CDF<- CR
  return(data.frame(quantity=quantity,lambda=lambda,lost_sales=eus,
                    expected_sales=expectedunitsold,expectedprofit=expectedprofit,CDF=CDF))
}






