

#' profit_max
#'
#' maxmizing profit based on chage in price and elasticity.
#'
#' This function is helpful to determine the elasticity of a product with effect to price change, the figure could be negative as the change is price is negative.
#' it translates as for one currency unit change in price, this much is ecpected in units in increase of sales. condition must be that Price in period one was more than price
#' in period 2 and sales in period two was more than sales in period 1. a proposed price is given to period 3 which is future period to maxmize profit. it is advisable that elasticity to
#' be calibrated by testing it on several periods. this function does not take into account advertising and campaigns,i.e external factors. yet it's a good indicator of best pricing per SKU.
#' @param  cost, numeric, cost of the SKU.
#' @param  salesP1, integer, unit sales in period 1.
#' @param salesP2 integer unit sales in period 2.
#' @param priceP1 numeric,  average price of sku in period 1.
#' @param  priceP2 average price of sku in period 2.
#'
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
#' @return the elasticity ratio in unit sales, the -ve number represents the increase in sales for each decrease of unit currency.
#' @author "haytham omar  email: <haytham@rescaleanalytics.com>"
#' @export
#' @examples
#' profit_max(cost=2,salesP1=50,salesP2=100,priceP1=6,priceP2=4)




profit_max<- function(cost,salesP1,salesP2,priceP1,priceP2,na.rm=TRUE){


  profitP3<- function(cost,discount,elasticity,salesP1,salesP2,priceP1,priceP2){

    profitP1<-salesP1*(priceP1-cost)
    profitP2<-salesP2*(priceP2-cost)
    revenueP1<- salesP1*priceP1
    revenueP2<- salesP2*priceP2
    newprice<- priceP2*(1+discount)
    change<- newprice-priceP2
    salesP3<- salesP2*(1+elasticity*discount)
    x<- salesP3*(newprice-cost)
    return(x)
  }

  elasticity<- ((salesP2-salesP1)/salesP1)/((priceP2-priceP1)/priceP1)
  opt<-optimize(profitP3,c(-2,2),tol = 0.0001,maximum = TRUE,cost=cost,elasticity=elasticity,salesP1=salesP1,salesP2=salesP2,priceP1=priceP1,priceP2=priceP2)
  revenueP1<- salesP1*priceP1
  revenueP2<- salesP2*priceP2
  profitP1<-salesP1*(priceP1-cost)
  profitP2<-salesP2*(priceP2-cost)
  proposed_discount<-opt$maximum
  projected_profit<-opt$objective
  newprice<-priceP2*(1+proposed_discount)
  changeinprice<-newprice-priceP2
  salesP3<- salesP2*(1+elasticity*proposed_discount)
  increase_in_sales<-salesP3-salesP2
  projected_revenue<- salesP3*newprice
  profit_max<-data.frame(cost=cost,salesP1=salesP1,salesP2=salesP2,priceP1=priceP1,priceP2=priceP2,profitP1=profitP1,profitP2=profitP2,revenuep1=revenueP1,revenuep2=revenueP2,elasticity=elasticity,proposed_discount=proposed_discount,changeinprice=changeinprice,newprice=newprice,
                         change_in_sales=increase_in_sales,salesP3=salesP3,revenueP3=projected_revenue,projected_profit=projected_profit)
  return(profit_max)
}



