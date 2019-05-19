

#' profit_max_withfixedcost
#'
#' maxmizing profit based on chage in price and elasticity taking into consideration fixed and variable costs.
#'
#' This function is helpful to determine the elasticity of a product with effect to price change, the figure could be negative as the change is price is negative.
#' it translates as for one currency unit change in price, this much is ecpected in units in increase of sales. condition must be that Price in period one was more than price
#' in period 2 and sales in period two was more than sales in period 1. a proposed price is given to period 3 which is future period to maxmize profit. it is advisable that elasticity to
#' be calibrated by testing it on several periods. this function does not take into account advertising and campaigns,i.e external factors. yet it's a good indicator of best pricing per SKU.
#' @param  fixed_cost, numeric, fixed cost for ordering and handling the SKU.
#' @param  variable_cost, numeric, the cost of  the SKU, changing by quantity.
#' @param  salesP1, integer, unit sales in period 1.
#' @param salesP2 integer unit sales in period 2.
#' @param priceP1 numeric,  average price of sku in period 1.
#' @param  priceP2 average price of sku in period 2.
#'
#'
#' @import stats
#' @return the elasticity ratio in unit sales, the -ve number represents the increase in sales for each decrease of unit currency.
#' @author "haytham omar  email: <haytham@rescaleanalytics.com>"
#' @export
#' @examples
#' profit_max_withfixedcost(fixed_cost=200,variable_cost=20,salesP1=50,salesP2=100,priceP1=6,priceP2=4)




profit_max_withfixedcost<-function(fixed_cost,variable_cost,salesP1,salesP2,priceP1,priceP2) {


  profitP3<- function(fixed_cost,variable_cost,discount,elasticity,salesP1,salesP2,priceP1,priceP2){

    profitP1<-(salesP1*(priceP1-variable_cost))-fixed_cost
    profitP2<-(salesP2*(priceP2-variable_cost))-fixed_cost
    revenueP1<- salesP1*priceP1
    revenueP2<- salesP2*priceP2
    newprice<- priceP2*(1+discount)
    change<- newprice-priceP2
    salesP3<- salesP2*(1+elasticity*discount)
    x<- (salesP3*(newprice-variable_cost))-fixed_cost
    return(x)
  }

  elasticity<- ((salesP2-salesP1)/salesP1)/((priceP2-priceP1)/priceP1)
  opt<-optimize(profitP3,c(-2,2),tol = 0.0001,maximum = TRUE,fixed_cost=fixed_cost,variable_cost=variable_cost,elasticity=elasticity,salesP1=salesP1,salesP2=salesP2,priceP1=priceP1,priceP2=priceP2)
  revenueP1<- salesP1*priceP1
  revenueP2<- salesP2*priceP2
  profitP1<-(salesP1*(priceP1-variable_cost))-fixed_cost
  profitP2<-(salesP2*(priceP2-variable_cost))-fixed_cost
  proposed_discount<-opt$maximum
  projected_profit<-opt$objective
  newprice<-priceP2*(1+proposed_discount)
  changeinprice<-newprice-priceP2
  salesP3<- salesP2*(1+elasticity*proposed_discount)
  increase_in_sales<-salesP3-salesP2
  projected_revenue<- salesP3*newprice
  profit_max<-data.frame(fixed_cost=fixed_cost,variable_cost=variable_cost,salesP1=salesP1,salesP2=salesP2,priceP1=priceP1,priceP2=priceP2,profitP1=profitP1,
  profitP2=profitP2,revenuep1=revenueP1,revenuep2=revenueP2,elasticity=elasticity,proposed_discount=proposed_discount,changeinprice=changeinprice,newprice=newprice,
  change_in_sales=increase_in_sales,salesP3=salesP3,revenueP3=projected_revenue,projected_profit=projected_profit)
  return(profit_max)
}



