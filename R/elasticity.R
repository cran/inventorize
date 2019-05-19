

#' elasticity
#'
#' calculating elasticity of price change.
#'
#' This function is helpful to determine the elasticity of a product with effect to price change, the figure could be negative as the change is price is negative.
#' it translates as for each unit percentage  decrease in price , this much is ecpected precentage of  increase of sales. condition must be that Price in period one was more than proce
#' in period 2 and sales in period two was more than sales in period 1.
#'
#' @param  salesP1, integer, unit sales in period 1.
#' @param salesP2 integer unit sales in period 2.
#' @param priceP1 numeric,  average price of sku in period 1.
#'
#' @param  priceP2 average price of sku in period 2.

#' @return the elasticity ratio in unit sales, the -ve number represents the increase in sales for each decrease of unit currency.
#' @author "haytham omar  email: "<haytham@rescaleanalytics.com>"
#' @note this is the second version of the inventorize package, all the fucntions are  without
#' any academic contribution from my side, the aim is to facilitate and ease much of the bookkeeping that is endured during stock analysis.
#' @export

#' @examples
#' elasticity(salesP1=50,salesP2=100,priceP1=6,priceP2=4)




elasticity<- function(salesP1,salesP2,priceP1,priceP2){

  elasticity<- ((salesP2-salesP1)/salesP1)/((priceP2-priceP1)/priceP1)

  return(elasticity)
}




