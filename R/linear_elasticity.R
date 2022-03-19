


#' linear_elasticity
#'
#' calculating elasticity of a linear price response function
#'  This function is helpful to determine if your product is elastic or not based on a linear price response function. if product demand is
#'  not linear to price, try using the single product optimization function instead. The price elasticity of demand
#'  which is often shortened to demand elasticity
#'  is defined to be the percentage change in quantity demanded, q, divided by the percentage change in price, p.
#'  When Elasticity bigger 1, we say the good is price elastic.In this case, percentQ bigger percentP, and so, for a 1 percent change in price, there 
#'  is a greater than 1 percent
#'  change in quantity demanded.In this case, management should decrease price to have a higher revenue.
#'  When Elasticity smaller 1, we say the good is price inelastic.In this case, percentQ smaller percentP, and so, for a 1 percent change in price, there 
#'  is a less than 1 percent change
#' in quantity demanded.In this case, management should increase price to have a higher revenue.
#'  When Elasticity equal 1, we say the good is price unit elastic.In this case, percentQ equal percentP , and so, for a 1percent change in price,
#'  there is also an 1percent change in quantity demanded.
#'  This is the optimal price which means it maximizes revenue.
#'
#' @param  prices  vector of prices.
#' @param  Sales  Vector of sales against each price .
#' @param  present_price  numeric,  present price of the product .
#' @param  cost_of_product  cost of the product, if the product/service has no cost ,then cost is set to zero.
#' @param  plot  Default is false,if true, a plot is generated
#' @importFrom stats dnorm
#' @importFrom stats lm
#' @importFrom stats median
#' @importFrom stats optim
#' @importFrom stats optimize
#' @importFrom stats pnorm
#' @importFrom stats ppois
#' @importFrom stats predict
#' @importFrom stats qnorm
#' @import ggplot2
#' @importFrom magrittr %>%
#' @import tidyr
#' @importFrom  plotly ggplotly
#' @return the elasticity at the present price , the price for optimum revenue and thee price for optimum cost.
#' @author "haytham omar  email: "<haytham@rescaleanalytics.com>"
#' @note this is the third version of the inventorize package, all the functions are  without
#' any academic contribution from my side, the aim is to facilitate and ease much of the bookkeeping that is endured during stock analysis.
#' @export
#' @examples
#' linear_elasticity(prices=c(5,10,8,5,14),Sales= c(450,400,420,450,360),
#' present_price=15,cost_of_product=40)








linear_elasticity<-function(prices,Sales,present_price,cost_of_product,plot=FALSE){
  data<-data.frame(cbind(prices,Sales))
  lm_model<-lm(Sales~prices,data = data)
  intercept<- lm_model$coefficients[1][[1]]

  derv_p= lm_model$coefficients[2][[1]]
  Elasticity1 = ((-1 *present_price)/(intercept+present_price*derv_p))*derv_p

  optimum_profit<-(-derv_p*cost_of_product+ intercept)/(2*-derv_p)
  optimum_revenue<- intercept/(2*-derv_p)
  simulation_data<-data.frame(prices= seq(min(prices),max(optimum_profit,optimum_revenue)*1.3,1))
  simulation_data$Sales<- predict(lm_model,simulation_data)
  simulation_data$revenue<-simulation_data$Sales*simulation_data$prices
  simulation_data$profit<- (simulation_data$prices*simulation_data$Sales)   -(simulation_data$Sales*cost_of_product)

  if(plot==TRUE){
  print(plotly::ggplotly(simulation_data %>% ggplot(aes(x=prices,y=revenue,color='revenue'))+geom_line()+geom_line(aes(y=profit,color='profit'))+theme_classic()+
          geom_vline(xintercept = present_price[[1]],linetype='dashed',color='darkblue')+
          geom_vline(xintercept = optimum_profit,linetype='dashed',color='red')+ geom_vline(xintercept = optimum_revenue,linetype='dashed',color='turquoise')
          +ggtitle('Linear Elasticity Optimization')+theme(plot.title = element_text(hjust = 0.5))))
  }
data_final<-data.frame(Elasticity=Elasticity1,optimum_price_profit=optimum_profit,optimum_price_revenue=optimum_revenue)
data_final$Elasticity<- as.numeric(data_final$Elasticity)

data_final<-data_final %>% dplyr::mutate(comment= dplyr::case_when(Elasticity==1 ~ "Prduct is unit elastic, revenue is max for this point.
        In this case, percent changeQ = percent changeP , and so, for a 1percent  change in price, there is also an 1percent  change in quantity demanded.",
     Elasticity < 0.99999 ~ "Product is inelastic, in this case percent changeQ < percent changeP, and so, for a 1 percent  change in price, there is a less than 1 percent  change
in quantity demanded. ",Elasticity > 1.01 ~ "Product is elastic,in this case ,percent changeQ > percent changeP, and so, for a 1 percent  change in price, there is a greater than 1 percent 
change in quantity demanded."))
data_final$comment[is.na(data_final$comment)==TRUE]<-"Prduct is unit elastic, revenue is max for this point.
        In this case, percent changeQ = percent changeP , and so, for a 1percent  change in price, there is also an 1percent  change in quantity demanded."


  return(data_final)

}







