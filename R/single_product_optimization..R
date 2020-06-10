



#' single_product_optimization
#'
#' Calculating the optimum price based on linear and logit models for a single product.
#'
#' calculate the optimized price based on the price response function. the price response function is measured twice, one with linear model and
#' one time with a logit model. a simulation is then made with each price response function to define the maximum revenue for each.
#'  finally, a suggestion of which model to choose and the optimum price to use for this product.
#'  it is preferable to de-seasonalize the sales data before fitting if the sales
#'  are affected by spikes and declines due to regular events as holidays and weekends.
#'
#' @param  x a vector of average weekly/monthly/daily price data of a product
#' @param  y a vector of average weekly/monthly/daily sales data of a product
#' @param service_product_name the name of the product or service.
#' @param current_price the current price oof the product or service
#' @import ggplot2
#' @import stats
#' @importFrom magrittr %>%
#' @return a list of the squared error of th logit model, the squared error of the linear model, the best model for this product, the optimum
#' price for both the linear and the logit model, the current price,the a,b,c parameters of th logit model,the linear model paremeters ,
#' data simulated
#' at different price points and th expected revenue and the fitting results of both the logit and linear model.
#'
#' @author "haytham omar  email: <haytham@rescaleanalytics.com>"
#' @export
#' @examples
#' single_product_optimization(x= c(5,8,10,12),
#' y=c(25,21,23,15),
#' service_product_name = "Goat Cheese",current_price = 8.5)





single_product_optimization<- function(x,y,service_product_name,current_price){

  Measured <- as.data.frame(cbind(x, y))
  moodel<-lm(y~x,Measured)
  Measured$lm_p<-predict(moodel,Measured)
  squared_sum_lm<-sum((Measured$y - Measured$lm_p)^2)
  #initialize values
  c=max(Measured$y)
  b = -moodel$coefficients[2][[1]]*4/c
  a = -median(Measured$x)*b
  x0= c(a,b,c)
  #define function to optimise: optim will minimize the output
  f <- function(x) {

    y=0
    a=x[1]
    b=x[2]
    c=x[3]

    Predicted_y <-  c * exp(-(a+b*Measured$x)/(1+exp(-(a+b*Measured$x))))

    y = sum(( Measured$y-Predicted_y)^2)

    return(y)

  }
  optim
  #call optim: results will be available in variable Y
  Y<-optim(x0,f,control = list(maxit=100000))

  sum_squared_logit<-Y$value

  simulation_data<-data.frame(x= seq(min(Measured$x)-1.8*sd(Measured$x),max(Measured$x)+1.8*sd(Measured$x),by=0.5))

  logit1<- function(x,a=Y$par[1][1] ,b=Y$par[2][1],c=Y$par[3][1]){
    y= c * exp(-(a+b*x)/(1+exp(-(a+b*x))))
    return(y)
  }
  Measured$logit_p<-logit1(Measured$x)
  simulation_data$predicted_linear<- predict(moodel,simulation_data)
  simulation_data$predicted_logit<- logit1(simulation_data$x)
  simulation_data$revenue_linear<- simulation_data$predicted_linear *simulation_data$x
  simulation_data$revenue_logit<- simulation_data$predicted_logit *simulation_data$x


  best_model<- ifelse(sum_squared_logit> squared_sum_lm,"Linear model","Logit model")

  best_price_linear<- simulation_data$x[simulation_data$revenue_linear==max(simulation_data$revenue_linear)]
  best_price_logit<-simulation_data$x[simulation_data$revenue_logit==max(simulation_data$revenue_logit)]
  print(simulation_data %>% ggplot(aes(x=x,y= predicted_linear,color='Linear response function'))+geom_line()+theme(plot.title = element_text(hjust = 0.5))+
  geom_line(aes(y=predicted_logit,color= 'logit response function'))+ggtitle(paste('Price Response Function for',service_product_name))+theme_classic())
  print(simulation_data %>% ggplot(aes(x=x,y= revenue_linear,color='revenue_linear'))+geom_line()+geom_line(aes(y=revenue_logit,color='revenue_logit'))+
          ggtitle(paste('revenue vs price for',service_product_name))+theme_classic()+
          annotate('text',label= paste('optimal p linear',round(best_price_linear)),
                   color='red',size=4,x=best_price_linear,y= max(simulation_data$revenue_linear))+annotate('text',label=
     paste('optimal p logit is ',round(best_price_logit)),color='darkgreen',size=4,
    x=best_price_logit,y= max(simulation_data$revenue_logit))+theme(plot.title = element_text(hjust = 0.5))+geom_vline(xintercept = current_price[[1]],linetype='dashed')+xlab("Price")+ylab("Response"))



  print(Measured %>% ggplot(aes(x= x,y=y,color='actual observations'))+geom_point()+
          geom_line(aes(y=lm_p,color='linear fitting'))+
          geom_line(aes(y=logit_p,color='Logit fitting'))+
          theme_classic()+ggtitle(paste('fitting of linear and logit for',service_product_name))+ylab('Average sales')+xlab("Price")+
          annotate("text",label=paste("Best fitted is",best_model),size=3,x= mean(x),y=max(y))+theme(plot.title = element_text(hjust = 0.5)))
  all_data<-list(optimization_paremeters=Y,lm_model=moodel,squared_error_logit=print(paste('squared_error_logit= ',sum_squared_logit)),
                 squared_error_linear=print(paste('squared_errorr_lm= ',squared_sum_lm)),simulation_data=simulation_data,best_model=print(paste('best_model is',
                        best_model,'for',service_product_name)),optimum_linear=print(paste('optimum linear price is',best_price_linear,'for',service_product_name)),
                 optimum_logit=print(paste('optimum logit price is',best_price_logit,'for',service_product_name)),
                 current_price=print(paste('current price is ',current_price)),article_name=paste('article name is',service_product_name),predixtions=Measured)
  return(all_data)
}



