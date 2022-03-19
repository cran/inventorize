


#' Multi_Competing_optimization
#'
#' Calculating the optimum price based on consumer choice model for products that competes with each other.
#'
#' for multiple products that are offered , some of these products compete with each other. for example; Beef, chicken and lamb. each of them provides
#' a certain value to consumer and are offered with different prices. this function calculates the intrinsic utility value -what is the perceived value of this product to the consumer- for competing products and optimize
#' thee price of each product accordingly. please note that the more the products you put in the model, the more processing time it will take due to complexity of optimization problem.it is recommended to
#' maximum of 8 products to your model.
#'
#' @param  X  a data frame of product prices at every event.
#' @param  y  integer vector with choices of a customer at each event , for example if the
#'  competing products are only three , the possible choices are NA,1,2,3. NA being a consumer did not buy any thing at this event and he chose to walk away.
#' @param n_variables  Number of products competing with each other.
#' @param initial_products_cost  a vector of current costs for each product,for example if we have three products , it could be c(1.8,2.5,3.9).or
#' if there is no costs , it would be c(0,0,0)
#'
#'
#' @importFrom  plotly ggplotly
#' @import ggplot2
#' @importFrom magrittr %>%
#' @import tidyr
#' @importFrom stats dnorm
#' @importFrom stats lm
#' @importFrom stats median
#' @importFrom stats optim
#' @importFrom stats optimize
#' @importFrom stats pnorm
#' @importFrom stats ppois
#' @importFrom stats predict
#' @importFrom stats qnorm

#' @return a data frame with the product names which are names of X,the intrinsic utility value,the current cost and the optimized price for each product
#' @author "haytham omar  email: <haytham@rescaleanalytics.com>"
#' @export
#' @examples
#'Multi_Competing_optimization(X= data.frame(Chedar_Cheese= runif(100,10,15),
#' Mozarella=runif(100,8,10),
#'Parmesan=runif(100,9,12)),y= as.numeric(rep(c(1,2,3,NA,2),20)),n_variables = 3,
#'initial_products_cost = c(8,6,7))



Multi_Competing_optimization<- function(X,y,n_variables,initial_products_cost){
data<-cbind(X,y)
prices<- sapply(X,max)
costs <-initial_products_cost
initial_value=apply(X[1,],1,mean,na.rm=TRUE)
n_variables=n_variables
variables= rep(initial_value,n_variables)

multi_revenue_function<- function(X,y,initial_value,n_variables){

initial_value=initial_value
n_variables=n_variables
variables= rep(initial_value,n_variables)



y=data[["y"]]

f<-function(variables){


summed<-data.frame(matrix(NA,nrow = nrow(data),ncol = ncol(X)))

for (i in 1:nrow(summed)){
  for (j in 1:length(variables)){
    summed[i,j]<-exp(variables[j]-X[i,j])
  }
}
summed$sum<- apply(summed[,1:ncol(X)],1,sum)

data$loglikebottom<-log(1+summed$sum,exp(1))

offset_func<- function(variables,data,rows,x,y){
  a<-if(is.na(x)==TRUE){
    0
  } else {
    variables[x]-data[rows,y]
  }
  return(as.vector(a))
}

data$dummy<-NA
for (i in 1:nrow(data)){
  data$dummy[i]<- offset_func(variables,data,i,y[i],y[i])[[1]]
}

objective_function<- sum(data$dummy)-sum(data$loglikebottom)
return(objective_function)
}

y1<- optim(variables,f,method = 'L-BFGS-B',control = list(fnscale=-1,trace=5),lower = 0,upper = max(X)*2)
return(y1)
}


p<-multi_revenue_function(X,y,initial_value,n_variables)
values<-p$par

f1<- function(prices){
costs<- costs
r<- prices-costs
v<- exp(values-prices)
prob<- v/(1+sum(v))
profit<- prob*r

total_profit<- sum(profit)
return(total_profit)
}
prices1<-colMeans(X,na.rm = TRUE)
y2<- optim(prices1,f1,method = 'L-BFGS-B',control = list(fnscale=-1,trace=5),lower = 0,upper = max(prices)*1.5)
final_data<-data.frame(Product_name=names(X),utitliy_of_product=p$par,optimized_prices=y2$par,cost=costs)
print(plotly::ggplotly(final_data %>% tidyr::pivot_longer(-Product_name,names_to="Value",values_to="Measure") %>%
  ggplot(aes(x=Product_name,y=Measure,fill=Value))+geom_col(position = "dodge")+
    theme_classic()+ggtitle("Value/Cost/Optimum price for competing products")+coord_flip()+theme(plot.title = element_text(hjust = 0.5))))

return(final_data)
}









