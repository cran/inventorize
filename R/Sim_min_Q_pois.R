

#' sim_min_Q_pois
#'
#' Simulating a Min,Q policy or also calleD S,Q policy,  .
#'
#' The Function takes a demand vector, mean of demand ,sd,lead time and requested service level to simulate and inventory system, 
#' orders are lost if inventory level is less than requested demand, also ordering is made at
#'  day t+1, metrics like item fill rate and cycle service level are calculated. the min is calculated based on a normal distribution.
#' @param  demand  A vector of demand in N time periods.
#' @param  lambda  rate of demand in N time periods.
#' @param leadtime  lead time from order to arrival
#' @param shortage_cost  shortage cost per unit of sales lost
#' @param Quantity  Fixed order quantity to be ordered at min
#' @param service_level  cycle service level requested
#' @param inventory_cost  inventory cost per unit.
#' @param ordering_cost  ordering cost for every time an order is made.
#' @importFrom stats dnorm
#' @importFrom stats lm
#' @importFrom stats median
#' @importFrom stats optim
#' @importFrom stats optimize
#' @importFrom stats pnorm
#' @importFrom stats ppois
#' @importFrom stats predict
#' @importFrom stats qnorm
#' @return a list of two date frames, the simulation and the metrics.
#' @author "haytham omar  email: <haytham@rescaleanalytics.com>"
#' @export
#' @examples
#' sim_min_Q_pois(demand = rpois(50,8),lambda = 4,leadtime = 4,shortage_cost =30,
#' Quantity = 12,service_level = 0.70,
#' inventory_cost = 50,ordering_cost=FALSE)


sim_min_Q_pois<- function(demand,lambda,leadtime,service_level,Quantity,
                            shortage_cost= FALSE,inventory_cost=FALSE,
                          ordering_cost=FALSE){
  
  mu = lambda
  L = leadtime
  Q =Quantity
  min= qpois(service_level,lambda)*leadtime
  saftey_stock= min- (lambda*leadtime)                 
  
  
  
  N = length(demand) 
  
  order = rep(NA,N+1) 
  I = rep(NA,N+1)
  IP = rep(NA,N+1)
  sales = rep(NA,N+1)
  recieved = rep(NA,N+1)
  
  IP[1] = I[1] = min+Q
  order[1]=0
  demand<-c(0,demand)
  for(t in 2: (L)){
    sales[t] <- min(demand[t], I[t-1])
    I[t] <- I[t-1] - sales[t]
    order[t] <- Q * (IP[t-1] <= min)
    IP[t] <-  IP[t-1] + order[t] - sales[t]
    
  }
  
  for (t in seq((L+1),(N))){
    sales[t] = min(demand[t], I[t-1] + order[t-L])
    I[t] = I[t-1] + order[t-L] - sales[t]
    order[t] = Q * (IP[t-1] <= min)
    IP[t] = IP[t-1] + order[t] - sales[t]
    recieved[t]<- order[t-L]
  }
  
  data<-data.frame(period= seq(1:(N+1)),demand=demand,sales=sales,inventory_level=I,
                   inventory_position=IP,s= rep(min,N+1),order= order,
                   recieved=recieved)
  
  data$lost_order<- data$demand -data$sales
  message('this function is deprecated, kindly use sim_min_Q() instead')
  
  metrics<- data.frame(shortage_cost= sum(data$lost_order,na.rm = TRUE)*shortage_cost,
                       inventory_cost= sum(data$inventory_level,na.rm = TRUE)*inventory_cost,
                       ordering_cost=length(which(data$order >0))*ordering_cost,
                       average_inventory_level= mean(data$inventory_level,na.rm = TRUE),
                       total_lost_sales= sum(data$lost_order,na.rm = TRUE),
                       Item_fill_rate= 1-(sum(data$lost_order,na.rm = TRUE)/sum(demand[1:(length(demand)-1)])),
                       cycle_service_level= 1-(length(which(data$lost_order >0))/(length(demand)-1))
                       ,saftey_stock=saftey_stock)
  return(list(simu_data=data,metrics=metrics))                
  
}






