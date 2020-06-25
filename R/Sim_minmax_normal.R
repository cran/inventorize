


#' sim_minmax_normal
#'
#' Simulating a Min,max policy or aslo called s,S policy,  .
#'
#' The Function takes a demand vector, mean of demand ,sd,lead time and requested service level to simulate and inventory system, 
#' orders are lost if inventory level is less than requested demand, also ordering is made at
#'  day t+1, metrics like item fill rate and cycle service level are calculated. the min is calculated based on a normal distribution.
#' @param  demand  A vector of demand in N time periods.
#' @param  mean  average demand in N time periods.
#' @param sd  standard deviation in N time periods.
#' @param service_level  cycle service level requested
#' @param leadtime  lead time from order to arrival
#' @param shortage_cost  shortage cost per unit of sales lost
#' @param Max  Max quantity for order up to level
#' @param inventory_cost  inventory cost per unit.
#' @import stats

#' @return a list of two date frames, the simulation and the metrics.
#' @author "haytham omar  email: <haytham@rescaleanalytics.com>"
#' @export
#' @examples
#' sim_minmax_normal(demand = rpois(25,8),mean = 5,sd=1,service_level = 0.5,
#' leadtime = 4,shortage_cost = 0,Max= 40,inventory_cost = 0)


sim_minmax_normal<- function(demand,mean,sd,leadtime,service_level,Max,
                            shortage_cost= FALSE,inventory_cost=FALSE){
  
  mu = mean
  L = leadtime

  min= round((mean *leadtime)+ ((sd*sqrt(leadtime))* qnorm(0.95)),digits = 0)
  saftey_stock= ((sd*sqrt(leadtime))* qnorm(0.95))                        
  
  
  
  N = length(demand) 
  
  order = rep(NA,N+1) 
  I = rep(NA,N+1)
  IP = rep(NA,N+1)
  sales = rep(NA,N+1)
  recieved = rep(NA,N+1)
  Max= rep(Max,N+1)
  IP[1] = I[1] = min+Max[1]
  order[1]=0
  demand<-c(0,demand)
  for(t in 2: (L)){
    sales[t] <- min (demand[t], I[t-1])
    I[t] <- I[t-1] - sales[t]
    order[t] <- (Max[t]- IP[t-1]) * (IP[t-1] <= min)
    IP[t] <-  IP[t-1] + order[t] - sales[t]
    
  }
  
  for (t in seq((L+1),(N))){
    sales[t] = min(demand[t], I[t-1] + order[t-L])
    I[t] = I[t-1] + order[t-L] - sales[t]
    order[t] = (Max[t]- IP[t-1]) * (IP[t-1] <= min)
    IP[t] = IP[t-1] + order[t] - sales[t]
    recieved[t]<- order[t-L]
  }
  
  data<-data.frame(period= seq(1:(N+1)),demand=demand,sales=sales,inventory_level=I,
                   inventory_position=IP,s= rep(min,N+1),Max=Max,order= order,
                   recieved=recieved)
  
  data$lost_order<- data$demand -data$sales
  
  metrics<- data.frame(shortage_cost= sum(data$lost_order,na.rm = TRUE)*shortage_cost,
                       inventory_cost= sum(data$inventory_level,na.rm = TRUE)*inventory_cost,
                       average_inventory_level= mean(data$inventory_level,na.rm = TRUE),
                       total_lost_sales= sum(data$lost_order,na.rm = TRUE),
                       Item_fill_rate= 1-(sum(data$lost_order,na.rm = TRUE)/sum(demand[1:(length(demand)-1)])),
                       cycle_service_level= 1-(length(which(data$lost_order >0))/(length(demand)-1))
                       ,saftey_stock=saftey_stock)
  return(list(simu_data=data,metrics=metrics))                
  
}







