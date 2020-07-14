


#' sim_Base_normal
#'
#' Simulating a Base Stock  policy.
#'
#' The Function takes a demand vector, mean of demand ,sd,lead time and requested service level to simulate and inventory system, 
#' orders are lost if inventory level is less than requested demand, also ordering is made at
#'  day t+1, metrics like item fill rate and cycle service level are calculated based on a 
#'  normal distribution.
#'the base is calculated automatically based on the mean demand and standard deviaiton. every period the order is exactly as the sales.
#' @param  demand  A vector of demand in N time periods.
#' @param  mean  average demand in N time periods.
#' @param sd  standard deviation in N time periods.
#' @param service_level  cycle service level requested
#' @param leadtime  lead time from order to arrival
#' @param shortage_cost  shortage cost per unit of sales lost
#' @param ordering_delay  logical,Default is FALSE,if TRUE, orders are delayed one period.
#' @param Base  Set to False for automatic calculation,else manual input of base.
#' @param inventory_cost  inventory cost per unit.
#' @param ordering_cost  ordering cost for every time an order is made.

#' @import stats

#' @return a list of two date frames, the simulation and the metrics.
#' @author "haytham omar  email: <haytham@rescaleanalytics.com>"
#' @export
#' @examples
#'sim_base_normal(demand=rpois(80,6),mean=6,sd=0.2,leadtime=5,service_level=0.95,Base = 50,
#'shortage_cost= 1,inventory_cost=1,ordering_cost=1,ordering_delay=FALSE)



sim_base_normal<- function(demand,mean,sd,leadtime,service_level,Base=FALSE,ordering_delay=FALSE,
                             shortage_cost= FALSE,inventory_cost=FALSE,
                             ordering_cost=FALSE){
  
  mu = mean
  L = leadtime
  
  min= round((mean *leadtime)+ ((sd*sqrt(leadtime))* qnorm(service_level)),digits = 0)
  saftey_stock= ((sd*sqrt(leadtime))* qnorm(service_level))                        
  
  Base<- if(Base==FALSE){
    Base=round(min+saftey_stock,0)
  }else{
    Base
  }
  N = length(demand) 
  
  order = rep(NA,N+1) 
  I = rep(NA,N+1)
  IP = rep(NA,N+1)
  sales = rep(0,N+1)
  recieved = rep(NA,N+1)
  
  IP[1] = I[1] = Base
  order[1]=0
  demand<-c(0,demand)
  for(t in 2: (L)){
    sales[t] <- min (demand[t], I[t-1])
    I[t] <- I[t-1] - sales[t]
    order[t] <- sales[t-ordering_delay]
    IP[t] <-  IP[t-1] + order[t] - sales[t]
    
  }
  
  for (t in seq((L+1),(N))){
    sales[t] = min(demand[t], I[t-1] + order[t-L])
    I[t] = I[t-1] + order[t-L] - sales[t]
    order[t] = sales[t-ordering_delay]
    IP[t] = IP[t-1] + order[t] - sales[t]
    recieved[t]<- order[t-L]
  }
  
  data<-data.frame(period= seq(1:(N+1)),demand=demand,sales=sales,inventory_level=I,
                   inventory_position=I,Base=Base,order= order,
                   recieved=recieved)
  
  data$lost_order<- data$demand -data$sales
  
  metrics<- data.frame(shortage_cost= sum(data$lost_order,na.rm = TRUE)*shortage_cost,
                       inventory_cost= sum(data$inventory_level,na.rm = TRUE)*inventory_cost,
                       average_inventory_level= mean(data$inventory_level,na.rm = TRUE),
                       ordering_cost=length(which(data$order >0))*ordering_cost,
                       total_lost_sales= sum(data$lost_order,na.rm = TRUE),
                       Item_fill_rate= 1-(sum(data$lost_order,na.rm = TRUE)/sum(demand[1:(length(demand)-1)])),
                       cycle_service_level= 1-(length(which(data$lost_order >0))/(length(demand)-1))
                       ,saftey_stock=saftey_stock)
  return(list(simu_data=data,metrics=metrics))                
  
}







