


#' Hibrid_pois
#'
#' Hibrid Policy Poisson distribution service level,  .
#'
#' The Function takes a demand vector, mean of demand ,sd,lead time and 
#' requested service level to simulate and inventory system, 
#' orders are lost if inventory level is less than requested demand, also ordering is made at
#'  day t+1, metrics like item fill rate and cycle service level are calculated.
#'  the order up to level is calculated based on the review period,lead time and normal distribution.
#'  Inventory is replenished if inventory position is below min or it is time for review period.
#' @param  demand  A vector of demand in N time periods.
#' @param  lambda  rate of demand in N time periods.
#' @param service_level  cycle service level requested
#' @param leadtime  lead time from order to arrival
#' @param shortage_cost  shortage cost per unit of sales lost
#' @param Review_period  the period where the ordering happens.
#' @param min  min quantity for order up to level,if FALSE, then calculated automatically.
#' @param inventory_cost  inventory cost per unit.
#' @param ordering_cost  ordering cost for every time an order is made.

#' @import stats

#' @return a list of two date frames, the simulation and the metrics.
#' @author "haytham omar  email: <haytham@rescaleanalytics.com>"
#' @export
#' @examples
#' 
#'Hibrid_pois(demand=rpois(80,6),lambda=4,leadtime=5,service_level=0.65,
#'Review_period =9,min=30,
#'shortage_cost= FALSE,inventory_cost=FALSE,ordering_cost=FALSE)




Hibrid_pois<- function(demand,leadtime,service_level,lambda,Review_period,min=FALSE,
                         shortage_cost= FALSE,inventory_cost=FALSE,
                         ordering_cost=FALSE){
  
  mu = lambda
  L = leadtime
  Max= qpois(service_level,lambda)*(leadtime+Review_period)
  saftey_stock= Max- (lambda*(leadtime+Review_period))                 
  
  
  min<- if (min==FALSE){
    min= qpois(service_level,lambda)*leadtime
  }else {
    min
  }
  
  N = length(demand) 
  
  order = rep(NA,N+1) 
  I = rep(NA,N+1)
  IP = rep(NA,N+1)
  sales = rep(NA,N+1)
  recieved = rep(NA,N+1)
  Max= rep(Max,N+1)
  IP[1] = I[1] = Max[1]
  order[1]=0
  ordering_time<- rep(rep(c(0,1),c(Review_period-1,1)),length(demand))
  demand<-c(0,demand)
  for(t in 2: (L)){
    sales[t] <- min (demand[t], I[t-1])
    I[t] <- I[t-1] - sales[t]
    order[t] <- if((IP[t-1] <= min)){
      (Max[t]- IP[t-1]) * (IP[t-1] <= min)
    }else {
      (Max[t]- IP[t-1]) * (ordering_time[t])}
    
    IP[t] <-  IP[t-1] + order[t] - sales[t]
    
  }
  
  for (t in seq((L+1),(N))){
    sales[t] = min(demand[t], I[t-1] + order[t-L])
    I[t] = I[t-1] + order[t-L] - sales[t]
    order[t] = if((IP[t-1] <= min)){
      (Max[t]- IP[t-1]) * (IP[t-1] <= min)
    }else {
      (Max[t]- IP[t-1]) * (ordering_time[t])}
    
    IP[t] = IP[t-1] + order[t] - sales[t]
    recieved[t]<- order[t-L]
  }
  
  data<-data.frame(period= seq(1:(N+1)),demand=demand,sales=sales,inventory_level=I,
                   inventory_position=IP,min=min,review= Review_period,Max=Max,order= order,
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







