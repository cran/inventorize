


#' Periodic_review_normal
#'
#' Simulating a Periodic order up to level policy,  .
#'
#' The Function takes a demand vector, mean of demand ,sd,lead time and 
#' requested service level to simulate and inventory system, 
#' orders are lost if inventory level is less than requested demand, also ordering is made at
#'  day t+1, metrics like item fill rate and cycle service level are calculated.
#'   the order up to level is calculated based on the review period,lead time and normal distribution .
#' @param  demand  A vector of demand in N time periods.
#' @param  mean  average demand in N time periods.
#' @param sd  standard deviation in N time periods.
#' @param service_level  cycle service level requested
#' @param leadtime  lead time from order to arrival
#' @param shortage_cost  shortage cost per unit of sales lost
#' @param Review_period  the period where the ordeering happens.
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
#'Periodic_review_normal(demand=rpois(80,6),mean=6,sd=0.2,leadtime=5,service_level=0.95,
#'Review_period =9,
#'shortage_cost= FALSE,inventory_cost=FALSE,ordering_cost=FALSE)






Periodic_review_normal<- function(demand,mean,sd,leadtime,service_level,Review_period,
                             shortage_cost= FALSE,inventory_cost=FALSE,
                             ordering_cost=FALSE){
  
  mu = mean
  L = leadtime
  
  Max= round((mean *(leadtime+Review_period))+ ((sd*sqrt(leadtime+Review_period))* qnorm(service_level)),digits = 0)
  saftey_stock= ((sd*sqrt(leadtime+Review_period))* qnorm(service_level))                        
  

  
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
    order[t] <- (Max[t]- IP[t-1]) * (ordering_time[t])
    IP[t] <-  IP[t-1] + order[t] - sales[t]
    
  }
  
  for (t in seq((L+1),(N))){
    sales[t] = min(demand[t], I[t-1] + order[t-L])
    I[t] = I[t-1] + order[t-L] - sales[t]
    order[t] = (Max[t]- IP[t-1]) * (ordering_time[t])
    IP[t] = IP[t-1] + order[t] - sales[t]
    recieved[t]<- order[t-L]
  }
  
  data<-data.frame(period= seq(1:(N+1)),demand=demand,sales=sales,inventory_level=I,
                   inventory_position=IP,review= Review_period,Max=Max,order= order,
                   recieved=recieved)
  
  data$lost_order<- data$demand -data$sales
  message('this function is deprecated, kindly use priodic_policy() instead')
  
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







