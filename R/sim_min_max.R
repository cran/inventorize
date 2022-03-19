
#' sim_min_max
#'
#' Simulating a min max policy or also called s,S policy,  .
#' The Function takes a demand vector, mean of demand ,sd,lead time and requested service level to simulate an inventory system, 
#' orders are lost if inventory level is less than requested demand, also ordering is made at
#'  day t+1, metrics like item fill rate and cycle service level are calculated. 
#'  the min is calculated based on a normal distribution or a poisson distribution, also min can be set manually.
#'  Max - inventory position is ordered whenever inventory position reaches min
#' @param  demand  A vector of demand in N time periods.
#' @param  mean  average demand in N time periods.default is FALSE and is automatically calculated. otherwise set manually.
#' @param  sd  standard deviation in N time periods.default is FALSE and is automatically calculated. otherwise set manually.
#' @param leadtime  lead time from order to arrival (order to delivery time)
#' @param service_level  cycle service level requested
#' @param initial_inventory_level  integer,Default is False and simulation starts with min as inventory level
#' @param min  integer,Default is False and min is calculated based on mean,demand and lead time unless set manually
#' @param Max  integer,Default is False and max is calculated as a ratio to min,otherwise set manually.
#' @param Max_to_min  numeric, the ratio of Max to min calculation , default 1.3 but can be changed manually.
#' @param shortage_cost  numeric,Default is FALSE shortage cost per unit of sales lost
#' @param inventory_cost  numeric,Default is FALSE inventory cost per unit.
#' @param ordering_cost  numeric,Default is FALSE ordering cost for every time an order is made.
#' @param distribution  distribution  to calculate safety stock based on demand distribution, current choices are 'normal' or 'poisson'
#' @param recalculate  integer, the mean and sd is recalculated every X periods from first period to x,default is FALSE .
#' @param recalculate_windows  integer, the min  mean and sd windows to recalculate , for exammple if it is set to 4 mean and sd
#' is calculated from t to t-4,,default is FALSE .
#' @param plot  Logical, Default is False, if true a plot is generated
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
#' @importFrom  plotly ggplotly
#' @return a list of two date frames, the simulation and the metrics.
#' @author "haytham omar  email: <haytham@rescaleanalytics.com>"
#' @export
#' @examples
#' sim_min_max(demand = rpois(80,6),leadtime = 4,service_level = 0.95,recalculate = 8)

sim_min_max<- function (demand, mean = FALSE, sd=FALSE, leadtime, service_level, initial_inventory_level = FALSE, min= FALSE,Max=FALSE,Max_to_min=1.3,
                       shortage_cost = FALSE, inventory_cost = FALSE, 
                       ordering_cost = FALSE,distribution= 'normal',recalculate=FALSE,recalculate_windows=FALSE,plot=FALSE) 
  
{
  inventory_level<-NULL
  period<-NULL
  L = leadtime

  if(recalculate != FALSE){
    
    mean = c(rep(NA,length(demand)+1))
    sd= c(rep(NA,length(demand)+1))
    minn= c(rep(NA,length(demand)+1))
    min= c(rep(NA,length(demand)+1))
    for (i in 1: length(mean)){
      mean[i]= mean(demand[1:i],na.rm=TRUE)
      sd[i]= sd(demand[1:i],na.rm=TRUE)
      if(distribution== 'normal'){
        
        minn[i] = round((mean[i] * (leadtime)) + ((sd[i] * sqrt(leadtime)) * 
                                                    qnorm(service_level)), digits = 0)
      } else {
        
        minn[i] = qpois(service_level,mean[i]*(leadtime))
      }
      
    }
    
    if(recalculate_windows != FALSE){
      mean[1]<- demand[1]
      sd[1]<- sd(demand)
      for (i in 2: length(mean)){
        mean[i]= mean(demand[max((i- recalculate_windows),1):(i-1)],na.rm=TRUE)
        sd[i]= ifelse(is.na(sd(demand[max((i- recalculate_windows),1):(i-1)],na.rm=TRUE)),sd[i-1],sd(demand[max((i- recalculate_windows),1):(i-1)],na.rm=TRUE))
        if(distribution== 'normal'){
          
          minn[i] = round((mean[i] * (leadtime)) + ((sd[i] * sqrt(leadtime)) * 
                                                      qnorm(service_level)), digits = 0)
        } else {
          
          minn[i] = qpois(service_level,mean[i]*(leadtime))
        }
        
      }
      
    }
    
    min[1]= minn[2]
    for (i in 2: length(min)){
      min[i]<- ifelse(i %% recalculate !=0,min[i-1],minn[i])
    }
  }
  
  classfication <- function(demand){
    intervals <- function(x){
      y<-c()
      k<-1
      counter<-0
      for (tmp in (1:length(x))){
        if(x[tmp]==0){
          counter<-counter+1
        }else{
          k<-k+1
          y[k]<-counter
          counter<-1
        }
      }
      y<-y[y>0]
      y[is.na(y)]<-1
      y
    }
    demand1 <- function(x){
      y<-x[x!=0]
      y
    }
    
    D <- demand1(demand)
    ADI <- mean(intervals(demand))
    CV2 <- (sd(D)/mean(D))^2
    
    
    if (ADI > (4/3)){
      if (CV2 > 0.5){
        Type <- "Lumpy"
      }else{
        Type <- "Intermittent"
      }
    }else{
      if (CV2 > 0.5){
        Type <- "Erratic"
      }else{
        Type <- "Smooth"
      }
    }
    return(data.frame('Type'=Type))
  }
  
  demand_class= classfication(demand)
  
  
  
  N= length(demand)
  
  if(mean[1]== FALSE & recalculate==FALSE){
    mean= mean(demand)
  } else if (mean[1]!= FALSE & recalculate==FALSE) {
    mean=mean
  }
  
  if(sd[1]== FALSE & recalculate==FALSE){
    sd= sd(demand)
  } else if (sd[1]!= FALSE & recalculate==FALSE) {
    sd =sd
  }
  
  if (min[1] != FALSE& recalculate==FALSE){
    
    min= min
    min = rep(min, N + 1)
    
  } else if(distribution== 'normal'& recalculate==FALSE){
    
    min = round((mean * leadtime) + ((sd * sqrt(leadtime)) * 
                                       qnorm(service_level)), digits = 0)
    min = rep(min, N + 1)
    
  } else if (distribution== 'poisson' & recalculate==FALSE){
    
    min = qpois(service_level,mean*leadtime)
    min = rep(min, N + 1)
    
  }
  
  if(Max==FALSE){
    Max= round(Max_to_min *min,0)
  } else {
    Max= rep(Max,N+1)
  }
  
  N = length(demand)
  order = rep(NA, N + 1)
  I = rep(NA, N + 1)
  IP = rep(NA, N + 1)
  sales = rep(NA, N + 1)
  recieved = rep(NA, N + 1)
  order[1] = 0
  demand <- c(0, demand)
  
  if(initial_inventory_level==FALSE){
    IP[1] = I[1] =  Max[1]
  } else {
    IP[1] = I[1] =  initial_inventory_level
    
  }
  saftey_stock= min- (mean*leadtime)
  
  
  for (t in 2:(L)) {
    sales[t] <- min(demand[t], I[t - 1])
    I[t] <- I[t - 1] - sales[t]
    order[t] <- (Max[t] - IP[t - 1]) * (IP[t - 1] <= min[t])
    IP[t] <- IP[t - 1] + order[t] - sales[t]
  }
  for (t in seq((L + 1), (N))) {
    sales[t] = min(demand[t], I[t - 1] + order[t - L])
    I[t] = I[t - 1] + order[t - L] - sales[t]
    order[t] = (Max[t] - IP[t - 1]) * (IP[t - 1] <= min[t])
    IP[t] = IP[t - 1] + order[t] - sales[t]
    recieved[t] <- order[t - L]
  }
  data <- data.frame('period' = seq(1:(N + 1)), demand = demand,
                     sales = sales, 'inventory_level' = I, inventory_position = IP,saftey_stock=saftey_stock,
                     min = min, Max = Max, order = order, recieved = recieved)
  data$lost_order <- data$demand - data$sales
  metrics <- data.frame(shortage_cost = sum(data$lost_order, 
                                            na.rm = TRUE) * shortage_cost, inventory_cost = sum(data$inventory_level, na.rm = TRUE) * 
                          inventory_cost, average_inventory_level = mean(data$inventory_level, na.rm = TRUE),total_orders= length(which(data$order > 0)),ordering_cost = length(which(data$order > 0)) * ordering_cost,
                        total_lost_sales = sum(data$lost_order, na.rm = TRUE), average_ordering_quantity= mean(order[order>0],na.rm = TRUE),ordering_interval= paste0(round(length(demand)/length(which(data$order > 0)),2),'_periods'),
                        Item_fill_rate = 1 - (sum(data$lost_order, na.rm = TRUE)/sum(demand[1:(length(demand) - 1)])), 
                        cycle_service_level = 1 -(length(which(data$lost_order > 0))/(length(demand) - 1)), saftey_stock = mean(saftey_stock,na.rm = TRUE),
                        average_sales= mean(sales,na.rm = TRUE))
  metrics$"average_flow_time(throughput)"= metrics$average_inventory_level/metrics$average_sales
  metrics$demand_class<- demand_class$Type
  if(plot== TRUE){
    suppressWarnings(
      print(plotly::ggplotly(data[is.na(data[,c('sales','demand','order')])==FALSE,] %>% ggplot(aes(x= period,y= demand,color="demand"))+geom_line()+
              geom_line(aes(y=sales,color="sales"))+
              geom_point(aes(y=inventory_level,color="inventory level"))+
              theme_minimal()+geom_line(aes(y=order,color="order"))+ggtitle("Min Max Policy")))
    )
  }
  
  return(list(simu_data = data, metrics = metrics))
                                                   
}




