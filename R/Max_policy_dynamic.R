#' Max_policy_dynamic
#'
#' Simulating a  max policy or also called S policy, the Max is dynamically calculated based on a forecast vector.
#'
#'  The Function takes a demand vector, mean of demand ,sd,lead time and requested service level to simulate an inventory system, 
#'  orders are lost if inventory level is less than requested demand, also ordering is made at
#'  day t+1, metrics like item fill rate and cycle service level are calculated. 
#'  the min is calculated based on a normal distribution or a poisson distribution, also min can be set manually.
#'  and order is equal to max((Max[t]-inventory position [t-1])+ forecast[t],0)
#' @param  demand  A vector of demand in N time periods.
#' @param  forecast  the forecast vector of equal n periods to demand.
#' @param leadtime  lead time from order to arrival (order to delivery time)
#' @param service_level  cycle service level requested
#' @param initial_inventory_level  integer,Default is False and simulation starts with min as inventory level
#' @param one_step_forecast logical, Default is true where demand lead time is calcluated as(forecast at period t * leadtime)
#' while if False, demand
#' leadtime is calculated as (forecast of period t to forecast of period t+leadtime-1)
#' @param shortage_cost  numeric,Default is FALSE shortage cost per unit of sales lost
#' @param inventory_cost  numeric,Default is FALSE inventory cost per unit.
#' @param ordering_cost  numeric,Default is FALSE ordering cost for every time an order is made.
#' @param distribution  distribution  to calculate safety stock based on demand distribution, current choices are 'normal' or 'poisson'
#' @param error_metric  metric is currently 'rmse' and 'mae', this calculates the error from period 1 to period t unless metric_windows is set.
#' this contributes to the calculation of saftey stock. default is 'rmse'
#' @param metric_windows  integer, for example if it is set to 4 rmse for t is calculated from t-1 to t-4,default is FALSE
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
#' @return a list of two date frames, the simulation and the metrics. the metrics are (1) shortage cost, (2) inventory cost which
#' is the cost of one unit of inventory in one period,(3) which is the average inventory level per period, (4) total orders made in the 
#' simulation, (5) ordering cost if any, (6) total lost sales if any,(7) average ordering quantity across all orders,(8) ordering
#' interval which is the average time between each order,(9) item fill rate,(10) cycle service level, (11) average saftey stock in each
#' period,(12) the average sales in every order,(13) overall root mean square error, (14) overall mean absolute error, 
#' (14) overall mean  error,(15) overall mean absolute percentage error,(16) the average flowttime which is the average time 
#' a unit spends on inventory and (17) the demand classification.
#' @author "haytham omar  email: <haytham@rescaleanalytics.com>"
#' @export
#' @examples
#'Max_policy_dynamic(demand = rnorm(90,9,2),forecast = rpois(90,9) ,
#'service_level = 0.7,leadtime = 10)


Max_policy_dynamic<-function (demand,forecast, leadtime, service_level,initial_inventory_level=FALSE,one_step_forecast=TRUE,
                      
                      shortage_cost = FALSE, 
                      inventory_cost = FALSE, ordering_cost = FALSE,
                      distribution= 'normal',error_metric= 'rmse',metric_windows= FALSE,plot=FALSE) 
{
  inventory_level<-NULL
  period<-NULL
  initial_inventory_level=initial_inventory_level
  L= leadtime
  N = length(demand)
  demand <- c(0, demand)
  forecast<- c(0,forecast)
  
  if (one_step_forecast== TRUE){
    dl<- forecast*leadtime
  } else {
    dl<- rep(NA,length(demand)) 
    
    for (i in 1: length(dl)){
      dl[i]<- sum(forecast[i : min((i+leadtime-1),length(dl))])
    }
  }
  
  error<- demand - forecast
  metric<- c(rep(NA,length(demand)))
  lambda<- mean(demand)
  
  order = rep(NA, N + 1)
  I = rep(NA, N + 1)
  IP = rep(NA, N + 1)
  sales = rep(NA, N + 1)
  recieved = rep(NA, N + 1)
  order[1] = 0
  
  if(error_metric == 'rmse'){
    if (metric_windows== FALSE){
      for (i in 2: length(demand)){
        metric[i]<- sqrt(mean((demand[1:i-1]- forecast[1:i-1])^2,na.rm = TRUE))
      }
    } else {
      for (i in 2: length(demand)){
        metric[i]<- sqrt(mean((demand[max((i- metric_windows),0):(i-1)]- forecast[max((i- metric_windows),0):(i-1)])^2,na.rm = TRUE))
        
      }
    }
  }
  
  if(error_metric == 'mae'){
    if (metric_windows== FALSE){
      for (i in 2: length(demand)){
        metric[i]<- mean(abs(demand[1:i-1]- forecast[1:i-1]),na.rm = TRUE)
      }
    } else {
      for (i in 2: length(demand)){
        metric[i]<- mean(abs(demand[max((i- metric_windows),0):(i-1)]- forecast[max((i- metric_windows),0):(i-1)]),na.rm = TRUE)
        
      }
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
  
  
  
  sigmadl<- metric* sqrt(leadtime)
  if(distribution== 'normal'){
    saftey_stock<- sigmadl *  qnorm( service_level)
  } else if(distribution== 'poisson'){
    saftey_stock<-  qpois(service_level, demand*leadtime) - (demand*leadtime)
    
    
  }
  Max= round(dl+saftey_stock,0)
  Max[is.na(Max)]<- round(mean(Max,na.rm = TRUE),0)
  if(initial_inventory_level==FALSE){
    IP[1] = I[1] =  Max[1]
  } else {
    IP[1] = I[1] =  initial_inventory_level
    
  }
  
  for (t in 2:(L)) {
    sales[t] <- min(demand[t], I[t - 1])
    I[t] <- I[t - 1] - sales[t]
    order[t] <- max((Max[t] - IP[t-1]+sales[t]),0)
    IP[t] <- IP[t - 1] + order[t] - sales[t]
  }
  for (t in seq((L + 1), (N))) {
    sales[t] = min(demand[t], I[t - 1] + order[t - L])
    I[t] = I[t - 1] + order[t - L] - sales[t]
    order[t] = max((Max[t] - IP[t-1]+sales[t]),0)
    IP[t] = IP[t - 1] + order[t] - sales[t]
    recieved[t] <- order[t - L]
  }
  data <- data.frame('period' = seq(1:(N + 1)), demand = demand, forecast=forecast,rolling_error= metric,
                     sales = sales, 'inventory_level' = I, inventory_position = IP,expected_demand_leadtime= dl,sigmadl= sigmadl,saftey_stock=saftey_stock,
                      Max = Max, order = order, recieved = recieved)
  data$lost_order <- data$demand - data$sales
  metrics <- data.frame(shortage_cost = sum(data$lost_order, 
                                            na.rm = TRUE) * shortage_cost, inventory_cost = sum(data$inventory_level, na.rm = TRUE) * 
                          inventory_cost, average_inventory_level = mean(data$inventory_level, na.rm = TRUE),total_orders= length(which(data$order > 0)),ordering_cost = length(which(data$order > 0)) * ordering_cost,
                        total_lost_sales = sum(data$lost_order, na.rm = TRUE), average_ordering_quantity= mean(order[order>0],na.rm = TRUE),ordering_interval= paste0(round(length(demand)/length(which(data$order > 0)),2),'_periods'),
                        Item_fill_rate = 1 - (sum(data$lost_order, na.rm = TRUE)/sum(demand[1:(length(demand) - 1)])), 
                        cycle_service_level = 1 -(length(which(data$lost_order > 0))/(length(demand) - 1)), saftey_stock = mean(saftey_stock,na.rm = TRUE),
                        average_sales= mean(sales,na.rm = TRUE),rmse= sqrt(mean((demand-forecast)^2,na.rm=TRUE)),
                        mae= mean(abs(demand-forecast),na.rm = TRUE),me= mean(demand-forecast),mape= mean((abs(demand-forecast)/abs(forecast))*100,na.rm=TRUE))
  metrics$"average_flow_time(throughput)"= metrics$average_inventory_level/metrics$average_sales
  metrics$demand_class<- demand_class$Type
  if(plot== TRUE){
    suppressWarnings(
      print(plotly::ggplotly(data[is.na(data[,c('sales','demand','order')])==FALSE,] %>% ggplot(aes(x= period,y= demand,color="demand"))+geom_line()+
              geom_line(aes(y=sales,color="sales"))+geom_line(aes(y=forecast,color="forecast"))+
              geom_point(aes(y=inventory_level,color="inventory level"))+
              theme_minimal()+geom_line(aes(y=order,color="order"))+ggtitle("Max  Policy Dynamic")))
    )
  }
  
 
  return(list(simu_data = data, metrics = metrics))
}



