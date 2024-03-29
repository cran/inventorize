
#' R_s_S
#'
#' Simulating a Min Max periodic policy or also called R,s,S policy,R represents the ordering/review period.  .
#'
#' The Function takes a demand vector, mean of demand ,sd,lead time and requested service level to simulate an inventory system, 
#' orders are lost if inventory level is less than requested demand, also ordering is made at
#'  day t+1, metrics like item fill rate and cycle service level are calculated. 
#'  the min is calculated based on a normal distribution or a poisson distribution, also min can be set manually.
#'  Max - inventory position is ordered whenever inventory position reaches min at the priod of review 
#' @param  demand  A vector of demand in N time periods.
#' @param  mean  average demand in N time periods.default is FALSE and is automatically calculated. otherwise set manually.
#' @param  sd  standard deviation in N time periods.default is FALSE and is automatically calculated. otherwise set manually.
#' @param leadtime  lead time from order to arrival (order to delivery time)
#' @param service_level  cycle service level requested
#' @param initial_inventory_level  integer,Default is False and simulation starts with min as inventory level
#' @param min  integer,Default is False and min is calculated based on mean,demand and lead time unless set manually
#' @param Max  integer,Default is False and max is calculated as a ratio to min,otherwise set manually.
#' @param Min_to_max  numeric, the ratio of min to max calculation , default 0.6 but can be changed manually
#' @param Review_period  Integer, the number of periods where every order is allowed to be made.
#' @param shortage_cost  numeric,Default is FALSE shortage cost per unit of sales lost
#' @param inventory_cost  numeric,Default is FALSE inventory cost per unit.
#' @param ordering_cost  numeric,Default is FALSE ordering cost for every time an order is made.
#' @param distribution  distribution  to calculate safety stock based on demand distribution, current choices are 'normal'
#'  'poisson','gamma' and negative binomial 'nbinom'
#' @param recalculate  Logical, if true the mean and sd is recalculated every  period from first period to t,default is FALSE .
#' @param recalculate_windows  integer, the min  mean and sd windows to recalculate , for example if it is set to 4 mean and sd
#' is calculated from t to t-4,,default is FALSE, if TRUE, recalculate has to be TRUE Also.
#' @param Backlogs  Logical, Default is False, if true inventory level accounts for previous lost orders
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
#' @importFrom stats qpois
#' @importFrom stats qgamma
#' @importFrom stats sd
#' @import ggplot2
#' @importFrom magrittr %>%
#' @importFrom  plotly ggplotly
#' @return a list of two date frames, the simulation and the metrics.
#' @author "haytham omar  email: <haytham@rescaleanalytics.com>"
#' @export
#' @examples
#'R_s_S(demand = rpois(90,9),service_level = 0.97,leadtime = 10,
#' Review_period = 10,Backlogs=TRUE)

R_s_S<-function (demand, mean = FALSE, sd=FALSE, leadtime, service_level, initial_inventory_level = FALSE, min= FALSE,Max=FALSE, Min_to_max=0.6,
                 Review_period,
                 shortage_cost = FALSE, inventory_cost = FALSE,
                 ordering_cost = FALSE,distribution= 'normal',recalculate=FALSE,recalculate_windows=FALSE,plot=FALSE,Backlogs=TRUE) 
{  
  inventory_level<-NULL
  period<-NULL
  
  ComputeNBDoverR <- function(x, mu_R, sigm_R){
    if(sigm_R^2 <= mu_R){
      sigm_R<- 1.05 * sqrt(mu_R)
    }
    z <- (sigm_R ^ 2) / mu_R
    if (z > 1){
      P0 <- (1 / z) ^ (mu_R / (z - 1))
      if (x == 0){
        PX <- P0
      } else {
        PX <- P0
        for (i in 1:x){
          PX = (((mu_R / (z - 1)) + i - 1) / i) * ((z - 1) / z) * PX
        }
      }
    }
    
    return(PX)}
  if(recalculate_windows==FALSE & recalculate !=FALSE){
    
    mean = c(rep(NA,length(demand)+1))
    sd= c(rep(NA,length(demand)+1))
    max= c(rep(NA,length(demand)+1))
    Max= c(rep(NA,length(demand)+1))
    mean[1]<- demand[1]
    sd[1]<- sd(demand)
    for (i in 2: length(mean)){
      mean[i]= mean(demand[1:i],na.rm=TRUE)
      sd[i]= sd(demand[1:i],na.rm=TRUE)
      if(distribution== 'normal'){
        
        max[i] = round((mean[i] * (leadtime+Review_period)) + ((sd[i] * sqrt(leadtime+Review_period)) * 
                                                                 qnorm(service_level)), digits = 0)
      } else if (distribution== 'poisson') {
        
        max[i] = qpois(service_level,mean[i]*(leadtime+Review_period))
      } else if( distribution== 'gamma'){
        dl= mean[i]*(leadtime+Review_period)
        sigmadl= sd[i] * sqrt(leadtime+Review_period)
        alpha= dl ^2 / sigmadl^2
        beta <- dl / sigmadl^2
        max[i]<- round(qgamma(service_level,alpha,beta))
      } else if (distribution== 'nbinom'){
        dl= mean[i]*(leadtime+Review_period)
        sigmadl= sd[i] * sqrt(leadtime+Review_period)
        
        x <- 0
        supp <- ComputeNBDoverR(x, dl, sigmadl)
        while (supp < service_level){
          x <- x + 1
          supp <- supp + ComputeNBDoverR(x, dl, sigmadl)
        }
        max[i]<- x
      }
      
    }
    Max <- max
    Max[1]= max[2]
    
  } else if ( recalculate_windows !=FALSE & recalculate != FALSE) {
    
    mean = c(rep(NA,length(demand)+1))
    sd= c(rep(NA,length(demand)+1))
    max= c(rep(NA,length(demand)+1))
    Max= c(rep(NA,length(demand)+1))
    mean[1]<- demand[1]
    sd[1]<- sd(demand)
    for (i in 2: length(mean)){
      mean[i]= mean(demand[max((i- recalculate_windows),1):(i-1)],na.rm=TRUE)
      sd[i]= ifelse(is.na(sd(demand[max((i- recalculate_windows),1):(i-1)],na.rm=TRUE)),sd[i-1],sd(demand[max((i- recalculate_windows),1):(i-1)],na.rm=TRUE))
      if(distribution== 'normal'){
        
        max[i] = round((mean[i] * (leadtime+Review_period)) + ((sd[i] * sqrt(leadtime+Review_period)) * 
                                                                 qnorm(service_level)), digits = 0)
      } else if (distribution== 'poisson') {
        
        max[i] = qpois(service_level,mean[i]*(leadtime+Review_period))
      } else if( distribution== 'gamma'){
        dl= mean[i]*(leadtime+Review_period)
        sigmadl= sd[i] * sqrt(leadtime+Review_period)
        alpha= dl ^2 / sigmadl^2
        beta <- dl / sigmadl^2
        max[i]<- round(qgamma(service_level,alpha,beta))
      } else if (distribution== 'nbinom'){
        dl= mean[i]*(leadtime+Review_period)
        sigmadl= sd[i] * sqrt(leadtime+Review_period)
        
        
        x <- 0
        supp <- ComputeNBDoverR(x, dl, sigmadl)
        while (supp < service_level){
          x <- x + 1
          supp <- supp + ComputeNBDoverR(x, dl, sigmadl)
        }
        max[i]<- x
      }
    }
    
    
    
    Max[1]= max[2]
    for (i in 2: length(max)){
      Max[i]<- ifelse(i %% recalculate_windows !=0,Max[i-1],max[i])
    }
  } 
  
  
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
  
  if (Max[1] != FALSE& recalculate==FALSE){
    
    Max= Max
    Max = rep(Max, N + 1)
    
  } else if(distribution== 'normal'& recalculate==FALSE){
    
    Max = round((mean * (leadtime+Review_period)) + ((sd * sqrt(leadtime+Review_period)) * 
                                                       qnorm(service_level)), digits = 0)
    Max = rep(Max, N + 1)
    
  } else if (distribution== 'poisson' & recalculate==FALSE){
    
    Max = qpois(service_level,mean*(leadtime+Review_period))
    Max = rep(Max, N + 1)
    
  } else if (distribution== 'gamma' & recalculate==FALSE){
    dl= mean*(leadtime+Review_period)
    sigmadl= sd * sqrt(leadtime+Review_period)
    alpha= dl ^2 / sigmadl^2
    beta <- dl / sigmadl^2
    Max <- round(qgamma(service_level,alpha,beta))
    Max = rep(Max, N + 1)
    
  } else if (distribution== 'nbinom' & recalculate==FALSE){
    dl= mean*(leadtime+Review_period)
    sigmadl= sd * sqrt(leadtime+Review_period)
    
    x <- 0
    supp <- ComputeNBDoverR(x, dl, sigmadl)
    while (supp < service_level){
      x <- x + 1
      supp <- supp + ComputeNBDoverR(x, dl, sigmadl)
    }
    Max<- round(x)
    Max = rep(Max, N + 1)
    
  }
  
  saftey_stock= Max- (mean*(leadtime+Review_period))
  
  if(min[1]== FALSE){
    min= round(Min_to_max*Max,0)
    
  } else{
    min= rep(min,N+1)
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
  
  
  
  
  L= leadtime
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
  
  ordering_time <- rep(rep(c(0, 1), c(Review_period - 1, 1)), 
                       length(demand))
  if(Backlogs != TRUE){
    for (t in 2:(L)) {
      sales[t] <- min(demand[t], I[t - 1])
      I[t] <- I[t - 1] - sales[t]
      order[t] = max((Max[t] - IP[t - 1]) * (ordering_time[t])* (IP[t - 1] <= min[t]),0)
      IP[t] <- IP[t - 1] + order[t] - sales[t]
    }
    for (t in seq((L + 1), (N))) {
      sales[t] = min(demand[t], I[t - 1] + order[t - L])
      I[t] = I[t - 1] + order[t - L] - sales[t]
      order[t] = max((Max[t] - IP[t - 1]) * (ordering_time[t])* (IP[t - 1] <= min[t]),0)
      IP[t] = IP[t - 1] + order[t] - sales[t]
      recieved[t] <- order[t - L]
    }
  }else {
    backlogs = rep(NA, N + 1)
    comu_backlogs = rep(NA, N + 1)
    expected=rep(NA, N + 1)
    order[1] = 0
    sales[1] <- 0
    expected[1]<- 0
    backlogs[1]<- 0
    comu_backlogs[1]<-0
    for (t in 2:(L)) {
      sales[t] <- min(demand[t], I[t - 1])
      I[t] <- ifelse(I[t-1]-demand[t]- comu_backlogs[t-1]>0,I[t-1]-
                       demand[t]-comu_backlogs[t-1],0)
      order[t] = max((Max[t] - IP[t - 1]) * (ordering_time[t])* (IP[t - 1] <= min[t]),0)
      expected[t]<- expected[t-1]+ order[t-1]
      backlogs[t]<- ifelse(I[t-1]< demand[t],abs(I[t-1]-demand[t]),0)
      comu_backlogs[t]<- ifelse(backlogs[t]>I[t-1] | backlogs[t]>0,comu_backlogs[t-1]+backlogs[t],0)
      IP[t] <- IP[t - 1] + order[t] - sales[t]
    }
    for (t in seq((L + 1), (N))) {
      sales[t] <- min(demand[t], I[t - 1])
      recieved[t] <- order[t - L]
      
      I[t] <- ifelse(I[t-1]-demand[t]+recieved[t]- comu_backlogs[t-1]>0,I[t-1]-
                       demand[t]+recieved[t]-comu_backlogs[t-1],0)
      order[t] = max((Max[t] - IP[t - 1]) * (ordering_time[t])* (IP[t - 1] <= min[t]),0)
      expected[t]<- expected[t-1]+ order[t-1]- recieved[t]
      backlogs[t]<-  ifelse(I[t-1]< demand[t],abs(I[t-1]-demand[t]),0)
      comu_backlogs[t]<- ifelse(backlogs[t]>I[t-1] | backlogs[t]>0,comu_backlogs[t-1]+backlogs[t],0)
      IP[t] <- I[t]+ expected[t]- comu_backlogs[t]
    }
  }
  
  if(Backlogs != TRUE){
  data <- data.frame('period' = seq(1:(N + 1)), demand = demand, 
                     sales = sales, 'inventory_level' = I, inventory_position = IP, 
                     min = min, review = Review_period, Max = Max, order = order, 
                     recieved = recieved)
  data$lost_order <- data$demand - data$sales
 } else {
   data <- data.frame('period' = seq(1:(N + 1)), demand = demand, 
                      sales = sales, 'inventory_level' = I, inventory_position = IP, 
                      min = min, review = Review_period, Max = Max, order = order, 
                      recieved = recieved,comu_backlogs)
   data$lost_order <- data$demand - data$sales
    
  }
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
              theme_minimal()+geom_line(aes(y=order,color="order"))+ggtitle("R s S Policy")))
    )
  }
  
  return(list(simu_data = data, metrics = metrics))
}



R_s_S(demand = rpois(90,9),service_level = 0.95,
                leadtime = 10,Review_period = 10,distribution = 'nbinom',
      recalculate = TRUE,recalculate_windows = 10,Backlogs = TRUE)








