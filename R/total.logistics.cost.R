

#' total.logistics.cost
#'
#' calculating total logistics cost .
#'
#' calculating total logistics cost based on a normal distribution.
#'
#'
#' @param quantity quantity ordered every cycle.
#' @param expected_annual_demand numeric,  expected annual demand of the SKU.
#'
#' @param  sd_annual_demand  annual standard deviation of the SKU.
#' @param expected_leadtimeindays expected lead time in days.
#' @param sd_leadtime  standard deviation of leadtime
#' @param costperunit purchase cost of the SKU
#' @param transportcost transport cost of the SKU
#' @param holdingrate  holding rate of the SKU
#' @param ordering_cost  ordering cost per order placed
#' @param csl  cycle service level desired
#'
#' @return a dataframe that contains calculations of the total logistics cost in detail.
#'
#' @import stats
#' @export
#' @author "haytham omar  email: <h.omar5942@gmail.com>"
#' @note this is the first version of the inventorize package, all the functions are common knowlege for supply chain without
#' any academic contribution from my side, the aim is to facilitate and ease much of the bookkeeping that is endured during stock analysis.
#' @examples
#' total.logistics.cost(quantity=32,expected_annual_demand=1550,
#' sd_annual_demand=110,expected_leadtimeindays=64,sd_leadtime=8,
#' costperunit=107,transportcost=22,holdingrate=0.15,ordering_cost=500,csl=0.95)





total.logistics.cost<-function(quantity,expected_annual_demand,sd_annual_demand,expected_leadtimeindays,sd_leadtime,costperunit,transportcost,
                               holdingrate,ordering_cost,csl){

  quantity
  purchase_cost<- expected_annual_demand*costperunit
  transport_cost<- expected_annual_demand*transportcost
  landed_cost<- purchase_cost+transport_cost
  ordering_cost<- (expected_annual_demand/quantity)*ordering_cost
  holding_cost<- (costperunit+transportcost)*holdingrate
  cyclestock_cost<- holding_cost*(quantity/2)
  dl<-expected_annual_demand*expected_leadtimeindays/365
  sigmadl<-sqrt( (expected_leadtimeindays*(sd_annual_demand*sqrt(1/365))^2)+(((expected_annual_demand/365)^2*(sd_leadtime)^2)))

  saftey_stock<- qnorm(csl)*sigmadl
  saftey_stock_cost<- saftey_stock*holding_cost
  total_cost<- purchase_cost+transport_cost+ordering_cost+cyclestock_cost+saftey_stock_cost
  l.costperitem<-(total_cost/expected_annual_demand)
  return(data.frame(quantity=quantity,
                    leadtime=expected_leadtimeindays,
                    purchase_cost=purchase_cost,
                    transport_cost=transport_cost,
                    landed_cost=landed_cost,
                    ordering_cost=ordering_cost,
                    cyclestock_cost=cyclestock_cost,
                    dl=dl,
                    sigmadl=sigmadl,
                    saftey_stock=saftey_stock,
                    saftey_stock_cost=saftey_stock_cost,
                    total_cost=total_cost,
                    costperunit=l.costperitem))
}




