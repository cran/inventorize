% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sim_min_max.R
\name{sim_min_max}
\alias{sim_min_max}
\title{sim_min_max}
\usage{
sim_min_max(
  demand,
  mean = FALSE,
  sd = FALSE,
  leadtime,
  service_level,
  initial_inventory_level = FALSE,
  min = FALSE,
  Max = FALSE,
  Max_to_min = 1.3,
  shortage_cost = FALSE,
  inventory_cost = FALSE,
  ordering_cost = FALSE,
  distribution = "normal",
  recalculate = FALSE,
  recalculate_windows = FALSE,
  plot = FALSE,
  Backlogs = FALSE
)
}
\arguments{
\item{demand}{A vector of demand in N time periods.}

\item{mean}{average demand in N time periods.default is FALSE and is automatically calculated. otherwise set manually.}

\item{sd}{standard deviation in N time periods.default is FALSE and is automatically calculated. otherwise set manually.}

\item{leadtime}{lead time from order to arrival (order to delivery time)}

\item{service_level}{cycle service level requested}

\item{initial_inventory_level}{integer,Default is False and simulation starts with min as inventory level}

\item{min}{integer,Default is False and min is calculated based on mean,demand and lead time unless set manually}

\item{Max}{integer,Default is False and max is calculated as a ratio to min,otherwise set manually.}

\item{Max_to_min}{numeric, the ratio of Max to min calculation , default 1.3 but can be changed manually.}

\item{shortage_cost}{numeric,Default is FALSE shortage cost per unit of sales lost}

\item{inventory_cost}{numeric,Default is FALSE inventory cost per unit.}

\item{ordering_cost}{numeric,Default is FALSE ordering cost for every time an order is made.}

\item{distribution}{distribution  to calculate safety stock based on demand distribution, current choices are 'normal'
'poisson','gamma' and negative binomial 'nbinom'}

\item{recalculate}{Logical, if true the mean and sd is recalculated every  period from first period to t,default is FALSE .}

\item{recalculate_windows}{integer, the min  mean and sd windows to recalculate , for example if it is set to 4 mean and sd
is calculated from t to t-4,,default is FALSE, if TRUE, recalculate has to be TRUE Also.}

\item{plot}{Logical, Default is False, if true a plot is generated}

\item{Backlogs}{Logical, Default is False, if true inventory level accounts for previous lost orders}
}
\value{
a list of two date frames, the simulation and the metrics.
}
\description{
Simulating a min max policy or also called s,S policy,  .
The Function takes a demand vector, mean of demand ,sd,lead time and requested service level to simulate an inventory system, 
orders are lost if inventory level is less than requested demand, also ordering is made at
 day t+1, metrics like item fill rate and cycle service level are calculated. 
 the min is calculated based on a normal distribution or a poisson distribution, also min can be set manually.
 Max - inventory position is ordered whenever inventory position reaches min
}
\examples{
sim_min_max(demand = rpois(80,6),leadtime = 4,service_level = 0.95,recalculate = TRUE)
}
\author{
"haytham omar  email: <haytham@rescaleanalytics.com>"
}
