#' dl.sigmadl
#'
#' claculating demand lead time,saftey stock when there is a  leadtime variability.
#'
#' calculating leadtime with leadtime variablility as delivery time diffires to long distances and reliability of mode of transport.
#' thus demand leadtime and standard deviation during lead time takes into consideration the lead time variability.
#' @param  expected_demand, numeric,expected daily demand .
#' @param sd_demand numeric,standard deviation of daily demand .
#' @param expected_leadtime numeric,  expected leadtime in days.
#'
#' @param  sd_leadtime  numeric,standard deviation of leadtime
#'
#'
#' @return a dataframe that contains calculations of the expected demand lead time and the expected saftey stock during leadtime. It is noted that saftey stock here is
#' more than normal due to leadtime variability.
#' @import stats
#' @author "haytham omar  email: <haytham@rescaleanalytics.com>"
#' @note this is the second version of the inventorize package, all the fucntions are  without
#' any academic contribution from my side, the aim is to facilitate and ease much of the bookkeeping that is endured during stock analysis.
#' @export
#' @examples
#' dl.sigmadl(expected_demand=100,sd_demand=22,expected_leadtime=12,sd_leadtime=3)



dl.sigmadl<- function(expected_demand,sd_demand,expected_leadtime,sd_leadtime){

  DL<- expected_demand*expected_leadtime
  sigmadl<-sqrt( (expected_leadtime*(sd_demand)^2)+((expected_demand^2*(sd_leadtime)^2)))
  return(data.frame(DL=DL,sigmadl=sigmadl))

}


