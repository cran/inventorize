#' dl.sigmadl
#'
#' claculating demand lead time with leadtime variability.
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
#' @return a dataframe that contains calculations of the expected demand lead time and the expected standard deviation during leadtime.
#' @import stats
#' @export
#' @author "haytham omar  email: <h.omar5942@gmail.com>"
#' @note  this is the first version of the inventorize package, all the fucntions are common knowlege for supply chain without
#' any academic contribution from my side, the aim is to facilitate and ease much of the bookkeeping that is endured during stock analysis.
#'the package relies heavily on the studies that I had in the MIT micromasters program for supply chain.
#' @examples
#' dl.sigmadl(expected_demand=100,sd_demand=22,expected_leadtime=12,sd_leadtime=3)



dl.sigmadl<- function(expected_demand,sd_demand,expected_leadtime,sd_leadtime){

  DL<- expected_demand*expected_leadtime
  sigmadl<-sqrt( (expected_leadtime*(sd_demand)^2)+((expected_demand^2*(sd_leadtime)^2)))
  return(data.frame(DL=DL,sigmadl=sigmadl))

}


