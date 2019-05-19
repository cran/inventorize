#' reorderpoint_leadtime_variability
#'
#' Calculating saftey stock based on the cycle service level.
#'
#' Calculating re-order point  based on  demand variability and lead time variability in   an assumed normal distribution. cycle service level is provided to calculate saftey stock accordingly.
#'
#'
#' @param dailydemand numeric,daily Expected  demand of the SKU .
#' @param dailystandarddeviation numeric,  standard  deviation of daily demand of the SKU .
#'
#' @param  leadtimein_days  leadtime in days of order.
#' @param  sd_leadtime_days  standard deviation of leadtime in days of order.
#' @param  csl  cycle service level requested
#' @param na.rm Logical, remove na if TRUE
#'
#' @return a dataframe that contains demand lead time,sigmadl,safteyfactor and re_order point.
#'
#' @import stats
#'
#' @author "haytham omar  email: <haytham@rescaleanalytics.com>"
#' @note this is the second version of the inventorize package, all the fucntions are  without
#' any academic contribution from my side, the aim is to facilitate and ease much of the bookkeeping that is endured during stock analysis.
#' @export
#' @examples
#' reorderpoint_leadtime_variability(dailydemand=50,dailystandarddeviation=5,
#' leadtimein_days=6,sd_leadtime_days=2,csl=0.90)


reorderpoint_leadtime_variability <-
  function(dailydemand,dailystandarddeviation,leadtimein_days,sd_leadtime_days,csl,na.rm=TRUE){

    DL<- dailydemand*leadtimein_days
    sigmadl<-sqrt( (leadtimein_days*(dailystandarddeviation)^2)+((dailydemand^2*(sd_leadtime_days)^2)))
    safteyfactor<- qnorm(csl)
    safteystock<-safteyfactor*sigmadl
    quantityinstock<- DL+safteystock
    allpar<- data.frame("demandleadtime"= DL,"sigmadl"=sigmadl,"safteyfactor"=safteyfactor,"reorder_point"=quantityinstock)
    return(allpar)
  }





