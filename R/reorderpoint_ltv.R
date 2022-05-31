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
#' @param distribution  distribution  to calculate safety stock based on demand distribution, current choices are 'normal'
#'  'poisson','gamma' and negative binomial 'nbinom'
#' @importFrom stats dnorm
#' @importFrom stats lm
#' @importFrom stats median
#' @importFrom stats optim
#' @importFrom stats optimize
#' @importFrom stats pnorm
#' @importFrom stats ppois
#' @importFrom stats predict
#' @importFrom stats qnorm
#' @importFrom stats qgamma
#' @importFrom stats qpois
#' @importFrom stats sd
#' @return a dataframe that contains demand lead time,sigmadl,safteyfactor and re_order point.
#' @author "haytham omar  email: <haytham@rescaleanalytics.com>"
#' @note this is the second version of the inventorize package, all the fucntions are  without
#' any academic contribution from my side, the aim is to facilitate and ease much of the bookkeeping that is endured during stock analysis.
#' @export
#' @examples
#' reorderpoint_leadtime_variability(dailydemand=50,dailystandarddeviation=5,
#' leadtimein_days=6,sd_leadtime_days=2,csl=0.90)


reorderpoint_leadtime_variability <-
  function(dailydemand,dailystandarddeviation,leadtimein_days,sd_leadtime_days,csl,distribution= 'nbinom'){

    dl<- dailydemand*leadtimein_days
    sigmadl<-sqrt( (leadtimein_days*(dailystandarddeviation)^2)+((dailydemand^2*(sd_leadtime_days)^2)))
    
    if(distribution== 'normal'){
      safteystock<- sigmadl *  qnorm( csl)
    } else if(distribution== 'poisson'){
      safteystock<-  qpois(csl, dl) - (dl)
      
      
    }else if (distribution== 'gamma'){
      alpha= dl ^2 / sigmadl^2
      beta <- dl / sigmadl^2
      
      safteystock<- qgamma(csl,alpha,beta)-dl
    } else if (distribution== 'nbinom'){
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
      
      x <- 0
      supp <- ComputeNBDoverR(x, dl, sigmadl)
      while (supp < csl){
        x <- x + 1
        supp <- supp + ComputeNBDoverR(x, dl, sigmadl)
      }
      
      safteystock<- x- dl
    }
    
    
  
    quantityinstock<- dl+safteystock
    allpar<- data.frame("demandleadtime"= dl,"sigmadl"=sigmadl,"safteystock"=safteystock,"reorder_point"=quantityinstock)
    return(allpar)
  }


reorderpoint_leadtime_variability(dailydemand=50,dailystandarddeviation=5,
                                 leadtimein_days=6,sd_leadtime_days=2,csl=0.90,distribution = 'gamma')


