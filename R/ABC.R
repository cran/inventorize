
#' ABC
#'
#' Identyfing ABC category based on the pareto rule.
#' Identyfing ABC category based on the pareto rule.A category is up to 80%. B category is up 95% and C category is up to 100%.
#' @param data, Data frame of tuo columns,first column is the item name, second column is the item value/flow/demand.
#' @param na.rm, logical and by default is TRUE
#' @param plot, default is FALSE,if true a plot is generated
#'
#' @return a dataframe that contains ABC categories with a bar plot of the count of items in each category.
#' @import ggplot2
#' @import dplyr
#' @importFrom magrittr %>%
#' @author "haytham omar  email: <haytham@rescaleanalytics.com>"
#' @note this is the second version of the inventorize package, all the fucntions are  without
#' any academic contribution from my side, the aim is to facilitate and ease much of the bookkeeping that is endured during stock analysis.
#' @importFrom stats dnorm
#' @importFrom stats lm
#' @importFrom stats median
#' @importFrom stats optim
#' @importFrom stats optimize
#' @importFrom stats pnorm
#' @importFrom stats ppois
#' @importFrom stats predict
#' @importFrom stats qnorm
#' @export
#' @examples
#' ABC(data.frame(SKU= seq(1:1000),demand=runif(1000,1,1000)))




ABC<- function(data,na.rm=TRUE,plot=FALSE){


  data1<- data.frame(data)

  data1<-data1 %>% dplyr::mutate(percentage=data1[,2]/sum(data1[,2])) %>% dplyr::arrange(desc(data1[,2]))
  data1$comulative<- cumsum(data1$percentage)

  for(i in 1:nrow(data1)){

    data1$category[i]<-if(data1$comulative[i] < 0.8){"A"
    }else if (data1$comulative[i]< 0.95){"B"
    }else{"C"}
  }
  if(plot==TRUE){
  print(ggplot(data1,aes(x=data1$category))+geom_bar(aes(fill=category))+xlab("Category count")+theme_minimal())
  }
  return(data1)

}


