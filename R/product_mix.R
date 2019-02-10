
#' productmix
#'
#' Identyfing ABC category based on the pareto rule for both demand and selling price,a mix of nine categories are produced.
#'Identyfing ABC category based on the pareto rule.A category is up to 80%. B category is up 95% and C category is up to 100%.
#'
#'
#' @param  SKUs, charachter, a vector of SKU names.
#' @param  sales, vector, a vector of items sold per sku, should be the same number of rows as SKU.
#' @param   revenue price  vector, a vector of total revenu  per sku, should be the same number of rows as SKU.
#' @param na.rm , logical and by default is TRUE
#'
#' @return a dataframe that contains ABC categories with a bar plot of the count of items in each category.
#' @import ggplot2
#' @importFrom dplyr mutate
#' @importFrom dplyr arrange
#' @importFrom magrittr %>%
#'
#' @author "haytham omar  email: <haytham@rescaleanalytics.com>"
#' @note this is the first version of the inventorize package, all the fucntions are common knowlege for supply chain without
#' any academic contribution from my side, the aim is to facilitate and ease much of the bookkeeping that is endured during stock analysis.
#' @export
#' @examples
#'productmix(SKUs=c(1:100),sales=runif(100,1,1000),revenue = rnorm(100,200,10),na.rm=TRUE)




productmix<- function(SKUs,sales,revenue,na.rm=TRUE){

  store<-data.frame(sku=SKUs,sales=sales,revenue=revenue)

  store$sales_mix<- store$sales/sum(store$sales)
  store<-store %>% arrange(desc(sales))
  store$comulative_sales<-cumsum(store$sales_mix)
  store
  store<- store %>% arrange(desc(revenue))
  store$revenue_mix<- store$revenue/sum(store$revenue,na.rm = TRUE)
  store
  store$comulative_revenue<- cumsum(store$revenue_mix)

  ### for abc sales
  for( i in 1:nrow(store)){

    store$sales_category[i]<- if( store$comulative_sales[i]<0.8) {"A"
    } else if (store$comulative_sales[i]< 0.95) { "B"
    } else { "C"}
  }



  #### for abc revenue

  for( i in 1:nrow(store)){

    store$revenue_category[i]<- if( store$comulative_revenue[i]< '0.8') {"A"
    } else if (store$comulative_revenue[i]< '0.95') { "B"
    } else { "C"}
  }


  store$product_mix<- paste(store$sales_category,store$revenue_category,sep = "_")
  print(ggplot(store,aes(x=store$product_mix))+geom_bar(fill="indianred")+xlab("Category count")
        +theme_minimal()+ggtitle("Product_mix")+theme(plot.title = element_text(hjust = 0.5)))
  return(store)

}


