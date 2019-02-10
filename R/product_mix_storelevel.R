
#' productmix_storelevel
#'
#' Identyfing ABC category based on the pareto rule for both demand and selling price,a mix of nine categories are produced.
#'Identyfing ABC category based on the pareto rule.A category is up to 80%. B category is up 95% and C category is up to 100%.
#'in this fuction the data is splitted by store and a product mix is made on each store individually.
#'
#'
#'
#' @param  SKUs, charachter, a vector of SKU names.
#' @param  sales, vector, a vector of items sold per sku, should be the same number of rows as SKUs.
#' @param   revenue  vector, a vector of total revenue  per sku, should be the same number of rows as SKUs.
#' @param   storeofsku  vector, which store the SKU is sold at.should be the same number of rows as SKUs.
#'
#'
#' @param na.rm, logical and by default is TRUE
#'
#' @return a dataframe that contains ABC categories by store with a bar plot of the count of items in each category.
#' @import ggplot2
#' @importFrom dplyr mutate
#' @importFrom dplyr arrange
#' @importFrom magrittr %>%
#' @importFrom plyr ldply
#' @author "haytham omar  email: <haytham@rescaleanalytics.com>"
#' @note this is the first version of the inventorize package, all the fucntions are common knowlege for supply chain without
#' any academic contribution from my side, the aim is to facilitate and ease much of the bookkeeping that is endured during stock analysis.
#' @export
#' @examples
#' productmix_storelevel(c(1:1000),sales = runif(1000,4,10000),
#' revenue  = rnorm(1000,100,20),storeofsku = rep(seq(1:10),100))









productmix_storelevel<- function(SKUs,sales,revenue,storeofsku,na.rm=TRUE){

  productmix<- function(store) {



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

  return(store)
}

productdata<- data.frame(sku=SKUs,sales=sales,revenue=revenue,storeofsku=storeofsku)

productdata_splitted<-split(productdata,as.factor(productdata$storeofsku))

matrix_list<-list()
for (i in 1:length(productdata_splitted)){
  matrix_list[[i]]<- matrix(NA,nrow = nrow(productdata_splitted[[i]]),ncol = ncol(productdata_splitted[[i]]))
}

for (i in 1:length(productdata_splitted)){

  matrix_list[[i]]<-productmix(productdata_splitted[[i]])

}


productmix_bystore<-ldply(matrix_list,data.frame)
print(ggplot(productmix_bystore,aes(x=as.factor(storeofsku),fill=product_mix))+geom_bar()+xlab("Category count at store level")
      +theme_minimal()+ggtitle("Product mix at store level ")+theme(plot.title = element_text(hjust = 0.5)))
return(productmix_bystore)


}


