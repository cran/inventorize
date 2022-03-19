
#' abc_dynamic
#'
#' Identyfing ABC category based on the pareto rule.
#' the function can have flexibility in defining the A,B thresholds. can be done on multiple splits for example
#' countries or stores
#' @param product, Vector that contains the product name .
#' @param key_to_split, logical and by default is False, otherwise a column that has a splitting dimension,
#' for example ; stores or cities
#' @param first_attribute , attribute to do the ABC analysis on, for example sales quantity
#' @param second_attribute , attribute to do the ABC analysis on .for example profit, the default is FALSE
#' @param A , changing the default threshold for A category which is 0.8, the default is FALSE
#' @param B , changing the default threshold for B category which is 0.95, the default is FALSE
#' @return a dataframe that contains ABC categories.
#' @importFrom stats dnorm
#' @importFrom stats lm
#' @importFrom stats median
#' @importFrom stats optim
#' @importFrom stats optimize
#' @importFrom stats pnorm
#' @importFrom stats ppois
#' @importFrom stats predict
#' @importFrom stats qnorm
#' @importFrom plyr ldply
#' @import dplyr
#' @importFrom magrittr %>%
#' @author "haytham omar  email: <haytham@rescaleanalytics.com>"
#' @note this is the second version of the inventorize package, all the fucntions are  without
#' any academic contribution from my side, the aim is to facilitate and ease much of the bookkeeping that is endured during stock analysis.
#' @export
#' @examples
#' abc_dynamic(c(1:1000), rep(seq(1:10),100), runif(1000,4,10000),rnorm(1000,100,20))




abc_dynamic<- function(product,key_to_split=F,first_attribute,second_attribute=F,A= F,B=F){
  A<- ifelse(A ==F,0.8,A)
  B<-ifelse(B ==F,0.95,B)
  data<- data.frame(product,key_to_split,first_attribute,second_attribute)   
  
  if(key_to_split[1]==F & second_attribute[1] ==F){
    data1<- data %>% arrange(desc(first_attribute))
    data1$perc<- data1$first_attribute/sum(data1$first_attribute)
    data1$comu<- cumsum(data1$perc)
    data1<-data1 %>% mutate(category=dplyr::case_when(comu <= A ~ 'A',
                                               comu <= B ~ 'B',
                                               comu > B ~'C'           
    ))
    data1<- data1 %>% arrange(desc(first_attribute))
    
    a<- data1
  } else if (key_to_split[1]==F & second_attribute[1] !=F){
    data1<- data %>% arrange(desc(first_attribute))
    data1$perc_first_attribute<- data1$first_attribute/sum(data1$first_attribute)
    data1$comu_first_attribute<- cumsum(data1$perc_first_attribute)
    data1<-data1 %>% mutate(category_first_attribute=dplyr::case_when(comu_first_attribute <= A ~ 'A',
                                                       comu_first_attribute <= B ~ 'B',
                                                       comu_first_attribute > B ~'C'           
    ))
    
    data1<- data1 %>% arrange(desc(second_attribute)) 
    data1$perc_second_attribute<- data1$second_attribute/sum(data1$second_attribute)
    data1$comu_second_attribute<- cumsum(data1$perc_second_attribute)
    data1<-data1 %>% mutate(category_second_attribute=dplyr::case_when(comu_second_attribute <= A ~ 'A',
                                                     comu_second_attribute <= B ~ 'B',
                                                     comu_second_attribute > B ~'C'           
    ))
    data1$second_attribute_first_attribute<- paste(data1$category_second_attribute,data1$category_first_attribute,sep = '_')
    data1<- data1 %>% arrange(desc(first_attribute))
    
    a<- data1
    
  } else if (key_to_split[1]!=F & second_attribute[1] ==F){
    
    data_splitted<- split(data,data$key_to_split) 
    segmeted_list<- list()
    
    
    for (i in 1: length(data_splitted)){
      
      data1<- data_splitted[[i]]
      data1<- data1 %>% arrange(desc(first_attribute))
      data1$perc<- data1$first_attribute/sum(data1$first_attribute)
      data1$comu<- cumsum(data1$perc)
      data1<-data1 %>% mutate(category=dplyr::case_when(comu <= A ~ 'A',
                                                 comu <= B ~ 'B',
                                                 comu > B ~'C'           
      ))
      data1<- data1 %>% arrange(desc(first_attribute))
      
      segmeted_list[[i]]<- data1
    }
    a<- ldply(segmeted_list,data.frame)
  } else {
    
    data_splitted<- split(data,data$key_to_split) 
    segmeted_list<- list()
    
    for (i in 1: length(data_splitted)){
      
      data1<- data_splitted[[i]]
      data1<- data1 %>% arrange(desc(first_attribute))
      data1$perc_first_attribute<- data1$first_attribute/sum(data1$first_attribute)
      data1$comu_first_attribute<- cumsum(data1$perc_first_attribute)
      data1<-data1 %>% mutate(category_first_attribute=dplyr::case_when(comu_first_attribute <= A ~ 'A',
                                                         comu_first_attribute <= B ~ 'B',
                                                         comu_first_attribute > B ~'C'           
      ))
      
      data1<- data1 %>% arrange(desc(second_attribute)) 
      data1$perc_second_attribute<- data1$second_attribute/sum(data1$second_attribute)
      data1$comu_second_attribute<- cumsum(data1$perc_second_attribute)
      data1<-data1 %>% mutate(category_second_attribute=dplyr::case_when(comu_second_attribute <= A ~ 'A',
                                                       comu_second_attribute <= B ~ 'B',
                                                       comu_second_attribute > B ~'C'           
      ))
      data1$second_attribute_first_attribute<- paste(data1$category_second_attribute,data1$category_first_attribute,sep = '_')
      data1<- data1 %>% arrange(desc(first_attribute))
      
      segmeted_list[[i]]<- data1
    }
    a<- ldply(segmeted_list,data.frame)
    
  }
  return(a)
}





