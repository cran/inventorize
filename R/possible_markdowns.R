#' possible_markdowns
#'
#'a markdown model 
#'This is a markdown model proposed in
#'Walker, John. "A model for determining price markdowns of seasonal merchandise." 
#'Journal of Product & Brand Management (1999), the idea that it is possible for seasonal merchandise to forecast how much
#'for a specific product can be left at the end of the season. based on the sales rate in the periods of the selling season.
#'for example, if a seasonal shirt initial buying quantity is 500, during the the first two weeks we sold 100 and the season for this
#'shirt is 6 weeks, then it is possible to forecast for a one time shot product how much is expected to be left with at the end of the 
#'season (at the end of the 6 weeks), the function applies the algorithm in walker (1999), the returning value is a classification of
#'the item if it is a slow moving or a regular item. also the possible markdowns that can be applied.
#' (only markdowns where there is a economic viability) and this can be a dynamic markdown process where the process can be repeated
#' every week, preferably when the product changes its status from Regular to slow moving. if the markdown recommendation is for example 
#' 0.9 then it means that the new price is 90 % of the original price. and so on for the following week, hence the dynamic process.
#'
#' @param  begining_inventory, inventory at the beginning of the season before selling.
#' @param  weeks, number of weeks in the season.
#' @param   current_week, the end of the current week.
#' @param   inventory_at_week,  inventory at the end of the current week.
#' @param expected_at_season_end, expected inventory left for salvage or writing off at the end of the season, if the forecast is
#' below it, then it becomes a regular item if the forecast is higher than expected at season end then it becomes a slow moving item.
#' @param  plot  Default is false,if true, a plot is generated
#' @importFrom stats dnorm
#' @importFrom stats lm
#' @importFrom stats median
#' @importFrom stats optim
#' @importFrom stats optimize
#' @importFrom stats pnorm
#' @importFrom stats ppois
#' @importFrom stats predict
#' @importFrom stats qnorm
#' @importFrom stats qpois
#' @importFrom stats sd
#' @import ggplot2
#' @importFrom magrittr %>%
#' @importFrom  plotly ggplotly
#' @return a dataframe that contains all tthe possible economically viable markdowns.
#' @author "haytham omar  email: <haytham@rescaleanalytics.com>"
#' @export
#' @examples
#' possible_markdowns(begining_inventory=1000,weeks=16,
#' current_week=2,inventory_at_week=825,expected_at_season_end=150,plot=TRUE)


possible_markdowns<- function(begining_inventory,
                              weeks,
                              current_week,
                              inventory_at_week,
                              expected_at_season_end,plot=TRUE){
margin_sales_increase<- NULL
end_of_season_inventory<- function(begining_inventory,
                                   weeks,
                                   current_week,
                                   inventory_at_week,
                                   expected_at_season_end){
  
  fc= (inventory_at_week/begining_inventory)^(1/current_week)
  expected_inventory=   inventory_at_week * fc ^ (weeks - current_week)
  
  Verdict<- ifelse(expected_inventory < expected_at_season_end,
                   'Regular','Slow moving')
  return(c(expected_inventory,Verdict))
}

ending_inventory<-as.numeric(end_of_season_inventory(begining_inventory,
                                                     weeks,
                                                     current_week,
                                                     inventory_at_week,
                                                     expected_at_season_end)[1])
status<-end_of_season_inventory(begining_inventory,
                                weeks,
                                current_week,
                                inventory_at_week,
                                expected_at_season_end)[2]

markdown_critical<- function(end_of_period_inventory_ratio,week,periods,percentage_reduction){
  i_ratio=end_of_period_inventory_ratio
  reduction=percentage_reduction
  lower1= 1- (i_ratio)^(1/week)
  higher1= 1-(1- ((1-(i_ratio)^((periods-week)/week))/reduction))^(1/(periods-week))
  return(higher1/lower1)
  
}
markdown<- c(seq(from=0,to=1,length.out=101))
margin<- c(rep(NA,length(markdown)))
for (i in 1:length(markdown)){
  margin[i]<- markdown_critical(inventory_at_week/begining_inventory,current_week,weeks,markdown[i])
}

possible_markdowns_data<- data.frame(markdown= markdown,margin_sales_increase= margin,status,expected_inventory_at_end_of_season=ending_inventory)
if(plot==TRUE){
  possible_markdowns_data1<- possible_markdowns_data[is.nan(possible_markdowns_data$margin_sales_increase)==FALSE,]
  print(plotly::ggplotly(possible_markdowns_data1 %>% ggplot(aes(x= markdown,y=margin_sales_increase))+geom_col(fill='darkred')+
                           
                           theme_classic()+ggtitle(paste('Markdowns proposed Vs Resulting increase in sales',current_week))+ylab('expcted increase of sales if markdown is applied')+
                           xlab("Markdowns")+
                           theme(plot.title = element_text(hjust = 0.5))))
}

if(status== 'Slow moving'){
  a<-('it is recommended to apply markdowns as the item is forecasted to be a slow moving item with excess inventory at end of season,
      the markdown reprsents the new selling price percentage from full original price
      and margin sales increase column is the expected increase in sales to achieve  that is economicaly viable the following week')
} else {
  a<- ' it is not recommended to do markdown at this week as the item is forecasted to have a regular selling pattern '
}
return(list(possible_markdowns_data,a))
}



