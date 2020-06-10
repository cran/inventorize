#' inventorize: Inventory Analytics And Cost Calculations.
#'
#' inventory analytics,revenue management and cost calculations for SKUs.
#'
#' @docType package
#' @name inventorize
"_PACKAGE"

# Update this function call
utils::globalVariables(c("category","%>%","arrange","mutate","desc","aes","xlab","geom_bar","ggplot","theme_minimal","product_mix","ldply",
                         "ggplotly","Product_name","Measure","Value","revenue","profit","predicted_linear","predicted_logit",
                         "revenue_linear","revenue_logit","lm_p","logit_p"))
