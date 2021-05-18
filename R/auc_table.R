##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param rf_auc
##' @param boost_auc
##' @param predomics_auc
##' @param outcomes
##'
##'

auc_table <- function(rf_auc, boost_auc, predomics_auc){


  df_comparison <- bind_rows(rf_auc, boost_auc, predomics_auc)

  auc_comparison_table <- gt(data = df_comparison, groupname_col = "Outcome", rowname_col = "Model")

  return(auc_comparison_table)
}
