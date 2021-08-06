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

auc_table <- function(naive_auc_cv, bhglm_auc_cv, rf_auc_cv, boost_auc_cv, predomics_auc_cv){

  cv_summary <- bind_rows(naive_auc_cv, bhglm_auc_cv, rf_auc_cv, boost_auc_cv, predomics_auc_cv) %>%
                group_by(Outcome, Model) %>%
                summarize(Mean_AUC = mean(AUC))

  auc_tableready <- cv_summary

  auc_comparison_table <- gt(data = auc_tableready, groupname_col = "Outcome", rowname_col = "Model")
  
  cv_calibration_plots <- bind_rows(naive_auc_cv, bhglm_auc_cv, rf_auc_cv, boost_auc_cv, predomics_auc_cv) %>% 
                          select(Outcome, Model, Calibration)
                          
  return(list(auc_comparison_table, cv_calibration_plots))
}
