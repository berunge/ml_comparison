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

auc_table <- function(rf_auc, boost_auc, predomics_auc,
                      rf_auc_cv, boost_auc_cv, predomics_auc_cv){


  df_comparison <- bind_rows(rf_auc, boost_auc, predomics_auc)

  cv_summary <- bind_rows(rf_auc_cv, boost_auc_cv, predomics_auc_cv) %>%
                group_by(Outcome, Model) %>%
                summarize(Mean_AUC = mean(AUC))

  auc_tableready <- df_comparison %>%
                    right_join(cv_summary, by = c("Outcome", "Model"))

  auc_comparison_table <- gt(data = auc_tableready, groupname_col = "Outcome", rowname_col = "Model")

  return(auc_comparison_table)
}
