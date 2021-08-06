##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param response the true classification of the subject
##' @param predictior the predicted classification of the subject
##' @param model string name of the model used in the prediction
##'
##'

auc_print <- function(output_structure){

  roc_object <- roc(response = output_structure[["Response"]],
                    predictor = output_structure[["Predictor"]],
                    auc = TRUE, plot = TRUE, direction = "<")

  print(roc_object)

  return(roc_object[["auc"]])

}
