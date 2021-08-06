##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param data sparse matrix formatted data
##' @param outcome string representation of outcome variable name
##' @return list with model, test predictions, and test data

fit_predomics_model <- function(data){

  set.seed(8675309)

  # build terga2 classifier
  classifier <- terga2(nCores = 1, seed = 1, plot = TRUE, evalToFit = "auc_")
  # rewrite description so the outputted file name is valid
  classifier$experiment$description <- data[[5]]

  metadata <- data[[3]][[1]]

  biomdata <- as(data[[1]][[1]], "matrix")

  #train the model on the training data
  trained_model <- predomics::fit(X = biomdata,
                                  y = metadata,
                                  clf = classifier,
                                  cross.validate = TRUE,
                                  compute.importance = FALSE,
                                  nfolds = 10)

  digested_model <- digest(obj = trained_model, penalty = 0.75/100, plot = FALSE)
  best_model <- digested_model$best$model

  predictions <- evaluateModel(mod = best_model,
                               X = as(data[[2]][[1]], "matrix"),
                               y = data[[4]][[1]],
                               clf = classifier,
                               eval.all = TRUE,
                               force.re.evaluation = TRUE, mode = "test")
                               
  pred_calibration <- list(FALSE)
  try(pred_calibration <- val.prob(p = predicted_score, y = data[[4]][[1]], group = TRUE), silent = TRUE)

  pred_auc <- auc_print(list("Model" = best_model,
                             "Predictor" =  predictions[["score_"]],
                             "Test_Data" = data[[2]][[1]],
                             "Response" = data[[4]][[1]]
                            )
                        )

  predomics_df <- tribble(~Outcome, ~Model, ~AUC, ~Calibration,
                   data[[5]], "Predomics", pred_auc[1], pred_calibration)

  return(predomics_df)

}
