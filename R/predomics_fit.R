##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param data sparse matrix formatted data
##' @param outcome string representation of outcome variable name
##' @return list with model, test predictions, and test data

fit_predomics_model <- function(data, outcome, class_label){

  set.seed(8675309)

  # build terga2 classifier
  classifier <- terga2(nCores = 4, seed = 1, plot = TRUE)
  # rewrite description so the outputted file name is valid
  classifier$experiment$description <- outcome

  metadata <- data[[3]] %>%
               drop_na(outcome)

  biomdata <- data[[1]] %>%
               semi_join(metadata, by = "number_sample_id")

  #train the model on the training data
  trained_model <- predomics::fit(X = t(select(biomdata, -"number_sample_id")),
                                  y = t(if_else(metadata[outcome] == class_label, "yes", "no")),
                                  clf = classifier,
                                  cross.validate = TRUE,
                                  compute.importance = FALSE,
                                  nfolds = 10)

  digested_model <- digest(obj = trained_model, penalty = 0.75/100, plot = FALSE)
  best_model <- digested_model$best$model

  predictions <- evaluateModel(mod = best_model,
                               X = t(as.matrix(select(data[[2]], -"number_sample_id"))),
                               y = t(as.matrix(if_else(data[[4]][outcome] == class_label, TRUE, FALSE))),
                               clf = classifier,
                               eval.all = TRUE,
                               force.re.evaluation = TRUE, mode = "test")

  pred_auc <- auc_print(list("Model" = best_model,
                             "Predictor" =  predictions[["score_"]],
                             "Test_Data" = data[[2]],
                             "Response" = if_else(data[[4]][outcome] == class_label, TRUE, FALSE, FALSE)
                            )
                        )

  predomics_df <- tribble(~Outcome, ~Model, ~AUC,
                   outcome, "Predomics", pred_auc[1])

  return(predomics_df)

}
