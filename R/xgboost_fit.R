##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param data matrix formatted data
##' @param outcome string representation of outcome variable name
##' @return list of lists with model, test predictions, and test data

xgboost_fit <- function(data, outcomes){

  model_lists <- map2(labels(outcomes), outcomes, ~ fit_boost_model(data, .x, .y))

  return(model_lists)
}

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param data sparse matrix formatted data
##' @param outcome string representation of outcome variable name
##' @return list with model, test predictions, and test data

fit_boost_model <- function(data, outcome, class_label){

  set.seed(8675309)

  metadata <- data[[3]] %>%
               drop_na(outcome)

  biomdata <- data[[1]] %>%
               semi_join(metadata, by = "number_sample_id")

  boost_tuner <- xgb.cv(data = as.matrix(select(biomdata, -"number_sample_id")),
                        label = as.matrix(if_else(metadata[outcome] == class_label, TRUE, FALSE)),
                        nrounds = 10000,
                        nfold = 10,
                        early_stopping_rounds = 25,
                        params = list(
                          objective = 'binary:logistic',
                          eval_metric = 'auc',
                          max_depth = 1,
                          eta = 0.0000001,
                          colsample_bynode = 1/5,
                          subsample = 1
                        ),
                        verbose = TRUE
  )

  boost_model <- xgboost(data = as.matrix(select(biomdata, -"number_sample_id")),
                         label = as.matrix(if_else(metadata[outcome] == class_label, TRUE, FALSE)),
                         nrounds = boost_tuner$best_iteration,
                         params = list(
                           objective = 'binary:logistic',
                           eval_metric = 'auc',
                           max_depth = 2,
                           eta = 0.0002,
                           colsample_bynode = 1/50,
                           subsample = 1/3
                         ),
                         verbose = TRUE
  )

  boost_prediction <- predict(boost_model, as.matrix(select(data[[2]], -"number_sample_id")))

  boost_auc <- auc_print(list("Model" = boost_model,
                              "Predictor" =  boost_prediction,
                              "Test_Data" = data[[2]],
                              "Response" = if_else(data[[4]][outcome] == class_label, TRUE, FALSE)
                              )
                         )

  boost_df <- tribble(~Outcome, ~Model, ~AUC,
                      outcome, "XGBoost", boost_auc[1])

  return(boost_df)

}
