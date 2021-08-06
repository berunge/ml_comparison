##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param data sparse matrix formatted data
##' @param outcome string representation of outcome variable name
##' @return list with model, test predictions, and test data

fit_boost_model <- function(data){

  set.seed(8675309)

  metadata <- as.matrix(data[[3]][[1]])

  biomdata <- t(as.matrix(data[[1]][[1]]))

  boost_tuner <- xgb.cv(data = biomdata,
                        label = metadata,
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

  boost_model <- xgboost(data = biomdata,
                         label = metadata,
                         nrounds = boost_tuner$best_iteration,
                         params = list(
                           objective = 'binary:logistic',
                           eval_metric = 'auc',
                           max_depth = 5,
                           eta = 0.0002,
                           colsample_bynode = 1/50,
                           subsample = 1/3
                         ),
                         verbose = TRUE
  )

  boost_prediction <- predict(boost_model, t(as.matrix(data[[2]][[1]])))

  boost_calibration <- val.prob(p = boost_prediction, y = data[[4]][[1]], group = TRUE)

  boost_auc <- auc_print(list("Model" = boost_model,
                              "Predictor" =  boost_prediction,
                              "Test_Data" = data[[2]][[1]],
                              "Response" = data[[4]][[1]]
                              )
                         )

  boost_df <- tribble(~Outcome, ~Model, ~AUC, ~Calibration,
                      data[[5]], "XGBoost", boost_auc[1], boost_calibration)

  return(boost_df)

}
