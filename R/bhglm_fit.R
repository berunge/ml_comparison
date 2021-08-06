##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param data dataframe formatted data
##' @param outcome string representation of outcome variable name
##' @return list with model, test predictions, and test data

fit_bhglm_model <- function (data){

  set.seed(8675309)

  metadata <- data[[3]][[1]]

  biomdata <- as(data[[1]][[1]], "matrix") %>% t()

  bhglm_model <- glmNet(x = biomdata,
                        y = metadata,
                        family = "binomial")

  test_meta <- data[[4]][[1]]
  test_data <- as(data[[2]][[1]], "matrix") %>% t()

  bhglm_model$offset <- FALSE

  bhglm_prediction <- predict(bhglm_model, newx = test_data, type = "response", s = bhglm_model$lambda)

  bhglm_calibration <- val.prob(p = bhglm_prediction, y = test_meta, group = TRUE)

  bhglm_auc <- auc_print(list("Model" = bhglm_model,
                              "Predictor" =  bhglm_prediction,
                              "Test_Data" = NA,
                              "Response" = test_meta)
  )

  bhglm_df <- tribble(~Outcome, ~Model, ~AUC,~Calibration,
                      data[[5]], "Logistic Lasso", bhglm_auc[1], bhglm_calibration)

  return(bhglm_df)
}
