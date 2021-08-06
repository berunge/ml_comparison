##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param data dataframe formatted data
##' @param outcome string representation of outcome variable name
##' @return list with model, test predictions, and test data

fit_rf_model <- function (data){

  set.seed(8675309)

  model_data <- data[[1]][[1]] %>%
                t() %>%
                as_tibble() %>%
                clean_names(case = "snake") %>%
                bind_cols(as_tibble_col(data[[3]][[1]], column_name = "outcome"))

  rf_model <- ranger(formula = outcome ~ .,
                     data = model_data,
                     num.trees = 100,
                     probability = TRUE,
                     save.memory = TRUE)

  test_data <- data[[2]][[1]] %>%
               t() %>%
               as_tibble() %>%
               clean_names(case = "snake") %>%
               bind_cols(as_tibble_col(data[[4]][[1]], column_name = "outcome"))

  rf_prediction <- predict(rf_model, data = test_data)

  rf_calibration <- val.prob(p =  rf_prediction[["predictions"]][,2], y = test_data[["outcome"]], group = TRUE)

  rf_auc <- auc_print(list("Model" = rf_model,
                           "Predictor" =  rf_prediction[["predictions"]][,2],
                           "Test_Data" = NA,
                           "Response" = data[[4]][[1]]
                          )
                      )

  rf_df <- tribble(~Outcome, ~Model, ~AUC, ~Calibration,
                      data[[5]], "Random Forest", rf_auc[1], rf_calibration)

  return(rf_df)
}
