##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param data dataframe formatted data
##' @param outcome string representation of outcome variable name
##' @return list with model, test predictions, and test data

fit_rf_model <- function (data, outcome, class_label){

  set.seed(8675309)

  variances <- select(data[[1]], starts_with("x")) %>%
    map_dbl(var) %>%
    enframe() %>%
    arrange(desc(value)) %>%
    slice(1:7000)

  ranger_data <- data[[1]] %>%
    select(all_of(c(outcome, variances$name))) %>%
    #change this to not throw out the unknown values
    mutate(across(outcome, ~ if_else(.x == class_label, TRUE, FALSE))) %>%
    drop_na(outcome)

  ranger_data[[outcome]] %<>% factor()

  model_formula <- as.formula(glue("{outcome} ~ ."))

  rf_model <- ranger(formula = model_formula,
                     data = ranger_data,
                     num.trees = 100,
                     probability = TRUE,
                     save.memory = TRUE)

  rf_prediction <- predict(rf_model, data = data[[2]])

  rf_auc <- auc_print(list("Model" = rf_model,
                           "Predictor" =  rf_prediction[["predictions"]][,2],
                           "Test_Data" = NA,
                           "Response" = data[[2]][outcome] == class_label
                          )
                      )

  rf_df <- tribble(~Outcome, ~Model, ~AUC,
                      outcome, "Random Forest", rf_auc[1])

  return(rf_df)
}
