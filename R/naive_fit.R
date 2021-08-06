##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param data dataframe formatted data
##' @param outcome string representation of outcome variable name
##' @return list with model, test predictions, and test data

fit_naive_model <- function (data){

  set.seed(8675309)

  #perform t-testing on the train data to identify the most informative taxa

  informative_taxa <- data[[1]][[1]] %>%
                      t() %>%
                      as("matrix") %>%
                      as_tibble() %>%
                      clean_names(case = "snake") %>%
                      mutate(outcome = data[[3]][[1]]) %>%
                      pivot_longer(cols = -outcome, names_to = "taxa") %>%
                      group_by(taxa) %>%
                      summarise(p_value = t.test(formula = value ~ outcome)[["p.value"]]) %>%
                      arrange(p_value) %>%
                      #drop least informative taxa from the data
                      drop_na() %>%
                      slice_head(prop = 0.2) %>%
                      select(taxa)

  model_data <- data[[1]][[1]] %>%
                t() %>%
                as("matrix") %>%
                as_tibble() %>%
                clean_names(case = "snake") %>%
                select(informative_taxa[[1]]) %>%
                bind_cols(as_tibble_col(data[[3]][[1]], column_name = "outcome"))

  naive_model <- glm(formula = outcome ~ .,
                     data = model_data,
                     family = binomial(link = "logit"))

  test_data <- data[[2]][[1]] %>%
                t() %>%
                as("matrix") %>%
                as_tibble() %>%
                clean_names(case = "snake") %>%
                bind_cols(as_tibble_col(data[[4]][[1]], column_name = "outcome"))

  naive_prediction <- predict(naive_model, newdata = test_data, type = "response")

  naive_calibration <- val.prob(p = naive_prediction, y = test_data[["outcome"]], group = TRUE)

  naive_auc <- auc_print(list("Model" = naive_model,
                           "Predictor" =  naive_prediction,
                           "Test_Data" = NA,
                           "Response" = data[[4]][[1]])
                        )

  naive_df <- tribble(~Outcome, ~Model, ~AUC, ~Calibration,
                   data[[5]], "Naive Logistic Regression", naive_auc[1], naive_calibration)

  return(naive_df)
}
