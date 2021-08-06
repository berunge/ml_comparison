

experiment_report_makerows <- function(experiment_data){

  #pull the relevant data from the list experiment data

  #sample sizes
  train_sample <- length(experiment_data[[3]][[1]])
  test_sample <- length(experiment_data[[4]][[1]])
  overall_sample <- train_sample + test_sample

  #Cases & Controls
  train_cases <- sum(experiment_data[[3]][[1]])
  train_controls <- train_sample - train_cases

  test_cases <- sum(experiment_data[[4]][[1]])
  test_controls <- test_sample - test_cases

  #Taxa Information
  number_taxa <- dim(experiment_data[[1]][[1]])[1]

  return(tribble(~Outcome, ~Overall_Sample, ~Train_Sample, ~Test_Sample, ~Train_Cases, ~Train_Controls, ~Test_Cases, ~Test_Controls, ~Number_Taxa,
                 experiment_data[[5]], overall_sample, train_sample, test_sample, train_cases, train_controls, test_cases, test_controls, number_taxa))

}


experiment_report <- function(rows){

  # for each experiment, report overall sample size, train sample size, test sample size
  #                             Train Cases, Train Controls, Test Cases, Test Controls

  experiment_n <- rows %>%
                  select(Outcome, Overall_Sample, Train_Sample, Test_Sample) %>%
                  gt(groupname_col = "Outcome")

  experiment_test_outcomes <- rows %>%
                              select(Outcome, Test_Sample, Test_Cases, Test_Controls) %>%
    gt(groupname_col = "Outcome")

  experiment_train_outcomes <- rows %>%
                               select(Outcome, Train_Sample, Train_Cases, Train_Controls) %>%
                               gt(groupname_col = "Outcome")

  experiment_taxa <- rows %>%
                     select(Outcome, Number_Taxa) %>%
                     gt(groupname_col = "Outcome")

  return(list(experiment_n, experiment_test_outcomes, experiment_train_outcomes, experiment_taxa))
}
