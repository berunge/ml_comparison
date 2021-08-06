##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param file_path_sample file path to a tab delimited sample data
##' @param site_variable name of sample site variable in metadata
##' @param test_prop proportion of data to reserve as test data
##'
##' @return rsplit object of sample metadata
##'


single_partition <- function(file_path_sample, site_variable, site_name, outcome_variable, outcome_name, test_prop = 0.2){

  #load in metadata
  metadata <- read_delim(file_path_sample, "\t", na = "no_data", col_types = cols(.default = "c"))

  # select out samples from desired sites
  site_samples <- metadata %>%
                  filter(.data[[site_variable]] %in% site_name) %>%
                  clean_names("snake") %>%
                  select(number_sample_id)

  #create the initial sample split
  sample_split <- initial_split(site_samples, prop = 1 - test_prop)

  return(sample_split)
}

##' @title
##' @param file_path_sample file path to a tab delimited sample data
##' @param site_variable name of sample site variable in metadata
##' @param cv_folds number of crossval folds to create
##'
##' @return rsample v-fold cross-validation data structure
##'

cv_partition <-function(file_path_sample, site_variable, site_name, cv_folds = 10){

  #load in metadata
  metadata <- read_delim(file_path_sample, "\t", na = "no_data", col_types = cols(.default = "c"))

  # select out samples from desired sites
  site_samples <- metadata %>%
                  filter(.data[[site_variable]] %in% site_name) %>%
                  clean_names("snake") %>%
                  select(number_sample_id)

  #perform v-fold splitting
  sample_split <- vfold_cv(site_samples, v = cv_folds)[["splits"]]

  return(sample_split)
}
