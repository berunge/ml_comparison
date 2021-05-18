##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param data list output by load_data w/ sparse matrix of biom data and rsplit object with metadata
##' @return list with train and test datasets as sparse Matrices and train and test metadata

predomics_format <- function(data){

  #extract biom data
  sample_biom <- data[[1]] %>%
    as('matrix') %>%
    as_tibble(rownames = "number_sample_id")

  #extract metadata
  train_meta <- training(data[[2]])
  test_meta <- testing(data[[2]])

  train_set <- sample_biom %>%
    semi_join(train_meta, by = "number_sample_id")

  test_set <- sample_biom %>%
    semi_join(test_meta, by = "number_sample_id")

  return(list(train_set, test_set, train_meta, test_meta))
}
