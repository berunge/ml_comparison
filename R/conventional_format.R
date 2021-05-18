##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param data list output by load_data w/ sparse matrix of biom data and rsplit object with metadata
##' @return list with train and test datasets as tibbles

conventional_format <- function(data){

  #extract biom data and convert into dataframe

  sample_biom <- data[[1]] %>%
    as('matrix') %>%
    as_tibble(rownames = "number_sample_id") %>%
    clean_names("snake")

  #extract metadata
  train_meta <- training(data[[2]])
  test_meta <- testing(data[[2]])

  #create combined test and train sets from split metadata by joining on sampleID

  train_set <- train_meta %>%
    left_join(sample_biom, by = "number_sample_id")

  test_set <- test_meta %>%
    left_join(sample_biom, by = "number_sample_id")

  return(list(train_set, test_set))

}
