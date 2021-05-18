##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param file_path_meta file path to a tab delimited metadata file format
##' @param file_path_biom file path to the .biom file w/ OTU data
##' @param site_variable name of sample site variable in metadata
##' @param site_name sample site name in metadata
##' @param test_prop proportion of sample to partition for testing (default 0.2)
##'
##' @return a list structure with the test, train OTU data in sparseMatrix format, and metadata in a tibble

load_data <- function(file_path_meta, file_path_biom, site_variable, site_name, test_prop = 0.2){

  AG_meta <- read_delim(file_path_meta, "\t", na = "no_data")
  AG_biom <- read_biom(file_path_biom)

  # select out samples from desired sites
  site_samples <- AG_meta %>%
    filter(.data[[site_variable]] %in% site_name) %>%
    clean_names("snake")

  #biom data comes in as a sparseMatrix from the Matrix package

  biom_matrix <- as(biom_data(AG_biom), "sparseMatrix") %>% t()

  #partition the metadata
  sample_split <- initial_split(site_samples, prop = 1 - test_prop)

  return(list(biom_matrix, sample_split))
}
