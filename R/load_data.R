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
  AG_biom <- import_biom(file_path_biom)

  # select out samples from desired sites
  site_samples <- AG_meta %>%
    filter(.data[[site_variable]] %in% site_name) %>%
    clean_names("snake")

  #condense OTUs to species level
  #compute relative abundances

  biom_matrix <- tax_glom(AG_biom, "Rank7") %>%
                 transform_sample_counts( function(x) x / sum(x)) %>%
                 otu_table() %>%
                 t()

  #partition the metadata
  sample_split <- initial_split(site_samples, prop = 1 - test_prop)

  return(list(biom_matrix, sample_split))
}

load_data_cv <- function(file_path_meta, file_path_biom, site_variable, site_name, folds = 10){

  AG_meta <- read_delim(file_path_meta, "\t", na = "no_data")
  AG_biom <- import_biom(file_path_biom)

  # select out samples from desired sites
  site_samples <- AG_meta %>%
    filter(.data[[site_variable]] %in% site_name) %>%
    clean_names("snake")

  #condense OTUs to species level

  biom_matrix <-tax_glom(AG_biom, "Rank7") %>%
                transform_sample_counts( function(x) x / sum(x)) %>%
                otu_table() %>%
                t()

  #partition the metadata
  sample_split <- vfold_cv(site_samples, v = folds)$splits

  sample <- map(sample_split, ~ list(biom_matrix, .x))

  return(sample)
}
