##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param phyloseq_object phyloseq object containing sample and taxa proportions
##' @param partition_object rsample object that defines the train/test split for this experiment
##' @param outcome dataframe row that has the outcome name and level that we wish to predict
##'
##' @return list structure with the train and test data and outcomes
##'

create_experiment_data <- function(phyloseq_object, site_variable, site_name, partition_type = "CV", outcome){

  # select out samples from desired sites
  site_samples <- sample_data(phyloseq_object) %>%
                  as_tibble(rownames = "row_id") %>%
                  clean_names("snake") %>%
                  #get records from the correct body site
                  filter(.data[[site_variable]] %in% site_name) %>%
                  #get records that do not have missing levels
                  filter(.data[[outcome[[1]]]] != outcome[[3]]) %>%
                  filter(!is.na(.data[[outcome[[1]]]])) %>%
                  select(row_id)


  if (partition_type == "CV"){
    #perform v-fold splitting
    partition_object <- vfold_cv(site_samples, v = 10)[["splits"]]
  } else if (partition_type == "80/20"){
    #create the initial sample split
    partition_object <- list(initial_split(site_samples, prop = 0.80))
  }


  experiments <- map_dfr(partition_object, ~ generate_experiment(.x, phyloseq_object, outcome))

  return(experiments)
}

generate_experiment <- function(partition_object, phyloseq_object, outcome){
  #produce the ids that belong in the train and test data
  train_ids <- training(partition_object)
  test_ids <- testing(partition_object)

  #filter the phylo_seq object by the train and test ids
  train_phyloseq_raw <- prune_samples(train_ids[["row_id"]], phyloseq_object)

  train_missing <- !is.na(sample_data(train_phyloseq_raw)[[outcome[[1]]]])

  train_phyloseq <- prune_samples(train_missing, train_phyloseq_raw)

  test_phyloseq_raw <- prune_samples(test_ids[["row_id"]], phyloseq_object)

  test_missing <- !is.na(sample_data(test_phyloseq_raw)[[outcome[[1]]]])

  test_phyloseq <- prune_samples(test_missing, test_phyloseq_raw)

  #extract the train and test samples & select the variable of interest
  train_samples <- sample_data(train_phyloseq)[[outcome[[1]]]] == outcome[[2]]
  test_samples <- sample_data(test_phyloseq)[[outcome[[1]]]] == outcome[[2]]

  #perform t-testing on the train data to identify the most informative taxa

  informative_taxa <- otu_table(train_phyloseq) %>%
                      t() %>%
                      as("matrix") %>%
                      as_tibble() %>%
                      mutate(outcome = train_samples) %>%
                      pivot_longer(cols = -outcome, names_to = "taxa") %>%
                      group_by(taxa) %>%
                      summarise(p_value = t.test(formula = value ~ outcome)[["p.value"]]) %>%
                      arrange(p_value) %>%
                      #drop least informative taxa from the data
                      drop_na() %>%
                      slice_head(prop = 0.9) %>%
                      select(taxa)

  #remove the least informative taxa from the data that will go to the experiment
  final_train_phyloseq <- prune_taxa(informative_taxa[[1]], train_phyloseq)
  final_test_phyloseq <- prune_taxa(informative_taxa[[1]], test_phyloseq)

  train_set <- as_tibble(final_train_phyloseq@otu_table, rownames = NA)
  test_set <- as_tibble(final_test_phyloseq@otu_table, rownames = NA)

  #return a list structure with the train and test data & outcomes

  tibble(Train_OTUs = list(otu_table(final_train_phyloseq)),
         Test_OTUS = list(otu_table(final_test_phyloseq)),
         Train_Samples = list(train_samples),
         Test_Samples = list(test_samples),
         Outcome = outcome[[1]])

  # return(list(otu_table(final_train_phyloseq),
  #             otu_table(final_test_phyloseq),
  #             train_samples,
  #             test_samples,
  #             outcome[[1]]))
}
