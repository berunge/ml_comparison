library(targets)
library(tarchetypes)
source("./packages.R")
# This is an example _targets.R file. Every
# {targets} pipeline needs one.
# Use tar_script() to create _targets.R and tar_edit()
# to open it again for editing.
# Then, run tar_make() to run the pipeline
# and tar_read(summary) to view the results.

## Load functions
lapply(list.files("./R", full.names = TRUE), source)


# Set target-specific options such as packages.
tar_option_set()


list(
  # Import .biom data, clean, and partition into test and train data
  #partition is a list structure containing OTU data in sparseMatrix format and rsplit object with metadata
  tar_target(partition, load_data(file_path_meta = "data/source/ag.txt",
                        file_path_biom = "data/source/AG.biom",
                        site_variable = "BODY_SITE",
                        site_name = "UBERON:feces",
                        test_prop = 0.2)),
  #conform train and test data into format acceptable by boost and random forest models
  tar_target(conv_data, conventional_format(data = partition)),

  #conform train and test data into format acceptable by predomics model
  tar_target(pred_data, predomics_format(data = partition)),

  tar_group_by(class_description, create_outcomes(), outcome),

  tar_target(boost_output, fit_boost_model(pred_data, outcome = class_description[["outcome"]], class_label = class_description[["levels"]]), pattern = map(class_description)),
  tar_target(rf_output, fit_rf_model(conv_data, outcome = class_description[["outcome"]], class_label = class_description[["levels"]]), pattern = map(class_description)),
  tar_target(predomics_output, fit_predomics_model(pred_data, outcome = class_description[["outcome"]], class_label = class_description[["levels"]]), pattern = map(class_description)),

  #print out table of AUCs

  tar_target(auc_table_out, auc_table(rf_output, boost_output, predomics_output))
)
