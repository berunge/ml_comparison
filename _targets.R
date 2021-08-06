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

# data/update/11-packaged/fecal/100nt/all_participants/one_sample/1k/ag_1k_fecal.biom
# data/update/11-packaged/fecal/100nt/all_participants/one_sample/1k/ag_1k_fecal.txt

list(
  #load .biom and metadate to produce phyloseq object
  #Rank7 corresponds to species level, Rank6 is genus level
  tar_target(phyloseq_object, load_into_phyloseq(file_path_sample = "/data/scratch/berunge/AG_updated/ag_10k_fecal.txt",
                                                 file_path_biom = "/data/scratch/berunge/AG_updated/ag_10k_fecal.biom",
                                                 taxonomic_level = "Rank7")),
  #group by each outcome to be analyzed
  tar_group_by(class_description, create_outcomes(), outcome),

  #cross Validation code
  tar_target(cv_part_exp_ungroup,
             create_experiment_data(phyloseq_object = phyloseq_object,
                                    site_variable = "body_site",
                                    site_name = "UBERON:feces",
                                    partition_type = "CV",
                                    outcome = class_description),
             pattern = map(class_description)),

  tar_group_size(cv_part_exp,
                 cv_part_exp_ungroup,
                 size = 1),

  tar_target(cv_part_naive,
             fit_naive_model(cv_part_exp),
             pattern = cross(cv_part_exp)),
  tar_target(cv_part_bhglm,
             fit_bhglm_model(cv_part_exp),
             pattern = map(cv_part_exp)),
  tar_target(cv_part_rf,
             fit_rf_model(cv_part_exp),
             pattern = map(cv_part_exp)),
  tar_target(cv_part_boost,
             fit_boost_model(cv_part_exp),
             pattern = map(cv_part_exp)),
  tar_target(cv_part_predomics,
             fit_predomics_model(cv_part_exp),
             pattern = map(cv_part_exp)),

  # #print out table of AUCs

  tar_target(auc_table_out,
             auc_table(cv_part_naive, cv_part_bhglm, cv_part_rf, cv_part_boost, cv_part_predomics)),

  #Generate Tables for sample sizes and cases/controls

  tar_target(cv_part_rows,
             experiment_report_makerows(cv_part_exp),
             pattern = map(cv_part_exp)),

  tar_target(cv_part_report,
             experiment_report(cv_part_rows))

)
