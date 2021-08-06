##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param file_path_sample file path to a tab delimited sample data
##' @param file_path_biom file path to the .biom file w/ OTU data
##' @param taxonomic_level char vector of taxanomic level to merge to
##'
##' @return phyloseq object including sample data
##'

load_into_phyloseq <- function(file_path_sample, file_path_biom, taxonomic_level){
  #load in the data
  #biom data can be loaded directly into the phyloseq object
  metadata <- read_delim(file_path_sample, "\t", na = "no_data", col_types = cols(.default = "c"))
  biome_data <- import_biom(file_path_biom)
  #connect the sample metadata to the phyloseq object
  sample_data(biome_data) <- metadata %>%
                             clean_names("snake") %>%
                             column_to_rownames(var = "number_sample_id")

  final_phyloseq <- biome_data %>%
                    # select out samples from desired site
                    subset_samples(body_site == "UBERON:feces") %>%
                    tax_glom(taxonomic_level) %>%
                    transform_sample_counts( function(x) x / sum(x)) %>%
                    prune_taxa(taxa_sums(.) > 0, .)

  return(final_phyloseq)
}
