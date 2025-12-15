library(tidyverse)
library(dplyr)
library(posterior)
library(yaml)
library(purrr)
library(glue)
Paths <- modules::use("./Modeling_Pipeline/pipeline/config/file_paths.R")
read_instances_specifications_lib <- modules::use("./Modeling_Pipeline/scripts/preprocess/read_instance_specification.R")
preprocessing_lib                 <- modules::use("./Modeling_Pipeline/scripts/preprocess/Preprocessing.R")
filtering_lib                     <- modules::use("./Modeling_Pipeline/scripts/preprocess/filtering_data_instances.R")
fit_models_lib                    <- modules::use("./Modeling_Pipeline/scripts/train/fit_models.R")
visualize_models_lib0             <- modules::use("./Modeling_Pipeline/scripts/visualize/visualize_age_standards.R")
visualize_models_lib1             <- modules::use("Modeling_Pipeline/scripts/visualize/run_visuals.R")
conventions <- modules::use("./Modeling_Pipeline/pipeline/config/conventions.R")


main<-function(instance_to_fit_path){
  
  rawdata_paths<-conventions$rawdata_paths
  grouping_paths<-conventions$grouping_paths
  setdatafiles_paths<-conventions$setdatafiles_paths
  list_of_instances <- read_instances_specifications_lib$read_instances_specifications_modified(instance_to_fit_path)
  
  unique_keys <- read_instances_specifications_lib$find_unique_instances_keys(list_of_instances)
  unique_keys1 <- unique_keys$unique_keys1 # this is just raw_data_type, model_type, phoneme_grouping_type
  unique_phoneme_grouping_type<- unique_keys$unique_phoneme_grouping_type 
  
  
  
  preprocessing_keys <- purrr::map(
    unique_keys$unique_keys1,
    read_instances_specifications_lib$expand_preprocessing_key,
    rawdata_paths = rawdata_paths,
    grouping_paths = grouping_paths
  )
  
  
  preprocessed_cache <- preprocessing_keys |>
    purrr::map(preprocessing_lib$auxiliary_preprocess_cache) |>
    purrr::set_names(
      purrr::map_chr(
        preprocessing_keys,
        ~ paste(.$raw_data_type, .$model_type, .$phoneme_grouping_type, sep = "|")
      )
    )
  
  preprocessed_cache_read <- unique_keys1 |>
    purrr::map(\(key1) {
      preprocessing_lib$read_preprocessed_from_key(key1)
    }) |>
    purrr::set_names(
      purrr::map_chr(unique_keys1, read_instances_specifications_lib$make_key_string)
    )
  
  list_df_phonemes <- purrr::imap(
    grouping_paths[unique_phoneme_grouping_type],
    function(path, grouping_type) {
      read.csv(path, stringsAsFactors = FALSE)
    }
  )
  
  
  list_of_df_filters_file_paths <- purrr::map(
    list_of_instances,
    ~filtering_lib$auxiliary_get_filtered(.x,list_df_phonemes=list_df_phonemes,preprocessed_cache=preprocessed_cache_read) 
  )
  
  
  fit_models_lib$iterate_run_bayesian_modeling(list_of_instances)
  
  
  visualize_models_lib1$iterate_plots_modified(
    list_of_instances,
    preprocessed_cache_read
  )
  
}


args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
  stop("Usage: Rscript main.R <instance_to_fit_path>")
}

instance_to_fit_path <- args[[1]]

main(instance_to_fit_path)
#for example:
#martinezcorr@Massassi MINGW64 ~/Documents/Research/Phoneme_Articulation (modularization_update)
#$ Rscript Modeling_Pipeline/pipeline/main.R "./Modeling_Pipeline/instance_specification/tests/instance_to_fit.csv"
