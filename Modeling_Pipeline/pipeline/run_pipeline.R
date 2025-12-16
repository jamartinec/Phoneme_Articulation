# Run pipeline
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





# Let us recall the inputs required by `read_instance_specification`.
# We need to read the file that defines the phoneme groupings into
# the data levels we want to model. Recall that there are phoneme
# classifications by categories and levels, but we want the flexibility
# to model data groups that, for example, belong to the same category
# while including different levels (this is handled through
# `subset_data_grouping`).

# We also need to read the file that specifies the instances we want to fit.
# Each instance is defined by a +4-tuple consisting of the model name,
# the prior name, and the data-level (or grouping) to be used.


# ------------------------------------------------------------------------------
## SANITY CHECK: Be careful: there must be consistency between the
## `phoneme_grouping1` file and the `subset_data_grouping1` file.
#
# Future work: create a function that checks that the subset-data columns
# specified in the instance file actually belong to the `subsetdata`
# column defined in the phoneme grouping file.

#-------------------------------------------------------------------------------



run_pipeline<-function(instance_to_fit_path){
  # Dictionaries with correspondence between keys in instances specification to files.
  rawdata_paths<-conventions$rawdata_paths
  grouping_paths<-conventions$grouping_paths
  setdatafiles_paths<-conventions$setdatafiles_paths 
  
  list_of_instances <- read_instances_specifications_lib$read_instances_specifications_modified(instance_to_fit_path)
  
  
  # Preprocessing without assuming all instances have the same: raw_data_type,model_type,phoneme_grouping_type
  # find the list of unique keys in the instances we're considering
  
  unique_keys <- read_instances_specifications_lib$find_unique_instances_keys(list_of_instances)
  unique_keys1 <- unique_keys$unique_keys1 # this is just raw_data_type, model_type, phoneme_grouping_type
  unique_phoneme_grouping_type<- unique_keys$unique_phoneme_grouping_type
  
  
  preprocessing_keys <- purrr::map(
    unique_keys$unique_keys1,
    read_instances_specifications_lib$expand_preprocessing_key,
    rawdata_paths = rawdata_paths,
    grouping_paths = grouping_paths
  )
  
  # now pass each unique 5-tuple and get in a new list  the preprocessed files for each case
  preprocessed_cache <- preprocessing_keys |>
    purrr::map(preprocessing_lib$auxiliary_preprocess_cache) |>
    purrr::set_names(
      purrr::map_chr(
        preprocessing_keys,
        ~ paste(.$raw_data_type, .$model_type, .$phoneme_grouping_type, sep = "|")
      )
    )
  
  
  #read the preprocessed files if they already exist:
  preprocessed_cache_read <- unique_keys1 |>
    purrr::map(\(key1) {
      preprocessing_lib$read_preprocessed_from_key(key1)
    }) |>
    purrr::set_names(
      purrr::map_chr(unique_keys1, read_instances_specifications_lib$make_key_string)
    )
  
  
  missing <- setdiff(unique_phoneme_grouping_type, names(grouping_paths))
  if (length(missing) > 0) {
    stop(
      "No grouping_paths defined for: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  
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


# Test1

test_instances <- Paths$Pipeline_instance_spec_tests_dir

instance_to_fit_path <- file.path(test_instances,"instance_to_fit.csv")
run_pipeline(instance_to_fit_path)

#Test2

# We are now going to run the pipeline assuming that we want to model
# the `mean_prob` variable, which can be modeled using a Beta regression.

instance_to_fit_path <- file.path(test_instances,"instance_to_fit2.csv")
run_pipeline(instance_to_fit_path)


# Test 3
# Let's analyze the pipeline but now using the aaps data + binomial model

instance_to_fit_path <- file.path(test_instances,"instance_to_fit3.csv")
run_pipeline(instance_to_fit_path)


#Test 4. Combined:
instance_to_fit_path <- file.path(test_instances,"instance_to_fit4.csv")
run_pipeline(instance_to_fit_path)

# Ensure that the instances of interest for testing the proposed cutting-point
# definition have been processed.

cuttingpoint_instances <- Paths$Pipeline_instance_spec_cuttingpoints_dir

instance_to_fit_path <- file.path(cuttingpoint_instances,"instance_to_fit1_B.csv")
run_pipeline(instance_to_fit_path)


instance_to_fit_path <- file.path(cuttingpoint_instances,"instance_to_fit1_A.csv")
run_pipeline(instance_to_fit_path)


