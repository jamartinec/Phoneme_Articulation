#-----------------------
# Create filtered data required for running each model instance
#-----------------------
import("tidyverse")
import("dplyr")
import("tidyr")


#------------------------------------------------------------------------------
# Future work: implement a sanity-check function to verify that the filtered
# data contains the expected columns.
#-----------------------------------------------------------------------------
# Load shared file-path definitions
Paths <- modules::use("./Modeling_Pipeline/pipeline/config/file_paths.R")
preprocessing_lib   <- modules::use("./Modeling_Pipeline/scripts/preprocess/Preprocessing.R")

# Receives the list of lists produced by `read_instance_specification.R`.
# We focus on the `(category, levels)` pair associated with each instance.
#
# Inputs:
# - list_of_instances (from `read_instance_specification`)
# - df_final (from preprocessing)
# - raw_data_type
# - phoneme_grouping_type
# - model_type




ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
}

ensure_parent_dir <- function(file_path) {
  ensure_dir(dirname(file_path))
}



# phoneme_grouping_data_path <- "./Modeling_Pipeline/phoneme_grouping/phoneme_grouping1.csv"
# phoneme_df <- read.csv(phoneme_grouping_data_path)


# Receives a single instance, which corresponds to one entry in `list_of_instances`.
export("filtering_data")
#filtering_data <- function(instance, df_final_data, raw_data_type, model_type, phoneme_grouping_type, phoneme_df){
filtering_data <- function(instance, df_final_data, phoneme_df){  
  
  category <- instance$category
  levels <- instance$levels
  phoneme_group_str <-instance$phoneme_group_str
  
  # Phonemes of interest
  target_phonemes <- instance$target_phonemes
  filtered_file_path <- instance$filtered_file_path
  subfolder_path <- dirname(filtered_file_path)
  ensure_dir(subfolder_path)  
  ensure_or_create_filtered_rds(phoneme_group_str,
                                              filtered_file_path,
                                              df_final_data,
                                              target_phonemes
                                )

  return(filtered_file_path)

}


# Create the .rds file if it does not exist; do NOT return the data frame
# (phase-by-phase workflow design)
ensure_or_create_filtered_rds <- function(phoneme_group_str,
                                          filtered_file_path,
                                          df_final_data,
                                          target_phonemes) {
  if (file.exists(filtered_file_path)) {
    message("filtered data file exists (rds). No work needed.")
    return(invisible(NULL))
  }
  
  message("Filtered data not found. Creating it from raw...")
  df_filtered <- df_final_data %>%
    dplyr::filter(.data$expected_phoneme %in% target_phonemes)
  
  # Make expected_phoneme a factor
  df_filtered$expected_phoneme <- as.factor(df_filtered$expected_phoneme)
  
  ensure_parent_dir(filtered_file_path)
  saveRDS(df_filtered, filtered_file_path, compress = "xz")
  message("Filtered data created and saved (rds).")
  invisible(NULL)
}

# Optional convenience function when the filtered data is needed in memory
read_filtered_data <- function(filtered_file_path) {
  readRDS(filtered_file_path)
}

export("auxiliary_get_filtered")
auxiliary_get_filtered<- function(instance,list_df_phonemes,preprocessed_cache) {
  
  # lookup preprocessed data
  prep <- preprocessing_lib$get_preprocessed_for_instance(instance, preprocessed_cache)
  df_final <- prep$df_final
  
  # lookup phoneme grouping df
  phoneme_df <- list_df_phonemes[[ instance$phoneme_grouping_type ]]
  
  if (is.null(phoneme_df)) {
    stop(
      "No phoneme_df found for grouping type: ",
      instance$phoneme_grouping_type,
      call. = FALSE
    )
  }
  
  # call existing filtering logic
    filtering_data(
    instance,
    df_final,
    phoneme_df
  )
}

