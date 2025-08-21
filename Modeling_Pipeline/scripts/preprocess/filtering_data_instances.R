#-----------------------
# Create Filtered data for running each instance.
#-----------------------
import("tidyverse")
import("dplyr")
import("tidyr")
# import("psych") # For factor analysis and scree plot
# import("readr") 
# import("brms") # For Bayesian analysis
# import("splines") # For natural splines

# import("utils")

#------------------------------------------------------------------------------
# Future work: create a function that check if the filtered data read actually
# contains the columns expected (sanity check ??)


#-----------------------------------------------------------------------------
Paths <- modules::use("./bayesian_code/utils/file_paths.R")
# Recibe la lista de listas que salen de read_instance_specification.R
# Estamos interesados en la dupla category y levels de cada instancia.

# Recibe: 
# list_of_instances (de read_instance_specification)
# df_final (de preproessing)
# raw_data_type, 
# phoneme_grouping_type
# model_type


ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
}

ensure_parent_dir <- function(file_path) {
  ensure_dir(dirname(file_path))
}



# phoneme_grouping_data_path <- "./Modeling_Pipeline/phoneme_grouping/phoneme_grouping1.csv"
# phoneme_df <- read.csv(phoneme_grouping_data_path)


# recibe una instancia, la cual es una de las entradas listadas en list_of_instances.
export("filtering_data")
#filtering_data <- function(instance, df_final_data, raw_data_type, model_type, phoneme_grouping_type, phoneme_df){
filtering_data <- function(instance, df_final_data, phoneme_df){  
  
  category <- instance$category
  levels <- instance$levels
  #phoneme_group_str <- paste(c(category, levels), collapse = "_")
  phoneme_group_str <-instance$phoneme_group_str
  
  #phonemes of interest.
  #target_phonemes <-  unlist(lapply(levels, function(lvl) phoneme_df[[category]][[lvl]]))
  target_phonemes <- instance$target_phonemes
  #folder_path <- "./data/processed_data/filtered_data/"
  #folder_path <- Paths$filtered_data_dir
  #subfolder_path <- file.path(folder_path, raw_data_type, model_type, phoneme_grouping_type )
  filtered_file_path <- instance$filtered_file_path
  subfolder_path <- dirname(filtered_file_path)
  ensure_dir(subfolder_path)  # <<< create the directory tree here
  
  #filename <-  paste0(phoneme_group_str, ".RData") 
  #filtered_file_path <- file.path(subfolder_path, filename)


message(filtered_file_path)
ensure_or_create_filtered_rds(phoneme_group_str,
                                            filtered_file_path,
                                            df_final_data,
                                            target_phonemes
)

  return(filtered_file_path)

}

################################################################################
# Create the .rds if it doesn't exist; do NOT return the df (phase-by-phase workflow)
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
  
  # Optional: make expected_phoneme a factor
  df_filtered$expected_phoneme <- as.factor(df_filtered$expected_phoneme)
  
  ensure_parent_dir(filtered_file_path)
  saveRDS(df_filtered, filtered_file_path, compress = "xz")
  message("Filtered data created and saved (rds).")
  invisible(NULL)
}

# Optional convenience when you DO need the data in memory
read_filtered_data <- function(filtered_file_path) {
  readRDS(filtered_file_path)
}




