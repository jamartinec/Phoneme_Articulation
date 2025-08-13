#-----------------------
# Create Filtered data for running each instance.
#-----------------------
# import("dplyr")
# import("tidyr")
# import("psych") # For factor analysis and scree plot
# import("readr") 
# import("brms") # For Bayesian analysis
# import("splines") # For natural splines
# import("tidyverse")
# import("utils")

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



phoneme_grouping_data_path <- "./Modeling_Pipeline/phoneme_grouping/phoneme_grouping1.csv"
phoneme_df <- read.csv(phoneme_grouping_data_path)

# recibe una instancia, la cual es una de las entradas listadas en list_of_instances.
filtering_data <- function(df_final_data, instance, raw_data_type, model_type, phoneme_grouping_type,phoneme_df){
  
  
  category <- instance$category
  levels <- instance$levels
  phoneme_group_str <- paste(c(category, levels), collapse = "_")
  print(phoneme_group_str)
  #phonemes of interest.
  target_phonemes <-  unlist(lapply(levels, function(lvl) phoneme_df[[category]][[lvl]]))
  #folder_path <- "./data/processed_data/filtered_data/"
  folder_path <- Paths$filtered_data_dir
  subfolder_path <- file.path(folder_path, raw_data_type, model_type, phoneme_grouping_type )
  ensure_dir(subfolder_path)  # <<< create the directory tree here
  
  filename <-  paste0(phoneme_group_str, ".RData") 
  filtered_file_path <- file.path(subfolder_path, filename)


message(filtered_file_path)###################################
df_filtered <- load_or_create_filtered_data(phoneme_group_str,
                                            filtered_file_path,
                                            df_final_data,
                                            target_phonemes
)
return(df_filtered)

}

################################################################################
load_from_rdata <- function(file, obj_name) {
  temp_env <- new.env()
  load(file, envir = temp_env)
  
  if (!exists(obj_name, envir = temp_env)) {
    stop(paste("Object", obj_name, "not found in", file))
  }
  
  return(get(obj_name, envir = temp_env))
}

load_or_create_filtered_data <- function(phoneme_group_str,
                                         filtered_file_path, 
                                         df_final_data,
                                         target_phonemes
) {
  if (file.exists(filtered_file_path)) {
    message("Trying to load filtered data...")
    out <- load_from_rdata(filtered_file_path, "df_filtered")
    message("Loaded filtered data.")
    return(out)
  }
  
  message("Filtered data not found. Creating it from raw...")
  df_filtered <- df_final_data %>%
    filter(.data$expected_phoneme %in% target_phonemes)
  
  ensure_parent_dir(filtered_file_path)  # <<< make sure parent exists before save
  save(df_filtered, file = filtered_file_path, compress = "xz")
  message("Filtered data created and saved.")
  return(df_filtered)
}


###############################################################################


purrr::map(rows, extract_one_instance)



