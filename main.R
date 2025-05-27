library(tidyverse)
library(dplyr)
library(posterior)


prefix <- "../data/processed_data/"
lib_bayesian_code1 <- modules::use("bayesian_code/visualize_diff_discr.R")
lib_bayesian_code2 <- modules::use("bayesian_code/visualize_curves.R")
lib_bayesian_code3 <- modules::use("bayesian_code/visualize_latent_ability.R")
lib_bayesian_code4 <- modules::use("bayesian_code/fit_bayesian_model.R")
lib_bayesian_code5 <- modules::use("bayesian_code/visualize_age_standards.R")
#########################################################################################################

run_bayesian_modeling <- function(category, levels, prefix){
  
  # Load data
  tmp_env_data <- new.env()
  loaded_data_objects_1 <- load(paste(prefix,"df_final.RData", sep = ""),envir = tmp_env_data)
  df_final_data <- tmp_env_data[[loaded_data_objects_1[1]]] 
  loaded_data_objects_2 <- load(paste(prefix,"phoneme_levels.RData", sep = ""),envir = tmp_env_data)
  phoneme_levels <- tmp_env_data[[loaded_data_objects_2[1]]] 
  
  # Phonemes of interest
  #target_phonemes <- phoneme_levels$Consonants$Level6
  target_phonemes <-  unlist(lapply(levels, function(lvl) phoneme_levels[[category]][[lvl]]))
  #phoneme_group_str <- "Consonants_Level6"
  phoneme_group_str <- paste(c(category, levels), collapse = "_")
  

  print(head(target_phonemes))
  
  # Filter data to include only those phonemes
  df_filtered <- df_final_data %>%
    filter(expected_phoneme %in% target_phonemes)
  df_filtered$expected_phoneme <- as.factor(df_filtered$expected_phoneme)

  lib_bayesian_code4$fit_bayesian_model_funct(df_filtered,target_phonemes,phoneme_group_str,prefix)
}
##########################################################################################################


run_visuals <- function(category, levels, prefix) {
  
  # Load phoneme levels
  load(paste0(prefix, "phoneme_levels.RData"))
  phonemes <- unlist(phoneme_levels[[category]][levels])
  reference_col_str <- min(phonemes)
  phoneme_group_str <- paste(c(category, levels), collapse = "_")
  
  # Remove "ZH" since it is not included in dataset
  phonemes <- setdiff(phonemes, "ZH")
  
  # Load data and model directly into current environment
  load(paste0(prefix, "df_final.RData"))  # Assumes it loads `df_final`
  load(paste0(prefix, "model_", phoneme_group_str, ".RData"))  # Assumes it loads `model`
  
  # Extract posterior samples
  posterior_samples <- as_draws_df(model)
  
  # Generate visualizations
  lib_bayesian_code3$visualize_latent_ability_funct(phoneme_group_str, df_final, posterior_samples)
  lib_bayesian_code2$visualize_curves_funct(phoneme_group_str, reference_col_str, posterior_samples)
  lib_bayesian_code1$visualize_diff_discr_funct(phoneme_group_str, reference_col_str, posterior_samples)
  lib_bayesian_code5$visualize_age_standards_funct(model, phonemes, phoneme_group_str, reference_col_str, posterior_samples)
}

# Example execution

#phoneme_group_str <- "Consonants_Level6"
#phoneme_group_str <- "Consonants_Level5"
#phoneme_group_str <- "Consonants_Level4"
#phoneme_group_str <- "Consonants_Level3"
#phoneme_group_str <- "Vowels_Level3"
#phoneme_group_str <- "Vowels_Level1_Level2"
#phoneme_group_str <- "Vowels_Level4_Level5"

# Uncomment depending on what you want to run
category <- "Consonants"
levels <- c("Level6")
#run_bayesian_modeling(category, levels, prefix)
run_visuals(category, levels, prefix)

