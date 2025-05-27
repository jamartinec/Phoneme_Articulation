library(tidyverse)
library(dplyr)
library(posterior)


lib_bayesian_code1 <- modules::use("bayesian_code/visualize_diff_discr.R")
lib_bayesian_code2 <- modules::use("bayesian_code/visualize_curves.R")
lib_bayesian_code3 <- modules::use("bayesian_code/visualize_latent_ability.R")
lib_bayesian_code4 <- modules::use("bayesian_code/fit_bayesian_model2.R")

#########################################################################################################

run_bayesian_modeling <- function(category, levels,model_specific){
  
  # Load data
  tmp_env_data <- new.env()
  loaded_data_objects_1 <- load("./data/processed_data/df_final.RData",envir = tmp_env_data)
  df_final_data <- tmp_env_data[[loaded_data_objects_1[1]]] 
  loaded_data_objects_2 <- load("./data/processed_data/phoneme_levels.RData",envir = tmp_env_data)
  phoneme_levels <- tmp_env_data[[loaded_data_objects_2[1]]] 
  print(phoneme_levels)
  
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

  lib_bayesian_code4$fit_bayesian_model_funct(model_specific,df_filtered,target_phonemes,phoneme_group_str)
}
##########################################################################################################


run_visuals <- function(category, levels){

  load("./data/processed_data/phoneme_levels.RData")
  phonemes <- unlist(phoneme_levels[[category]][levels])
  reference_col_str <- min(phonemes)
  print(reference_col_str)
  phoneme_group_str <- paste(c(category, levels), collapse = "_")
  print(phoneme_group_str)

  # Load data
  tmp_env_data <- new.env()
  loaded_data_objects <- load("./data/processed_data/df_final.RData",envir = tmp_env_data)
  # Assuming one object, probably named 'df_final'
  df_final_data <- tmp_env_data[[loaded_data_objects[1]]] 
  
  # Load model
  tmp_env <- new.env()
  model_name = paste0("model_", phoneme_group_str,".RData")
  model_place = paste0("./data/processed_data/",model_name)
  
  loaded_model_objects <- load(model_place, envir = tmp_env)
  # Assuming only one object is saved
  model <- tmp_env[[loaded_model_objects[1]]]  
  
  # Extract posterior samples. This extracts the posterior samples (i.e. the draws
  #from the posterior distribution of the model parameters) and stores in df -like format
  posterior_samples <- as_draws_df(model)
  print(head(posterior_samples))
  
  
  lib_bayesian_code3$visualize_latent_ability_funct(phoneme_group_str, df_final, posterior_samples)
  lib_bayesian_code2$visualize_curves_funct(phoneme_group_str,reference_col_str,posterior_samples)
  lib_bayesian_code1$visualize_diff_discr_funct(phoneme_group_str,reference_col_str,posterior_samples)

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
category <- "Vowels"
levels <- c("Level4","Level5")
model_specific <- list(
  phi_formula = ~ 1 + expected_phoneme
)
#levels <- c("Level3")
run_bayesian_modeling(category, levels, model_specific)
#run_visuals(category, levels)

