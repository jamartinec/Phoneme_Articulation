
lib_bayesian_code1 <- modules::use("bayesian_code/visualize_diff_discr.R")
lib_bayesian_code2 <- modules::use("bayesian_code/visualize_curves.R")
lib_bayesian_code3 <- modules::use("bayesian_code/visualize_latent_ability.R")
lib_bayesian_code4 <- modules::use("bayesian_code/fit_bayesian_model.R")


################################################################################

run_bayesian_modeling <- function(){
  
  # Load data
  tmp_env <- new.env()
  loaded_data_objects_1 <- load("./data/processed_data/df_final.RData",envir = tmp_env_data)
  df_final_data <- tmp_env_data[[loaded_data_objects_1[1]]] 
  loaded_data_objects_2 <- load("./data/processed_data/phoneme_levels.RData",envir = tmp_env_data)
  phoneme_levels <- tmp_env_data[[loaded_data_objects_2[1]]] 
  
  # Phonemes of interest
  target_phonemes <- phoneme_levels$Consonants$Level6
  phoneme_group_str <- "Consonants_Level6"

  head(target_phonemes)
  
  # Filter data to include only those phonemes
  df_filtered <- df_final_data %>%
    filter(expected_phoneme %in% target_phonemes)
  df_filtered$expected_phoneme <- as.factor(df_filtered$expected_phoneme)

  lib_bayesian_code4$fit_bayesian_model_funct(df_filtered,target_phonemes,phoneme_group_str)
}

#######################3


run_visuals <- function(){


#phoneme_group_str <- "Consonants_Level6"
#phoneme_group_str <- "Consonants_Level5"
#phoneme_group_str <- "Consonants_Level4"
#phoneme_group_str <- "Consonants_Level3"
#phoneme_group_str <- "Vowels_Level3"
#phoneme_group_str <- "Vowels_Level1_Level2"
phoneme_group_str <- "Vowels_Level4_Level5"


#reference_col_str = "CH"
#reference_col_str = "L"
#reference_col_str = "B"
#reference_col_str = "HH"
#reference_col_str = "AO"
#reference_col_str = "AA"
reference_col_str = "AE"

# Load data
tmp_env_data <- new.env()
loaded_data_objects <- load("./data/processed_data/df_final.RData",envir = tmp_env_data)
# Assuming one object, probably named 'df_final'
df_final_data <- tmp_env_data[[loaded_data_objects[1]]] 

# Load model
#load("./data/processed_data/model.RData")
#tmp_env <- new.env()
model_name = paste0("model_", phoneme_group_str,".RData")
model_place = paste0("./data/processed_data/",model_name)
loaded_model_objects <- load(model_place, envir = tmp_env)
# Assuming only one object is saved
model <- tmp_env[[loaded_model_objects[1]]]  

# Extract posterior samples. This extracts the posterior samples (i.e. the draws
#from the posterior distribution of the model parameters) and stores in df -like format
posterior_samples <- as_draws_df(model)

head(posterior_samples)


lib_bayesian_code3$visualize_latent_ability_funct(phoneme_group_str, df_final, posterior_samples)
lib_bayesian_code2$visualize_curves_funct(phoneme_group_str,reference_col_str,posterior_samples)
lib_bayesian_code1$visualize_diff_discr_funct(phoneme_group_str,reference_col_str,posterior_samples)

}

# Example execution
# Uncomment depending on what you want to run
#run_bayesian_modeling()
run_visuals()