#-----------------------
# Run Bayesian analysis
#-----------------------
import("dplyr")
import("tidyr")
import("psych") # For factor analysis and scree plot
import("readr") 
import("brms") # For Bayesian analysis
import("splines") # For natural splines
import("tidyverse")
import("utils")

export("fit_bayesian_model_funct")
fit_bayesian_model_funct <- function(model_specific,
                                     df_filtered,
                                     target_phonemes,
                                     phoneme_group_str,
                                    prefix){
  print(model_specific)
  model <- brm(
    formula = model_specific,
    family = Beta(link = "logit"),
    data = df_filtered,
    prior = c(
      prior(normal(0,5), class = "b", nlpar = "eta"),
      prior(normal(0,1), class = "b", nlpar = "logalpha"),
      prior(constant(1), class="sd", group="speaker", nlpar = "eta"),
      prior(normal(0, 1), dpar = "phi", class = "b")
    ),
    chains = 4, iter = 4000, cores = 4
    #chains = 1, iter = 400, cores = 4
  )
  
  model_name = paste0("model_", phoneme_group_str,".RData")
  #model_place = paste0(prefix,model_name,sep="")
  model_place<- file.path(prefix, model_name)
  print(model_name)
 
  # Create directory if it doesn't exist
  dir_path <- dirname(model_place)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
  }
  
  print("saving file")
  save(model, file = model_place)
}


export("run_bayesian_modeling")
run_bayesian_modeling <- function(category, 
                                  levels, 
                                  prefix, 
                                  model_specific
                                  ){
  
  data_place <- sub("^(.*?processed_data/).*", "\\1", prefix)
  
  # Phonemes of interest
  
  
  phoneme_group_str <- paste(c(category, levels), collapse = "_")
  print(phoneme_group_str)
  
  
  # Load data
  tmp_env_data <- new.env()
  loaded_data_objects_1 <- load(paste(data_place,"df_final.RData", sep = ""),
                                envir = tmp_env_data)
  df_final_data <- tmp_env_data[[loaded_data_objects_1[1]]] 
  
  loaded_data_objects_2 <- load(paste(data_place,"phoneme_levels.RData", sep = ""),
                                envir = tmp_env_data)
  phoneme_levels <- tmp_env_data[[loaded_data_objects_2[1]]]
  
  target_phonemes <-  unlist(lapply(levels, function(lvl) phoneme_levels[[category]][[lvl]]))
  #phoneme_group_str <- "Consonants_Level6"
  
  
  
 
  #  # Filter data to include only those phonemes
  df_filtered <- df_final_data %>%
    filter(expected_phoneme %in% target_phonemes)
  df_filtered$expected_phoneme <- as.factor(df_filtered$expected_phoneme)
  fit_bayesian_model_funct(model_specific, 
                           df_filtered,
                           target_phonemes,
                           phoneme_group_str,
                           prefix)
  
}

export("iterate_run_bayesian_modeling")
iterate_run_bayesian_modeling <- function(list_to_fit){
  folder_path = "./data/processed_data/"
  for (item in list_to_fit){
    
    prefix <- paste(folder_path, item["model_opt"], "/", sep="")
    print(prefix)
    
    run_bayesian_modeling(item[["category"]], item[["levels"]], prefix, item[["model_specific"]])
  }
}