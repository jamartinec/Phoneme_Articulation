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

# Load file path
Paths <- modules::use("./bayesian_code/utils/file_paths.R")
brms_help <- modules::use("./bayesian_code/utils/tristan_brm_helper.R")
#source("./bayesian_code/utils/file_path.R")

export("fit_bayesian_model_funct")
fit_bayesian_model_funct <- function(model_specific,
                                     prior_specific,
                                     df_filtered,
                                     #target_phonemes,# creo que esto no se esta usando, revisar
                                     prefix,
                                     phoneme_group_str
                                     ){
  
  # En este punto lo que vamos a hacer es crear un objeto de tipo argumento
  # (para la funcion brm), que contiene la formula, data, prior, filename.
  
  #model_name = paste0("model_", phoneme_group_str,".RData")
  model_name = paste0("model_TRIQUIS", phoneme_group_str)
  model_place <- file.path(prefix, model_name)
  #print(model_name)
  
  # Modularizar esto como funcion aparte. Esto debe testearse antes de pasar
  # a la funcion brm(). 
  # Create directory if it doesn't exist
  
  #dir_path <- dirname(model_place)
  #if (!dir.exists(dir_path)) {
  #  dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
  #}
  
  # Tener cuidado con el argumento file_refit
  # print("saving file")
  # save(model, file = model_place)
  
  brm_args <- brms_help$brms_args_create()
  args <- brm_args(formula = model_specific,
                   data = df_filtered,
                   prior = prior_specific,
                   file = model_place,
                   seed = 20250625,
                   chains = 4,
                   iter = 4000,
                   cores = 4#,
                   #...
                   )
  model <- do.call(brm, args)
  
  # model <- brm(
  #   
  #   formula = model_specific,
  #   #family = Beta(link = "logit"),
  #   data = df_filtered,
  #   prior = c(
  #     prior(normal(0,5), class = "b", nlpar = "eta"),
  #     prior(normal(0,1), class = "b", nlpar = "logalpha"),
  #     prior(constant(1), class="sd", group="speaker", nlpar = "eta"),
  #     prior(normal(0, 1), dpar = "phi", class = "b")
  #   ),
  #   chains = 4, iter = 4000, cores = 4
  #   #chains = 1, iter = 400, cores = 4
  # )
  #return(model)
  rm(model)
  gc()

# Considerar lo siguiente: que esta funcion retorne el modelo, 
# en caso de que se haya indicado en la funcion que llama a la actual (run o iterate)
  # que se desea hacer la validacion inmediatamente despues, procedar a hacerla
  # siguiendo el ejemplo de adding_loo_criterion()
}

##########################################################################
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
  tryCatch({
    message("Trying to load filtered data...")
    df_filtered <- load_from_rdata(filtered_file_path, "df_filtered")
    message("Loaded filtered data.")
    return(df_filtered)
  }, error = function(e) {
    message("Filtered data not found. Creating it from raw...")
    
    # Filter data to include only those phonemes
    
    df_filtered <- df_final_data %>%
      filter(expected_phoneme %in% target_phonemes)
    save(df_filtered, file = filtered_file_path)
    message("Filtered data created and saved.")
    return(df_filtered)
  })
}
##########################################################################

export("run_bayesian_modeling")
run_bayesian_modeling <- function(category, 
                                  levels, 
                                  prefix, 
                                  model_specific,
                                  prior_specific,
                                  df_final_data,
                                  phoneme_levels
                                  ){
  
  # Open the filtered files if already exists, otherwise open raw data files, 
  # apply the filter and save filtered objects for next time.
  
  # Phonemes of interest
  phoneme_group_str <- paste(c(category, levels), collapse = "_")
  print(phoneme_group_str)
  target_phonemes <-  unlist(lapply(levels, function(lvl) phoneme_levels[[category]][[lvl]]))
  
  # llamar desde utils
  
  #folder_path <- "./data/processed_data/filtered_data/"
  folder_path <- Paths$filtered_data_dir
  filename <-  paste0(phoneme_group_str, ".RData") 
  filtered_file_path <- file.path(folder_path,filename)
  print(filtered_file_path)###################################
  df_filtered <- load_or_create_filtered_data(phoneme_group_str,
                               filtered_file_path,
                               df_final_data,
                               target_phonemes
                               )
  
  # Filter data to include only those phonemes
  # df_filtered <- df_final_data %>%
  # filter(expected_phoneme %in% target_phonemes)
  df_filtered$expected_phoneme <- as.factor(df_filtered$expected_phoneme)
  fit_bayesian_model_funct(model_specific,
                           prior_specific,
                           df_filtered,
                           #target_phonemes, #parece que no se usa este arg#
                           prefix,
                           phoneme_group_str
                           )
  rm(df_filtered)
  gc()
  }

export("iterate_run_bayesian_modeling")
iterate_run_bayesian_modeling <- function(list_to_fit){
  
  # Llamar desde utils.
  #folder_path = "./data/processed_data/"
  folder_path = Paths$processed_data_dir
  #data_place <- sub("^(.*?processed_data/).*", "\\1", prefix)
  # Load 'raw' data in case it is needed for filtering later
  # Load data
  tmp_env_data <- new.env()
  #loaded_data_objects_1 <- load(paste(data_place,"df_final.RData", sep = ""),
                                #envir = tmp_env_data)
  loaded_data_objects_1 <- load(file.path(folder_path, "df_final.RData"),
                                envir = tmp_env_data)
  df_final_data <- tmp_env_data[[loaded_data_objects_1[1]]] 
  
  #loaded_data_objects_2 <- load(paste(data_place,"phoneme_levels.RData", sep = ""),
                                #envir = tmp_env_data)
  loaded_data_objects_2 <- load(file.path(folder_path,"phoneme_levels.RData"),
                                envir = tmp_env_data)
  phoneme_levels <- tmp_env_data[[loaded_data_objects_2[1]]]
  
  
  for (item in list_to_fit){
    
    #prefix <- paste(folder_path, item["model_opt"], "/", sep="")
    prefix <- file.path(folder_path,item["model_opt"])
    print(prefix)
    
    run_bayesian_modeling(item[["category"]], 
                          item[["levels"]], 
                          prefix, 
                          item[["model_specific"]],
                          item[["prior_specific"]],
                          df_final_data,
                          phoneme_levels
                          )
  }
}