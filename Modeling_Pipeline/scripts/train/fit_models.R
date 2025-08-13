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
  model_name = paste0("model_", phoneme_group_str)
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
                   # the model is always refitted even if an object with the same name already exists.
                   file_refit = "always", 
                   seed = 20250625,
                   chains = 4,
                   iter  = 4000,
                   cores = 4#,
                   #...
  )
  args$backend <- "cmdstanr"
  cat("  → Fitting model: ", model_name, "\n")
  model <- do.call(brm, args)
  # Add the validation criteria and save a file with same name.
  cat("  → Adding validation criteria (loo, waic)...\n")
  model <- brms_help$add_validation_criterion(
    model, 
    val_list=c("loo","waic"), 
    use_reloo = FALSE)
  saveRDS(model, file = paste0(model_place,".rds"))
  rm(model)
  gc()
  
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
  invisible(gc())
}

export("iterate_run_bayesian_modeling")
iterate_run_bayesian_modeling <- function(list_to_fit){
  #rstan::rstan_options(auto_write = TRUE)
  #options(mc.cores = parallel::detectCores())
  
  # Llamar desde utils.
  #folder_path = "./data/processed_data/"
  folder_path = Paths$processed_data_dir
  #data_place <- sub("^(.*?processed_data/).*", "\\1", prefix)
  # Load 'raw' data in case it is needed for filtering later
  # Load data
  tmp_env_data <- new.env()
  #loaded_data_objects_1 <- load(paste(data_place,"df_final.RData", sep = ""),
  #envir = tmp_env_data)
  # loaded_data_objects_1 <- load(file.path(folder_path, "df_final.RData"),
  #                               envir = tmp_env_data)
  # loaded_data_objects_1 <- load(file.path(folder_path, "df_finalVersion2.RData"),
  #                               envir = tmp_env_data)
  # df_final_data <- tmp_env_data[[loaded_data_objects_1[1]]] 
  
  # loaded_data_objects_1 <- load(file.path(folder_path, "df_final_AAPS.RData"),
  #                               envir = tmp_env_data)
  # df_final_data <- tmp_env_data[[loaded_data_objects_1[1]]]
  
  
  # loaded_data_objects_1 <- load(file.path(folder_path, "df_final_binomialxphoneme_Prob.RData"),
  #                               envir = tmp_env_data)
  # df_final_data <- tmp_env_data[[loaded_data_objects_1[1]]]
  
  loaded_data_objects_1 <- load(file.path(folder_path, "df_final_binomialxphoneme_Prob_singleWords.RData"),
                                envir = tmp_env_data)
  df_final_data <- tmp_env_data[[loaded_data_objects_1[1]]]
  
  
  
  #loaded_data_objects_2 <- load(paste(data_place,"phoneme_levels.RData", sep = ""),
  #envir = tmp_env_data)
  # loaded_data_objects_2 <- load(file.path(folder_path,"phoneme_levels.RData"),
  #                               envir = tmp_env_data)
  # loaded_data_objects_2 <- load(file.path(folder_path,"phoneme_levelsVersion2.RData"),
  #                               envir = tmp_env_data)
  # phoneme_levels <- tmp_env_data[[loaded_data_objects_2[1]]]
  
  # loaded_data_objects_2 <- load(file.path(folder_path,"phoneme_levels_AAPS.RData"),
  #                               envir = tmp_env_data)
  # phoneme_levels <- tmp_env_data[[loaded_data_objects_2[1]]]
  
  # loaded_data_objects_2 <- load(file.path(folder_path,"phoneme_levels_binomialxphoneme_Prob.RData"),
  #                               envir = tmp_env_data)
  # phoneme_levels <- tmp_env_data[[loaded_data_objects_2[1]]]
  
  loaded_data_objects_2 <- load(file.path(folder_path,"phoneme_levels_binomialxphoneme_Prob_singleWords.RData"),
                                envir = tmp_env_data)
  phoneme_levels <- tmp_env_data[[loaded_data_objects_2[1]]]
  
  #
  #
  
  failures <- list()
  
  for (item in list_to_fit){
    
    #prefix <- paste(folder_path, item["model_opt"], "/", sep="")
    prefix <- file.path(folder_path,item["model_opt"])
    cat("\n--- Running:", item[["model_opt"]], item[["category"]], paste(item[["levels"]], collapse = "_"), "\n")
    
    tryCatch(
      { 
        run_bayesian_modeling(item[["category"]], 
                              item[["levels"]], 
                              prefix, 
                              item[["model_specific"]],
                              item[["prior_specific"]],
                              df_final_data,
                              phoneme_levels
        )
        
      },
      error = function(e) {
        warning(sprintf(
          "Error fitting model: %s | Category: %s | Levels: %s\nMessage: %s",
          item[["model_opt"]],
          item[["category"]],
          paste(item[["levels"]], collapse = "_"),
          e$message
        ))
        failures[[length(failures) + 1]] <<- list(
          model_opt = item[["model_opt"]],
          category = item[["category"]],
          levels = item[["levels"]],
          error_message = e$message
        )
      }
    )
  }
  if (length(failures) > 0) {
    cat("\n--- The following model/data combinations failed:\n")
    print(failures)
    failure_df <- do.call(rbind, lapply(failures, as.data.frame))
    failure_df$timestamp <- Sys.time()
    log_path <- file.path(Paths$modeling_dir, "failed_models_log.txt")
    if (!file.exists(log_path)) {
      write.table(
        failure_df,
        file = file.path(Paths$modeling_dir, "failed_models_log.txt"),
        row.names = FALSE,
        quote = FALSE,
        sep = "\t"
      )
    } else {
      write.table(
        failure_df,
        file = log_path,
        row.names = FALSE,
        quote = FALSE,
        sep = "\t",
        col.names = FALSE,
        append = TRUE
      )
    }
  } else {
    cat("\n All models ran successfully.\n")
  }
  
  invisible(failures)
}