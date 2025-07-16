import("tidyverse")
import("dplyr")
import("posterior")

lib_visuals1 <- modules::use("bayesian_code/visuals/visualize_diff_discr.R")
lib_visuals2 <- modules::use("bayesian_code/visuals/visualize_curves.R")
lib_visuals3 <- modules::use("bayesian_code/visuals/visualize_latent_ability.R")
lib_visuals4 <- modules::use("bayesian_code/visuals/visualize_age_standards.R")
#lib_visuals5 <- modules::use("bayesian_code/visuals/visualize_age_standards_andraw.R")
lib_visuals6 <- modules::use("bayesian_code/visuals/visualize_age_standardsraw_splines.R")

export("run_visuals")
export("iterate_run_visuals")

run_visuals <- function(category, levels, prefix) {
  
  data_place <- sub("^(.*?processed_data/).*", "\\1", prefix)
  model_name <- sub(".*/processed_data/([^/]+)/.*", "\\1", prefix)
  print("MODEL NAME: ")
  print(model_name)
  
  # Load phoneme levels
  tmp_env <- new.env()
  loaded_data_objects_1 <- load(paste0(data_place, "phoneme_levels.RData"),
                                envir = tmp_env)
  phoneme_levels <- tmp_env[[loaded_data_objects_1[1]]]
  phonemes <- unlist(phoneme_levels[[category]][levels])
  reference_col_str <- min(phonemes)
  phoneme_group_str <- paste(c(category, levels), collapse = "_")
  
  # Remove "ZH" since it is not included in dataset
  phonemes <- setdiff(phonemes, "ZH")
  
  # Load data and model directly into current environment
  loaded_data_objects_2 <- load(paste0(data_place, "df_final.RData"),
                                envir = tmp_env)  
  df_final <- tmp_env[[loaded_data_objects_2[1]]]
  
  # loaded_data_objects_3 <- load(paste0(prefix, "model_", phoneme_group_str, ".RData"),
  #                               envir = tmp_env) 
  # model <- tmp_env[[loaded_data_objects_3[1]]]
  model <- readRDS(paste0(prefix, "model_", phoneme_group_str, ".rds"))
  
  # Extract posterior samples
  posterior_samples <- as_draws_df(model)
  
  # Generate visualizations
  
  #lib_visuals1$visualize_diff_discr_funct(model_name, phoneme_group_str, reference_col_str, posterior_samples)
  #lib_visuals2$visualize_curves_funct(model_name, phoneme_group_str, reference_col_str, posterior_samples)
  #lib_visuals3$visualize_latent_ability_funct(model_name,phoneme_group_str, df_final, posterior_samples)
  #lib_visuals4$visualize_age_standards_funct(model_name, model, phonemes, phoneme_group_str, reference_col_str, posterior_samples)
  #lib_visuals5$visualize_age_standards_funct(model_name, model, phonemes, phoneme_group_str, reference_col_str, posterior_samples)
  lib_visuals6$visualize_age_standards_funct(model_name, model, phonemes, phoneme_group_str, reference_col_str, posterior_samples)
}

iterate_run_visuals <- function(list_to_visualize){
  folder_path = "./data/processed_data/"
  for (item in list_to_visualize){
    
    prefix <- paste0(folder_path, item["model_opt"], "/")
    tryCatch({
      run_visuals(item[["category"]], item[["levels"]], prefix)
    }, error = function(e) {
      message(sprintf(
        "Error in iteration for category='%s', levels='%s': %s",
        item[["category"]],
        toString(item[["levels"]]),
        conditionMessage(e)
      ))
      print(e)
      
    })
  }
}