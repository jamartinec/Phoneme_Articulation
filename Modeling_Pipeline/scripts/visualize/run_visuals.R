import("tidyverse")
import("dplyr")
import("posterior")

Paths <- modules::use("./Modeling_Pipeline/pipeline/config/file_paths.R")
lib_visuals11 <- modules::use("./Modeling_Pipeline/scripts/visualize/visualize_age_standards.R")
preprocessing_lib  <- modules::use("./Modeling_Pipeline/scripts/preprocess/Preprocessing.R")


export("plot_one_model")

# Modified on nov 12
plot_one_model <- function(#model_type,
                            phoneme_numscore_mode,
                            df_final,
                            instance){
  
  agerange <- range(df_final$age_months)
  fitted_model <- readRDS(paste0(instance$fitted_model_file_path,".rds"))
  model_type <- instance$model_type
  # Extract posterior samples
  #posterior_samples <- as_draws_df(model)
  lib_visuals11$visualize_age_standards_funct(
    model_type,
    phoneme_numscore_mode,                                        
    agerange,
    instance,
    fitted_model
  )
}

export("iterate_plots")
iterate_plots <- function(model_type,
                          #phoneme_levels_filepath,
                          #phoneme_df,
                          #df_final_file_path,
                          df_final,
                          #phoneme_numscore_mode_file_path,
                          phoneme_numscore_mode,
                          list_of_instances){
  
  
  agerange <- range(df_final$age_months)
  #phoneme_numscore_mode <- readRDS(phoneme_numscore_mode_file_path)
  
  for (instance in list_of_instances){
    
    
    tryCatch({
      # December 12 modification: updated call to `plot_one_model`
      plot_one_model(phoneme_numscore_mode,agerange,instance)
    }, error = function(e) {
      warning(sprintf(
        "Error plotting model: %s | Category: %s | Levels: %s\nMessage: %s",
        instance$model_name, instance$category, paste(instance$levels, collapse = "_"), e$message
      ))
      
      print(e)
      
    })
  } 
  
}

export("iterate_plots_modified")
iterate_plots_modified <- function(
                          #model_type,
                          #df_final,
                          #phoneme_numscore_mode,
                          list_of_instances,
                          preprocessed_cache
                          ){
  

  
  for (instance in list_of_instances){
    prep <- preprocessing_lib$get_preprocessed_for_instance(instance, preprocessed_cache)
    df_final <- prep$df_final
    phoneme_numscore_mode <-prep$phoneme_numscore_mode
    
    
    tryCatch({
      # December 12 modification: updated call to `plot_one_model`
      plot_one_model(phoneme_numscore_mode,df_final,instance)
    }, error = function(e) {
      warning(sprintf(
        "Error plotting model: %s | Category: %s | Levels: %s\nMessage: %s",
        instance$model_name, instance$category, paste(instance$levels, collapse = "_"), e$message
      ))
      
      print(e)
      
    })
  } 
  
}