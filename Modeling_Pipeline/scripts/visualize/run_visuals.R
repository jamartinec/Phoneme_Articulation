import("tidyverse")
import("dplyr")
import("posterior")
lib_visuals11 <- modules::use("./Modeling_Pipeline/scripts/visualize/visualize_age_standards.R")
read_instances_specifications_lib <- modules::use("./Modeling_Pipeline/scripts/preprocess/read_instance_specification.R")
export("plot_one_model")
#dec12":modified
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
  
  # no se si vale la pena leerlo aca, mejor pasarlo como df?
  #phoneme_df <- readRDS(phoneme_levels_filepath)
  #df_final <- readRDS(df_final_file_path)
  agerange <- range(df_final$age_months)
  #phoneme_numscore_mode <- readRDS(phoneme_numscore_mode_file_path)
  
  for (instance in list_of_instances){
    
    
    tryCatch({
      #dec12:modified#plot_one_model(model_type,phoneme_numscore_mode,agerange,instance)
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
  
  # no se si vale la pena leerlo aca, mejor pasarlo como df?
  #phoneme_df <- readRDS(phoneme_levels_filepath)
  #df_final <- readRDS(df_final_file_path)
  
  
  # agerange <- range(df_final$age_months)
  #phoneme_numscore_mode <- readRDS(phoneme_numscore_mode_file_path)
  
  for (instance in list_of_instances){
    prep <- read_instances_specifications_lib$get_preprocessed_for_instance(instance, preprocessed_cache)
    df_final <- prep$df_final
    phoneme_numscore_mode <-prep$phoneme_numscore_mode
    
    
    tryCatch({
      #dec12:modified#plot_one_model(model_type,phoneme_numscore_mode,agerange,instance)
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