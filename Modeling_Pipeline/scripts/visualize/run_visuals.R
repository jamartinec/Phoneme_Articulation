import("tidyverse")
import("dplyr")
import("posterior")
lib_visuals11 <- modules::use("./Modeling_Pipeline/scripts/visualize/visualize_age_standards.R")

export("plot_one_model")
plot_one_model <- function(model_type,
                            phoneme_numscore_mode,
                            agerange,
                            instance){
  
  fitted_model <- readRDS(paste0(instance$fitted_model_file_path,".rds"))

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
      plot_one_model(model_type,phoneme_numscore_mode,agerange,instance)
    }, error = function(e) {
      warning(sprintf(
        "Error plotting model: %s | Category: %s | Levels: %s\nMessage: %s",
        instance$model_name, instance$category, paste(instance$levels, collapse = "_"), e$message
      ))
      
      print(e)
      
    })
  } 
  
  }