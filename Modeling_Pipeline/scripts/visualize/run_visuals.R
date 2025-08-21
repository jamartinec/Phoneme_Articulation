import("tidyverse")
import("dplyr")
import("posterior")
lib_visuals11 <- moudules::use("./Modeling_Pipeline/scripts/visualize/visualize_age_standards_binomial.R")

run_visuals <- function(phoneme_levels_filepath,
                        df_final_file_path,
                        instance){
  
  # no se si vale la pena leerlo aca, mejor pasarlo como df?
  phoneme_levels <- readRDS(phoneme_levels_filepath)
  df_final <- readRDS(df_final_file_path)
  agerange <- range(df_final$age_months)
  phoneme_numscore_mode <- readRDS(phoneme_numscore_mode_file_path)
  fitted_model <- readRDS(paste0(instance$fitted_model_file_path,".rds"))
  
  
  
  # Extract posterior samples
  posterior_samples <- as_draws_df(model)
  lib_visuals11$visualize_age_standards_funct(
    phoneme_numscore_mode,                                        
    agerange,
    instance,
    fitted_model
  )
}