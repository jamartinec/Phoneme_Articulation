import("dplyr")
import("brms")
import("tidyr")
import("ggplot2")
import("splines")
import("tidybayes")
Paths <- modules::use("./bayesian_code/utils/file_paths.R")


#### para checkear que funcione, comparemos#################################

# library("dplyr")
# library("brms")
# library("tidyr")
# library("ggplot2")
# library("splines")
# library("tidybayes")
# Paths <- modules::use("./bayesian_code/utils/file_paths.R")
# 
# load_from_rdata <- function(file, obj_name) {
#   temp_env <- new.env()
#   load(file, envir = temp_env)
#   
#   if (!exists(obj_name, envir = temp_env)) {
#     stop(paste("Object", obj_name, "not found in", file))
#   }
#   
#   return(get(obj_name, envir = temp_env))
# }
# 
# folder_path <- Paths$processed_data_dir
# filename <-  paste0("phoneme_numscore_mode_binomialxphoneme_Prob_singleWords", ".RData") 
# phoneme_numscore_mode_file_path <- file.path(folder_path,filename)
# phoneme_numscore_mode <-load_from_rdata(phoneme_numscore_mode_file_path,"phoneme_numscore_mode")
# 
# model_path_file <- Paths$processed_data_dir
# model_path_file <- file.path(model_path_file,"model_binomial_Probability_singleWords")
# model_file_name <- "model_Vowels_Levelphoneme1.rds"
# model_path_file <- file.path(model_path_file,model_file_name)
# model<-readRDS(model_path_file)
# 
# phonemes <- c("AA", "AH")
# # Load filtered data for plot overlay
# folder_path <- Paths$filtered_data_dir
# # JUST FOR COMPARING
# folder_path <- file.path(folder_path,"filtered_data_phoneme_binomial_probability_singleWords")
# phoneme_group_str<- "Vowels_Levelphoneme1"
# filename <-  paste0(phoneme_group_str, ".RData")
# filtered_file_path <- file.path(folder_path,filename)
# df_filtered <-load_from_rdata(filtered_file_path,"df_filtered")
# #############################################################################

export("visualize_age_standards_funct")
visualize_age_standards_funct <- function(
                                          #phoneme_num_score_mode_file_path,# asumiendo que vamos a correr binomial
                                          phoneme_numscore_mode,                                        
                                          agerange,
                                          instance,
                                          fitted_model
                                          ) {
  

  
  
  # Esta funcion deberia poder recibir tanto el path a phoneme_num_score_mode
  # recordar que viene del preprocesamiento, como el objeto dataframe si se cuenta
  # con el. La logica es que algunas veces queremos correr solo el tramo de visualizacion
  # en cuyo caso la funcion debe acceder leer el archivo, y otras veces si estamos corriendo
  # dentro del pipeline ya contamos con el objeto.
  
  #phoneme_num_score_mode_file_path <- NULL
  # phoneme_numscore_mode <-load_from_rdata(phoneme_numscore_mode_file_path,"phoneme_numscore_mode")
  
  
  # we should use the same age_rank from df_final (preprocessing output)
  # we have included a speaker column for managind the randomo effect groups.
  ## just for debuggin-->  
  
  newdata <- tidyr::crossing(
    age_months       = seq(agerange[1], agerange[2], by = 1), # RANGE!#seq(0, 90, by = 1), # RANGE!
    expected_phoneme = instance$phonemes,#phonemes,
    speaker          = "fake" # Tristan's recommendation.
    
  )
  
  
  # Use the mode across participants for that particular phoneme.
  # this is a step which is particular for binomial model.
  newdata <- newdata %>%
    left_join(phoneme_numscore_mode, by = "expected_phoneme") %>%
    mutate(num_score = mode_num_score) %>%
    select(-mode_num_score)  
  
  
  # The posterior predictive draws will be done using tidybayes
  # re_formula = NA sets the random effects values to 0 and make predictions
  # re_formula = NULL, allow_new_levels = TRE will resample random 
  # effect values from existing speakers as a way of simulating/averaging over 
  # speakers.
  predicted<- tidybayes::add_predicted_draws(
    model,
    newdata = newdata,
    re_formula = NULL,
    allow_new_levels = TRUE
  )
  
  predicted <- predicted|>
    mutate(
      sum_score = .prediction,
      draw = .draw,
      row_id = .row,
      proportion = sum_score/num_score
    ) |>
    select(-c(.prediction,.draw))
  
  print(class(predicted))
  print(dim(predicted))
  print(predicted)
  
  # Load filtered data for plot overlay
  df_filtered <- readRDS(instance$filtered_file_path)
  

  # Summarize posterior predictive intervals
  plot_data <- predicted %>%
    group_by(expected_phoneme, age_months) %>%
    median_qi(proportion, .width = c(0.5, 0.95)) %>%
    pivot_wider(
      names_from = .width,
      values_from = c(.lower, .upper)
    ) %>%
    rename(
      q025 = `.lower_0.95`,
      q975 = `.upper_0.95`,
      q25  = `.lower_0.5`,
      q75  = `.upper_0.5`,
      q50  = proportion
    )
  
  
  # No estoy seguro sobre como tratar esta transformacion de las edades para el caso
  # de la informacion del pplr. Mirar instancia, hablar con Amy.
  
  age_plot <- ggplot(plot_data, aes(x = (age_months+30) / 12, y = q50, color = expected_phoneme)) +
    geom_line(linewidth = 1.1, color = "steelblue") +
    geom_ribbon(aes(ymin = q025, ymax = q975, fill = expected_phoneme), alpha = 0.2, color = NA) +
    geom_ribbon(aes(ymin = q25, ymax = q75, fill = expected_phoneme), alpha = 0.4, color = NA) +
    facet_wrap(~ expected_phoneme) +
    geom_point(
      data =  df_filtered %>%
        mutate(mean_prob_jitter = (sum_score/num_score) + stats::runif(n(), -0.02, 0.02)),  # add jitter,
      aes(x = (age_months+30) / 12, y = mean_prob_jitter, color = expected_phoneme),
      inherit.aes = FALSE,
      alpha = 0.5,
      size = 1.5,
    )+
    labs(
      x = "Age (years)",
      y = "Probability of success"
    ) +
    
    theme_minimal(base_size = 14) +
    theme(
      axis.title = element_text(size = 16, face = "bold"),
      axis.text = element_text(size = 14),
      strip.text = element_text(size = 14, face = "bold"),
      legend.title = element_blank(),
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90"),
      plot.title = element_blank(),
      plot.subtitle = element_blank()
    )
  
  
  # model_name, phoneme_group_str se sacan de la instancia
  #plot_name <- paste0("age_plot_",phoneme_group_str,".png")
  plot_name <- "age_plot.png"
  plot_file_path <- file.path(instance$plots_folder_path,plot_name)
  #plot_place <- paste0("./output/bayesian_model/", model_name,"/", plot_name)
  ggsave(plot_file_path, plot = age_plot, width = 8, height = 6, dpi = 300)
  
  # Display the plot
  print(age_plot)
  
}