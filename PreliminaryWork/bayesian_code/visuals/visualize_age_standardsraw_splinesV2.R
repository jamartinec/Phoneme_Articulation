import("dplyr")
import("brms")
import("tidyr")
import("ggplot2")
import("splines")
Paths <- modules::use("./bayesian_code/utils/file_paths.R")

load_from_rdata <- function(file, obj_name) {
  temp_env <- new.env()
  load(file, envir = temp_env)
  
  if (!exists(obj_name, envir = temp_env)) {
    stop(paste("Object", obj_name, "not found in", file))
  }
  
  return(get(obj_name, envir = temp_env))
}


visualize_age_standards_funct <- function(model_name, model, phonemes, phoneme_group_str, reference_col_str, posterior_samples) {
  
  # Create a grid of new data
  ages <- seq(0, 90, by = 1)
  newdata <- expand.grid(
    expected_phoneme = phonemes,
    age_months = ages
  )
  
  #splines basis for ns(age, 3)
  splines_basis<-ns(newdata$age_months,df=3)
  colnames(splines_basis) <-paste0("ns(age_months, 3)",1:3)
  newdata <- bind_cols(newdata,as.data.frame(splines_basis))
  
  # Posterior predictive draws
  predicted <- posterior_predict(
    model,
    newdata = newdata,
    allow_new_levels = TRUE,
    sample_new_levels = "gaussian",
    draws = 2000
  )
  
  # Add row index to newdata
  newdata$row_id <- 1:nrow(newdata)
  
  # Convert predicted draws to long format
  pred_df <- as.data.frame(predicted)
  pred_df$draw <- 1:nrow(pred_df)
  long_preds <- pred_df %>%
    pivot_longer(
      cols = -draw,
      names_to = "row_id",
      values_to = "mean_prob"
    ) %>%
    mutate(row_id = as.integer(gsub("V", "", row_id))) %>%
    left_join(newdata, by = "row_id")
  
  # Load filtered data for plot overlay
  folder_path <- Paths$filtered_data_dir
  filename <-  paste0(phoneme_group_str, ".RData") 
  filtered_file_path <- file.path(folder_path,filename)
  df_filtered <-load_from_rdata(filtered_file_path,"df_filtered")
  
  
  # Summarize posterior predictive intervals
  plot_data <- long_preds %>%
    group_by(expected_phoneme, age_months) %>%
    summarise(
      q025 = stats::quantile(mean_prob, 0.025),
      q25  = stats::quantile(mean_prob, 0.25),
      q50  = stats::quantile(mean_prob, 0.5),
      q75  = stats::quantile(mean_prob, 0.75),
      q975 = stats::quantile(mean_prob, 0.975)
    )

  
  
  # Non reference phonemes
  nonref_phonemes <- setdiff(phonemes, reference_col_str)
  
  # Helper to add dummy variables
  add_dummies <- function(df, var_prefix, base_name, nonref_phonemes) {
    for (ph in nonref_phonemes) {
      col_name <- paste0(var_prefix, base_name, ph)
      df[[col_name]] <- as.integer(df$expected_phoneme == ph)
    }
    df
  }
  
  # Add predictors. Not needed for the simple model model0_Version2
  #newdata <- newdata %>%
    #mutate(
      #eta_age_months = age_months,
      #phi_age_months = age_months
    #) %>%
    #add_dummies("eta_", "expected_phoneme", nonref_phonemes) %>%
    #add_dummies("logalpha_", "expected_phoneme", nonref_phonemes) %>%
    #add_dummies("phi_", "expected_phoneme", nonref_phonemes)
  
  
  
  age_plot <- ggplot(plot_data, aes(x = (age_months + 30) / 12, y = q50, color = expected_phoneme)) +
    geom_line(linewidth = 1.1, color = "steelblue") +
    geom_ribbon(aes(ymin = q025, ymax = q975, fill = expected_phoneme), alpha = 0.2, color = NA) +
    geom_ribbon(aes(ymin = q25, ymax = q75, fill = expected_phoneme), alpha = 0.4, color = NA) +
    facet_wrap(~ expected_phoneme) +
    geom_point(
      data = df_filtered,
      aes(x = (age_months + 30) / 12, y = mean_prob, color = expected_phoneme),
      inherit.aes = FALSE,
      alpha = 0.5,
      size = 1.5,
      #color ="darkred"
    )+
    labs(
      x = "Age (years)",
      y = "Phoneme goodness score"
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
  
  
  
  plot_name <- paste0("age_plot_",phoneme_group_str,".png")
  plot_place <- paste0("./output/bayesian_model/", model_name,"/", plot_name)
  ggsave(plot_place, plot = age_plot, width = 8, height = 6, dpi = 300)
  
  # Display the plot
  print(age_plot)
  
}