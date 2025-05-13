import("dplyr") 
import("stringr")
import("tidyr")
import("ggplot2")
import("utils")
import("purrr")
import("stats") 


export("visualize_curves_funct")
visualize_curves_funct <- function(phoneme_group_str,reference_col_str,posterior_samples){

  # Remove figures
  #if (dev.cur() != 1) {  # Device 1 is always the null device
  #  dev.off()
  #}
  
  #-------------------------------------
  # Visualize Logit Curves
  #-------------------------------------
  
  ##############################################################################
  #-----------------------------------------------
  # Posterior samples of discrimination parameter
  #-----------------------------------------------
  
  # Extract posterior samples for alpha (discrimination)
  alpha_samples <- posterior_samples %>%
    select(starts_with("b_logalpha"))
  
  
  
  # Rename the intercept column to "AO"
  colnames(alpha_samples)[1] <- reference_col_str
  
  
  # Extract original phoneme names from column names (excluding the intercept)
  phoneme_names <- colnames(alpha_samples)[-1] %>%
    str_remove("b_logalpha_expected_phoneme")
  
  # Get the full column names for referencing
  full_col_names <- colnames(alpha_samples)[-1]
  
  # Add the intercept (AO) to each phoneme coefficient, but don't exponentiate yet
  alpha_samples <- alpha_samples %>%
    mutate(across(all_of(full_col_names), ~ . + .data[[reference_col_str]]))
  
  # Now, exponentiate everything (including AO itself)
  alpha_samples <- alpha_samples %>%
    mutate(across(everything(), exp))
  
  # Rename columns according to the extracted phoneme names
  colnames(alpha_samples) <- c(reference_col_str, phoneme_names)
  
  # Check the transformed data
  #head(alpha_samples)
  
  #-----------------------------------------------
  # Posterior samples of difficulty parameter
  #-----------------------------------------------
  
  # Extract posterior samples for beta (difficulty), excluding `b_eta_age_months`
  beta_samples <- posterior_samples %>%
    select(starts_with("b_eta"), -matches("b_eta_age_months"))
  
  # Rename the intercept column to "AO"
  colnames(beta_samples)[1] <- reference_col_str
  
  # Extract original phoneme names from column names (excluding the intercept)
  phoneme_names <- colnames(beta_samples)[-1] %>%
    str_remove("b_eta_expected_phoneme")
  
  # Get the full column names for referencing
  full_col_names <- colnames(beta_samples)[-1]
  
  # Add the intercept (AO) to each phoneme coefficient
  beta_samples <- beta_samples %>%
    mutate(across(all_of(full_col_names), ~ . + .data[[reference_col_str]]))
  
  
  # Negate everything (including AO)
  beta_samples <- beta_samples %>%
    mutate(across(everything(), ~ (-.)))
  
  
  # Rename columns according to the extracted phoneme names
  colnames(beta_samples) <- c(reference_col_str, phoneme_names)
  
  # Check the transformed data
  head(beta_samples)
  
  #-----------------------------------------------------------------------
  # Plot logistic curves (median, IQR according to posterior distribution)
  #-----------------------------------------------------------------------
  
  # Define the range of theta values
  theta_values <- seq(-3, 7, length.out = 100)
  
  # Prepare a data frame to store results
  logistic_curves <- data.frame()
  
  # Loop over each phoneme
  for (phoneme in colnames(alpha_samples)) {
    
    # Extract posterior samples for the current phoneme
    alpha_vals <- alpha_samples[[phoneme]]
    beta_vals <- beta_samples[[phoneme]]
    
    # Generate logistic curves for each posterior sample
    phoneme_curves <- map_dfr(1:length(alpha_vals), function(i) {
      tibble(
        theta = theta_values,
        probability = 1 / (1 + exp(-alpha_vals[i] * (theta_values - beta_vals[i]))),
        sample = i
      )
    })
    
    # Add phoneme label
    phoneme_curves <- phoneme_curves %>%
      mutate(Phoneme = phoneme)
    
    # Combine with other phonemes
    logistic_curves <- bind_rows(logistic_curves, phoneme_curves)
  }
  
  # Summarize the posterior logistic curves by theta and phoneme
  logistic_summary <- logistic_curves %>%
    group_by(Phoneme, theta) %>%
    summarise(
      median_prob = median(probability),
      lower_iqr = quantile(probability, 0.25),
      upper_iqr = quantile(probability, 0.75),
      .groups = 'drop'
    )
  
  # Plotting the curves
  logistic_plot <- ggplot(logistic_summary, aes(x = theta, y = median_prob, color = Phoneme)) +
    geom_line(linewidth = 1) +
    geom_ribbon(aes(ymin = lower_iqr, ymax = upper_iqr, fill = Phoneme), alpha = 0.2) +
    theme_minimal(base_size = 16) +
    labs(
      x = "Latent Trait (θ)",
      y = "Accuracy",
      fill = "Phoneme",
      color = "Phoneme"
    )
  
  # Display the plot
  print(logistic_plot)
  plot_name <- paste0("logistic_plot_",phoneme_group_str,".png")
  plot_place <- paste0("./output/bayesian_model/",plot_name)
  ggsave(plot_place, plot = logistic_plot, width = 8, height = 6, dpi = 300)
}