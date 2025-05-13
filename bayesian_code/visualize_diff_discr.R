import("dplyr") 
import("stringr")
import("tidyr")
import("ggplot2")
import("utils")
import("purrr")
import("stats") 


export("visualize_diff_discr_funct")
visualize_diff_discr_funct <- function(phoneme_group_str,reference_col_str,posterior_samples){
  #-------------------------------------
  # Visualize Bayesian Model
  #-------------------------------------
  
  
  # Remove figures
  #if (dev.cur() != 1) {  # Device 1 is always the null device
 #    dev.off()
  #}
  
  
  
  #-----------------------------------------------
  # Posterior samples of discrimination parameter
  #-----------------------------------------------
  
  # Extract posterior samples for alpha (discrimination)
  # Extract all posterior samples for the discrimination parameter, denoted logalpha
  # In brms, b_logalalpha_* are the fixed effect coefficients in the linear model logalpha
  
  
  alpha_samples <- posterior_samples %>%
    select(starts_with("b_logalpha"))
  

  # Rename the intercept column to "AO"
  # Here "A0" is the reference category. 
  colnames(alpha_samples)[1] <- reference_col_str
  
  # Extract original phoneme names from column names (excluding the intercept)
  phoneme_names <- colnames(alpha_samples)[-1] %>%
    str_remove("b_logalpha_expected_phoneme")
  
  # Get the full column names for referencing
  full_col_names <- colnames(alpha_samples)[-1]
  
  # Add the intercept (AO) to each phoneme coefficient, but don't exponentiate yet
  # This applies log(alpha_j) = intercept +\beta_j for each phoneme j
  alpha_samples <- alpha_samples %>%
    mutate(across(all_of(full_col_names), ~ . + .data[[reference_col_str]]))
  
  # Now, exponentiate everything (including AO itself)
  alpha_samples <- alpha_samples %>%
    mutate(across(everything(), exp))
  
  # Rename columns according to the extracted phoneme names
  colnames(alpha_samples) <- c(reference_col_str, phoneme_names)
  
  # Check the transformed data
  #head(alpha_samples)
  
  # Convert alpha_samples to long format for plotting
  alpha_samples <- alpha_samples %>%
    pivot_longer(cols = everything(), names_to = "Phoneme", values_to = "Alpha")
  
  #-----------------------------------------------
  # Posterior samples of difficulty parameter
  #-----------------------------------------------
  
  # Extract posterior samples for beta (difficulty), excluding `b_eta_age_months`
  # This grabs the intercept and all coefficients related to expected_phoneme, but 
  # excludes b_eta_age_months (a fixed effect for age, not phoneme-specific)
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
  # Larger negative logit corresponds to higher difficulty
  # High Beta -> easier phoneme
  # Low Beta -> harder phoneme
  
  beta_samples <- beta_samples %>%
    mutate(across(everything(), ~ (-.)))
  
  # Rename columns according to the extracted phoneme names
  colnames(beta_samples) <- c(reference_col_str, phoneme_names)
  
  # Check the transformed data
  head(beta_samples)
  
  # Convert beta_samples to long format for plotting
  beta_samples <- beta_samples %>%
    pivot_longer(cols = everything(), names_to = "Phoneme", values_to = "Beta")
  
  
  #-----------------------------------------------
  # Plot posterior densities
  #-----------------------------------------------
  
  
  # Plotting Alpha (Discrimination)
  # Shows how each phoneme varies in its discrimination ability
  # High /alpha --> Sharper separation between correct and incorrect responses
  # Low /alpha --> less informative ites (less sensitive to differences?)
  alpha_plot <- ggplot(alpha_samples, aes(x = Alpha, fill = Phoneme)) +
    geom_density(alpha = 0.6) +
    theme_minimal(base_size = 16) +  # Increase the base font size
    theme(
      axis.title = element_text(face = "bold"),  # Bold axis labels
      legend.title = element_text(face = "bold", size = 16),  # Bold legend title
      legend.text = element_text(size = 14)  # Increase legend text size
    ) +
    labs(x = "Discrimination",
         y = "Posterior density",
         fill = "Phoneme")  # Adds a bolded title for the legend
  
  # Plotting Beta (Difficulty)
  # Higher values = easier phonemes
  beta_plot <- ggplot(beta_samples, aes(x = Beta, fill = Phoneme)) +
    geom_density(alpha = 0.6) +
    theme_minimal(base_size = 16) +  # Increase the base font size
    theme(
      axis.title = element_text(face = "bold"),  # Bold axis labels
      legend.title = element_text(face = "bold", size = 16),  # Bold legend title
      legend.text = element_text(size = 14)  # Increase legend text size
    ) +
    labs(x = "Difficulty",
         y = "Posterior density",
         fill = "Phoneme")  # Adds a bolded title for the legend
  
  # Print the plots
  print(alpha_plot)
  
  
  
  plot_name <- paste0("discrimination_plot_",phoneme_group_str,".png")
  plot_place <- paste0("./output/bayesian_model/",plot_name)
  ggsave(plot_place, plot = alpha_plot, width = 8, height = 6, dpi = 300)
  
  print(beta_plot)
  plot_name <- paste0("difficulty_plot_",phoneme_group_str,".png")
  plot_place <- paste0("./output/bayesian_model/",plot_name)
  ggsave(plot_place, plot = beta_plot, width = 8, height = 6, dpi = 300)
}