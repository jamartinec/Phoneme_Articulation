import("dplyr") 
import("stringr")
import("tidyr")
import("ggplot2")
import("utils")
import("purrr")
import("stats") 


export("visualize_latent_ability_funct")
visualize_latent_ability_funct <- function(model_name, phoneme_group_str, df_final, posterior_samples){
  #-------------------------------------
  # Visualize Latent Ability by Age
  #-------------------------------------
  
  # Remove figures
  #if (dev.cur() != 1) {  # Device 1 is always the null device
  #  dev.off()
  #}
  

  # Step 1: Extract relevant posterior samples
  # grab all posterior draws for the random intercept/slopes associated with the
  #grouping factor speaker.
  random_effects <- posterior_samples %>%
    select(starts_with("r_speaker"))
  
  #extract the fixed effect coefficient (posterior draws) for the variable age_months
  #in the linear predictor eta. b_eta_age_X is the name used by brms when modeling
  #hierarchical predictors. pulls() turns the one column into a plain numerical vector
  
  fixed_effect_age <- posterior_samples %>%
    select(b_eta_age_months) %>% 
    pull()
  
  # Step 2: Prepare speaker data with their ages (grouped) from df_final
  speaker_info <- df_final %>%
    mutate(
      age_group = case_when(
        age_months <= 48-30 ~ "< 4 years",
        age_months <= 72-30 ~ "4-5 years",
        age_months <= 96-30 ~ "6-7 years",
        TRUE ~ "8+ years"
      )
    ) %>%
    select(speaker, age_group, age_months) %>%
    distinct()
  
  # Extract speaker IDs from the column names of random_effects
  speaker_ids <- colnames(random_effects) %>%
    str_remove("^r_speaker__eta\\[") %>%    # Remove the prefix
    str_remove(",Intercept\\]$")             # Remove the suffix
  
  # Rename columns to match speaker IDs
  colnames(random_effects) <- speaker_ids
  
  # Step 3: Combine random effects with the fixed age effect
  # Adding the fixed age effect for each speaker based on their age in df_final
  random_effects_combined <- random_effects %>%
    pivot_longer(cols = everything(), names_to = "speaker", values_to = "Random_Effect") %>%
    left_join(speaker_info, by = "speaker") %>%
    mutate(Combined_Effect = Random_Effect + (fixed_effect_age * age_months))
  
  # Step 4: Plot posterior densities by age
  plot_random_effects <- ggplot(random_effects_combined, aes(x = Combined_Effect, fill = as.factor(age_group))) +
    geom_density(alpha = 0.6) +
    theme_minimal(base_size = 16) +
    theme(
      axis.title = element_text(face = "bold"),
      legend.title = element_text(face = "bold", size = 16),
      legend.text = element_text(size = 14)
    ) +
    labs(x = "Latent ability",
         y = "Posterior density",
         fill = "Age (months)")
  
  # Display the plot
  print(plot_random_effects)
  plot_name <- paste0("random_effects_",phoneme_group_str,".png")
  plot_place <- paste0("./output/bayesian_model/",model_name,"/",plot_name)
  ggsave(plot_place, plot = plot_random_effects, width = 8, height = 6, dpi = 300)
  
}
  


