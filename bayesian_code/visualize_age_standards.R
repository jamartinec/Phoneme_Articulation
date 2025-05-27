import("dplyr")
import("brms")
import("tidyr")
import("ggplot2")


visualize_age_standards_funct <- function(model, phonemes, phoneme_group_str, reference_col_str, posterior_samples) {

  # Create a grid of new data
  ages <- seq(0, 90, by = 1)
  newdata <- expand.grid(
    expected_phoneme = phonemes,
    age_months = ages
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
  
  # Add predictors
  newdata <- newdata %>%
    mutate(
      eta_age_months = age_months,
      phi_age_months = age_months
    ) %>%
    add_dummies("eta_", "expected_phoneme", nonref_phonemes) %>%
    add_dummies("logalpha_", "expected_phoneme", nonref_phonemes) %>%
    add_dummies("phi_", "expected_phoneme", nonref_phonemes)
  

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
  
  # Plot age curves
  age_plot <- ggplot(plot_data, aes(x = (age_months + 30) / 12, y = q50, color = expected_phoneme)) +
    geom_line(linewidth = 1.1) +
    geom_ribbon(aes(ymin = q025, ymax = q975, fill = expected_phoneme), alpha = 0.2, color = NA) +
    geom_ribbon(aes(ymin = q25, ymax = q75, fill = expected_phoneme), alpha = 0.4, color = NA) +
    facet_wrap(~ expected_phoneme) +
    labs(
      x = "Age (years)",
      y = "Predicted articulation accuracy"
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
  
  # Display the plot
  print(age_plot)
  plot_name <- paste0("age_plot_",phoneme_group_str,".png")
  plot_place <- paste0("./output/bayesian_model/",plot_name)
  ggsave(plot_place, plot = age_plot, width = 8, height = 6, dpi = 300)
  
}
