# Load necessary packages
import("dplyr")
import("stringr")
import("tidyr")
import("writexl")

export("save_credible_intervals_funct")
save_credible_intervals_funct <- function(phoneme_group_str, reference_col_str, posterior_samples) {
  #-------------------------------
  # Discrimination (logalpha)
  #-------------------------------
  alpha_samples <- posterior_samples %>%
    select(starts_with("b_logalpha"))
  
  colnames(alpha_samples)[1] <- reference_col_str
  phoneme_names <- colnames(alpha_samples)[-1] %>%
    str_remove("b_logalpha_expected_phoneme")
  full_col_names <- colnames(alpha_samples)[-1]
  
  alpha_samples <- alpha_samples %>%
    mutate(across(all_of(full_col_names), ~ . + .data[[reference_col_str]])) %>%
    mutate(across(everything(), exp))
  colnames(alpha_samples) <- c(reference_col_str, phoneme_names)
  
  alpha_samples <- alpha_samples %>%
    pivot_longer(cols = everything(), names_to = "Phoneme", values_to = "Alpha")
  
  alpha_ci <- alpha_samples %>%
    group_by(Phoneme) %>%
    summarise(
      lower_95 = stats::quantile(Alpha, 0.025),
      median = stats::median(Alpha),
      upper_95 = stats::quantile(Alpha, 0.975),
      .groups = "drop"
    ) %>%
    mutate(Parameter = "Discrimination")
  
  #-------------------------------
  # Difficulty (eta)
  #-------------------------------
  beta_samples <- posterior_samples %>%
    select(starts_with("b_eta"), -matches("b_eta_age_months"))
  
  colnames(beta_samples)[1] <- reference_col_str
  phoneme_names <- colnames(beta_samples)[-1] %>%
    str_remove("b_eta_expected_phoneme")
  full_col_names <- colnames(beta_samples)[-1]
  
  beta_samples <- beta_samples %>%
    mutate(across(all_of(full_col_names), ~ . + .data[[reference_col_str]])) %>%
    mutate(across(everything(), ~ -(.)))
  colnames(beta_samples) <- c(reference_col_str, phoneme_names)
  
  beta_samples <- beta_samples %>%
    pivot_longer(cols = everything(), names_to = "Phoneme", values_to = "Beta")
  
  beta_ci <- beta_samples %>%
    group_by(Phoneme) %>%
    summarise(
      lower_95 = stats::quantile(Beta, 0.025),
      median = stats::median(Beta),
      upper_95 = stats::quantile(Beta, 0.975),
      .groups = "drop"
    ) %>%
    mutate(Parameter = "Difficulty")
  
  #-------------------------------
  # Save as Excel file
  #-------------------------------
  ci_table <- bind_rows(alpha_ci, beta_ci) %>%
    select(Parameter, Phoneme, lower_95, median, upper_95)
  
  output_path <- paste0("./output/tables/credible_intervals_", phoneme_group_str, ".xlsx")
  writexl::write_xlsx(ci_table, output_path)
}