library(ggplot2)
library(dplyr)
library(tidyr)

# Extract posterior means for logalpha and eta from the model
posterior_samples <- fixef(model)

# Extract fixed effects
logalpha_intercept <- posterior_samples["logalpha_Intercept", "Estimate"]
eta_intercept <- posterior_samples["eta_Intercept", "Estimate"]

# Extract per-phoneme fixed effects
logalpha_phoneme <- posterior_samples[grep("logalpha_expected_phoneme", rownames(posterior_samples)), "Estimate"]
eta_phoneme <- posterior_samples[grep("eta_expected_phoneme", rownames(posterior_samples)), "Estimate"]

# Create a dataframe for phoneme-specific values
phoneme_levels <- levels(df_filtered$expected_phoneme)  # Ensure phoneme levels are correct

phoneme_params <- data.frame(
  expected_phoneme = phoneme_levels,
  logalpha_expected_phoneme = logalpha_phoneme,
  eta_expected_phoneme = eta_phoneme
)

# Add global intercept values to each phoneme
phoneme_params <- phoneme_params %>%
  mutate(
    logalpha_total = logalpha_intercept + logalpha_expected_phoneme,
    eta_total = eta_intercept + eta_expected_phoneme
  )

# Generate x values from -3 to 3
x_seq <- seq(-3, 3, length.out = 100)

# Expand grid to get all phoneme-x pairs
curve_data <- expand.grid(
  expected_phoneme = phoneme_levels,
  x = x_seq
)

# Merge phoneme-specific parameters
curve_data <- left_join(curve_data, phoneme_params, by = "expected_phoneme")

# Compute logistic probability for each phoneme and x value
curve_data <- curve_data %>%
  mutate(
    slope = exp(logalpha_total),
    eta_shifted = x + eta_total,
    probability = 1 / (1 + exp(-slope * eta_shifted))
  )

ggplot(curve_data, aes(x = x, y = probability, color = expected_phoneme)) +
  geom_line(size = 1.2) +  # Logistic curve for each phoneme
  labs(
    title = "Logistic Curves for Each Phoneme",
    x = "Latent Ability (x)",
    y = "Predicted Articulation Probability",
    color = "Phoneme"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))