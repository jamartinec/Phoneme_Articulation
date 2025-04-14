#-----------------------
# Run Bayesian analysis
#-----------------------

# Load data
#load("../../data/df_final.RData")
load("./data/processed_data/df_final.RData")
load("./data/processed_data/phoneme_levels.RData")

# Phonemes of interest
target_phonemes <- phoneme_levels$Vowels$Level3  

# Filter data to include only those phonemes
df_filtered <- df_final %>%
  filter(expected_phoneme %in% target_phonemes)
df_filtered$expected_phoneme <- as.factor(df_filtered$expected_phoneme)

# Fit the Bayesian beta regression model (2PL model)
model <- brm(
  bf(mean_prob ~ exp(logalpha) * eta, 
     eta ~ 1 + expected_phoneme +  age_months + (1 | speaker),  
     logalpha  ~ 1 + expected_phoneme,  
     phi ~ 1 + expected_phoneme + age_months,  # Precision parameter
     nl = TRUE),  # Non-linear model
  family = Beta(link = "logit"),
  data = df_filtered,
  prior = c(
    prior(normal(0,5), class = "b", nlpar = "eta"),
    prior(normal(0,1), class = "b", nlpar = "logalpha"),
    prior(constant(1), class="sd", group="speaker", nlpar = "eta"),
    prior(normal(0, 1), dpar = "phi", class = "b")
  ),
  chains = 4, iter = 4000, cores = 4
)

save(model, file = "./data/processed_data/model.RData")
