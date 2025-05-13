# Load packages
library(dplyr)
library(tidyr)
library(psych)    # For factor analysis and scree plot
library(readr)   
library(brms)     # For Bayesian analysis
library(splines)  # For natural splines
library(tidyverse)

# Create new environment?

fit_bayesian_model_func<- function(df_final,phoneme_levels,phoneme_group_str){
  #x: numeric vector - input data
  # y: character string - label for the data
  if (!is.numeric(x)) stop("Argument 'x' must be numeric")
  
  
  target_phonemes <- phoneme_levels$Consonants$Level6
  phoneme_group_str <- "Consonants_Level6"
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
  
  model_name = paste0("model_", phoneme_group_str,".RData")
  model_place = paste0("./data/processed_data/",model_name)
  save(model, file = model_place)
  #save(model, file = "./data/processed_data/model.RData")
}
  