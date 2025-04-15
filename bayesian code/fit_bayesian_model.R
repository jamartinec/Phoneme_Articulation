#-----------------------
# Run Bayesian analysis
#-----------------------
# Load packages
library(dplyr)
library(tidyr)
library(psych)    # For factor analysis and scree plot
library(readr)   
library(brms)     # For Bayesian analysis
library(splines)  # For natural splines
library(tidyverse)

# Load data
#load("../../data/df_final.RData")
load("./data/processed_data/df_final.RData")
load("./data/processed_data/phoneme_levels.RData")

# Phonemes of interest
target_phonemes <- phoneme_levels$Vowels$Level3 
#target_phonemes <- c(phoneme_levels$Vowels$Level1,
                     #phoneme_levels$Vowels$Level2)

#target_phonemes <- c(phoneme_levels$Vowels$Level4,
                     #phoneme_levels$Vowels$Level5)

phoneme_group_str <- "Vowels_Level3"
#phoneme_group_str <- "Vowels_Level1_Level2"

#phoneme_group_str <- "Vowels_Level4_Level5"


head(target_phonemes)

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