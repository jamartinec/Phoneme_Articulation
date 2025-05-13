#-----------------------
# Run Bayesian analysis
#-----------------------
import("dplyr")
import("tidyr")
import("psych") # For factor analysis and scree plot
import("readr") 
import("brms") # For Bayesian analysis
import("splines") # For natural splines
import("tidyverse")
import("utils")
 
export("fit_bayesian_model_funct")
fit_bayesian_model_funct <- function(df_filtered,target_phonemes,phoneme_group_str){

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
}