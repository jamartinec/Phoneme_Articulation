# Modeling Alternatives

# Model 0: Baseline Model
- Model: Beta regression 
default_formulas <- list(
    eta_formula = ~ 1 + expected_phoneme +  age_months + (1 | speaker),
    logalpha_formula =  ~ 1 + expected_phoneme,  
    phi_formula = ~ 1 + expected_phoneme + age_months
  )
  
# Model 1: 
model_specific <- list(
  phi_formula = ~ 1 + expected_phoneme
)

# Model 2: 
model_specific <- list(
  phi_formula = ~ 1 + age_months
)