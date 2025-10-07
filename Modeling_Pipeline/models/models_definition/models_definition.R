import("brms")
import("splines")
import("stats")

#-------------------------------------------------------------------------------
# Model type: Beta
# Variable modeled: mean_prob

model0 <- bf(
  mean_prob ~ exp(logalpha) * eta, 
  eta ~ 1 + expected_phoneme +  age_months + (1 | speaker),
  logalpha  ~ 1 + expected_phoneme, 
  phi ~ 1 + expected_phoneme + age_months,  # Precision parameter
  family = Beta(link = "logit"),
  nl = TRUE)  # Non-linear model

model1 <- bf(
  mean_prob ~ exp(logalpha) * eta, 
  eta ~ 1 + expected_phoneme +  age_months + (1 | speaker),
  logalpha  ~ 1 + expected_phoneme, 
  phi  ~ 1 + expected_phoneme,  # Precision parameter
  family = Beta(link = "logit"),
  nl = TRUE)  # Non-linear model

model2 <- bf(
  mean_prob ~ exp(logalpha) * eta, 
  eta ~ 1 + expected_phoneme +  age_months + (1 | speaker),
  logalpha  ~ 1 + expected_phoneme, 
  phi ~  1 + age_months,  # Precision parameter
  family = Beta(link = "logit"),
  nl = TRUE)  # Non-linear model

model3 <- bf( # apply inverse logit function to asymptote, for constraint the values
  mean_prob ~  inv_logit(asymptote)/ (1 + exp(-exp(logalpha) * eta)),
  asymptote ~ (1 + expected_phoneme),
  eta ~ 1 + expected_phoneme +  age_months + (1 | speaker),
  logalpha  ~ 1 + expected_phoneme, 
  phi ~ 1 + expected_phoneme + age_months,  # Precision parameter
  family = Beta(link = "identity"),
  nl = TRUE)  # Non-linear model

model4 <- bf(
  mean_prob ~ exp(logalpha) * eta, 
  eta ~ 1 + expected_phoneme +  ns(age_months,3) + (1 | speaker),
  logalpha  ~ 1 + expected_phoneme, 
  phi ~ 1 + expected_phoneme + age_months,  # Precision parameter
  family = Beta(link = "logit"),
  nl = TRUE)  # Non-linear model

model5 <- bf(
  mean_prob ~ exp(logalpha) * eta, 
  eta ~ 1 + expected_phoneme +  ns(age_months,2) + (1 | speaker),
  logalpha  ~ 1 + expected_phoneme, 
  phi ~ 1 + expected_phoneme + age_months,  # Precision parameter
  family = Beta(link = "logit"),
  nl = TRUE)  # Non-linear model

model6 <- bf(
  mean_prob ~ exp(logalpha) * eta, 
  eta ~ 1 + expected_phoneme +  ns(age_months,4) + (1 | speaker),
  logalpha  ~ 1 + expected_phoneme, 
  phi ~ 1 + expected_phoneme + age_months,  # Precision parameter
  family = Beta(link = "logit"),
  nl = TRUE)  # Non-linear model

# versions based on model 3, modifying phi formula
model7 <- bf( # apply inverse logit function to asymptote, for constraing the values
  mean_prob ~  inv_logit(asymptote)/ (1+  exp(-exp(logalpha) * eta)),
  asymptote ~ (1 + expected_phoneme),
  eta ~ 1 + expected_phoneme +  age_months + (1 | speaker),
  logalpha  ~ 1 + expected_phoneme, 
  phi ~  1 + expected_phoneme,  # Precision parameter
  family = Beta(link = "identity"),
  nl = TRUE)  # Non-linear model

model8 <- bf( # apply inverse logit function to asymptote, for constraing the values
  mean_prob ~  inv_logit(asymptote)/ (1+  exp(-exp(logalpha) * eta)),
  asymptote ~ (1 + expected_phoneme),
  eta ~ 1 + expected_phoneme +  age_months + (1 | speaker),
  logalpha  ~ 1 + expected_phoneme, 
  phi ~  1 + age_months,  # Precision parameter
  family = Beta(link = "identity"),
  nl = TRUE)  # Non-linear model

# versions based on model, modifying eta formula (adding splines to age)
model9 <- bf( # apply inverse logit function to asymptote, for constraing the values
  mean_prob ~  inv_logit(asymptote)/ (1+  exp(-exp(logalpha) * eta)),
  asymptote ~ (1 + expected_phoneme),
  eta ~ 1 + expected_phoneme +  ns(age_months,3) + (1 | speaker),
  logalpha  ~ 1 + expected_phoneme, 
  phi ~ 1 + expected_phoneme + age_months,  # Precision parameter
  family = Beta(link = "identity"),
  nl = TRUE)  # Non-linear model

model10 <- bf( # apply inverse logit function to asymptote, for constraing the values
  mean_prob ~  inv_logit(asymptote)/ (1+  exp(-exp(logalpha) * eta)),
  asymptote ~ (1 + expected_phoneme),
  eta ~ 1 + expected_phoneme +  ns(age_months,2) + (1 | speaker),
  logalpha  ~ 1 + expected_phoneme, 
  phi ~ 1 + expected_phoneme + age_months,  # Precision parameter
  family = Beta(link = "identity"),
  nl = TRUE)  # Non-linear model

model11 <- bf( # apply inverse logit function to asymptote, for constraing the values
  mean_prob ~  inv_logit(asymptote)/ (1+  exp(-exp(logalpha) * eta)),
  asymptote ~ (1 + expected_phoneme),
  eta ~ 1 + expected_phoneme +  ns(age_months,4) + (1 | speaker),
  logalpha  ~ 1 + expected_phoneme, 
  phi ~ 1 + expected_phoneme + age_months,  # Precision parameter
  family = Beta(link = "identity"),
  nl = TRUE)  # Non-linear model


# Model0 Version 2 (We want to build a simple model for each phoneme)
model0_Version2 <- bf(
  mean_prob ~ 1 + ns(age_months, 3),
  phi ~ 1  + age_months,  # Precision parameter
  family = Beta(link = "logit")
  #nl = TRUE # Non-linear model
)  
#-------------------------------------------------------------------------------
# Model type: Beta
# Variable modeled: mean_prob
model_binomial <- bf(
  sum_score | trials(num_score) ~ 1 + ns(age_months, 4),
  family = binomial(link = "logit")
)

model_binomialv2 <- bf(
  sum_score | trials(num_score) ~ 1 + ns(age_months, 3),
  family = binomial(link = "logit")
)

model_list <- list(
  "model0_Version2"              = model0_Version2,
  #"model_binomial_dummytest"     = model_binomial,
  "model_binomial_Probability"   = model_binomial,
  "model_binomialv2"             = model_binomialv2
)

################################################################################
# PRIORS:
# Define a list of difFerent priors. For each prior we also specify a list of models
# for which this prior make sense.

prior_binomial = c(
  prior(normal(0, 5), class = "b")
)

prior0 = c(
  prior(normal(0,5), class = "b", nlpar = "eta"),
  prior(normal(0,1), class = "b", nlpar = "logalpha"),
  prior(constant(1), class="sd", group="speaker", nlpar = "eta"),
  prior(normal(0, 1), dpar = "phi", class = "b")
)


#chatgpt advice for priors
prior1 = c(
  # Coefficients for eta (usually log-linear predictor for mean)
  prior(normal(0, 2), class = "b", nlpar = "eta"),
  # Coefficients for logalpha (used in exponent → strong effect on steepness)
  prior(normal(0, 1), class = "b", nlpar = "logalpha"),
  # Coefficients for asymptote (used only in models 3 and 7–10)
  prior(normal(0, 1), class = "b", nlpar = "asymptote"),
  # Prior on group-level SD for random intercept (speaker-level variability)
  prior(exponential(1), class = "sd", group = "speaker", nlpar = "eta"),
  # Coefficients for phi (precision, dpar)
  prior(normal(0, 1), class = "b", dpar = "phi")
)

prior0_Version2 = c(
  prior(normal(0, 5), class = "b"),          # priors for mean model
  prior(normal(0, 1), class = "b",dpar="phi")         # priors for phi model
)





prior_list <- list(
  "prior0_Version2"   = list(object =prior0_Version2, valid_models = c("model0_Version2") ) ,
  "prior_binomial"    = list(object = prior_binomial, valid_models = c("model_binomial_dummytest",
                                                                       "model_binomial_AAPS", 
                                                                       "model_binomial_Probability", 
                                                                       "model_binomial_Probability_singleWords",
                                                                       "model_binomialv2"
                                                                       
                                                                       )
                             )
)

export("return_lists")
return_lists = function(){
  return(list(model_list = model_list, prior_list = prior_list))
}