import("brms")
import("splines")
import("stats")

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

#-------------------------------------------------------------------------------
# Model0 Version 2 (We want to build a simple model for each phoneme)
model0_Version2 <- bf(
  mean_prob ~ 1 + ns(age_months, 3),
  phi ~ 1  + age_months,  # Precision parameter
  family = Beta(link = "logit")
  #nl = TRUE # Non-linear model
  )  

#-------------------------------------------------------------------------------
model_binomial <- bf(
  sum_score | trials(num_score) ~ 1 + ns(age_months, 4),
  family = binomial(link = "logit")
)

#-------------------------------------------------------------------------------

model_list <- list(
  #model0 = list(name = "model0", object = model0),
  #model1 = list(name = "model1", object = model1),
  #model2 = list(name = "model2", object = model2),
  #model3 = list(name = "model3", object = model3),
  #model4 = list(name = "model4", object = model4),
  #model5 = list(name = "model5", object = model5),
  #model6 = list(name = "model6", object = model6),
  #model7 = list(name = "model7", object = model7),
  #model8 = list(name = "model8", object = model8),
  #model9 = list(name = "model9", object = model9)#,
  #model10 =list(name = "model10", object = model10)
  #model0_Version2 = list(name = "model0_Version2", object = model0_Version2)
  #model_binomial_AAPS = list(name="model_binomial_AAPS", object = model_binomial)#,
  #model_binomial_Probability = list(name="model_binomial_Probability", object = model_binomial )
  
  #model_binomial_Probability_singleWords = list(name="model_binomial_Probability_singleWords", object = model_binomial)
  model_binomial_dummytest = list(name="model_binomial_dummytest", object = model_binomial)
)

#model_list <- list(
  #"model0" = model0#,
  #"model1" = model1,
  #"model2" = model2,
  #"model3" = model3#,
  #"model4" = model4
  #"model5" = model5,#
  #"model6" = model6#
#)
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


#chatgpt advice for priors (don't judge me)
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



all_models <- c("model0",
                "model1",
                "model2",
                "model3",
                "model4",
                "model5",
                "model6",
                "model7",
                "model8",
                "model9",
                "model10"
                )

prior_list_fit <- list(
  #prior0 = list(name = "prior0", object = prior0, valid_models = all_models )#,
  #prior1 = list(name = "prior1", object = prior1, valid_models = all_models ),
  #prior0_Version2 = list(name = "prior0_Version2", object = prior0_Version2, valid_models = c("model0_Version2") )
  prior_binomial = list(name = "prior_binomial", object = prior_binomial, valid_models = c("model_binomial_AAPS", "model_binomial_Probability", "model_binomial_Probability_singleWords") )
)
################################################################################
data1<-list(category = "Vowels",levels = c("Level1", "Level2"))
data2<-list(category = "Vowels",levels = c("Level4", "Level5"))
data3<-list(category = "Vowels",levels = c("Level3"))
data4<-list(category = "Consonants",levels = c("Level3"))
data5<-list(category = "Consonants",levels = c("Level4"))
data6<-list(category = "Consonants",levels = c("Level5"))
data7<-list(category = "Consonants",levels = c("Level6"))
#------------------------------------------------------------------------------

dataPhoneme1<-list(category = "Vowels",levels = c("Levelphoneme1"))
dataPhoneme2<-list(category = "Vowels",levels = c("Levelphoneme2"))
dataPhoneme3<-list(category = "Vowels",levels = c("Levelphoneme3"))
dataPhoneme4<-list(category = "Vowels",levels = c("Levelphoneme4"))
dataPhoneme5<-list(category = "Vowels",levels = c("Levelphoneme5"))
dataPhoneme6<-list(category = "Vowels",levels = c("Levelphoneme6"))
dataPhoneme7<-list(category = "Vowels",levels = c("Levelphoneme7"))
dataPhoneme8<-list(category = "Vowels",levels = c("Levelphoneme8"))
dataPhoneme9<-list(category = "Vowels",levels = c("Levelphoneme9"))
dataPhoneme10<-list(category = "Vowels",levels = c("Levelphoneme10"))
dataPhoneme11<-list(category = "Vowels",levels = c("Levelphoneme11"))
dataPhoneme12<-list(category = "Vowels",levels = c("Levelphoneme12"))
dataPhoneme13<-list(category = "Vowels",levels = c("Levelphoneme13"))
dataPhoneme14<-list(category = "Vowels",levels = c("Levelphoneme14"))
dataPhoneme15<-list(category = "Vowels",levels = c("Levelphoneme15"))
dataPhoneme16<-list(category = "Consonants",levels = c("Levelphoneme16"))
dataPhoneme17<-list(category = "Consonants",levels = c("Levelphoneme17"))
dataPhoneme18<-list(category = "Consonants",levels = c("Levelphoneme18"))
dataPhoneme19<-list(category = "Consonants",levels = c("Levelphoneme19"))
dataPhoneme20<-list(category = "Consonants",levels = c("Levelphoneme20"))
dataPhoneme21<-list(category = "Consonants",levels = c("Levelphoneme21"))
dataPhoneme22<-list(category = "Consonants",levels = c("Levelphoneme22"))
dataPhoneme23<-list(category = "Consonants",levels = c("Levelphoneme23"))
dataPhoneme24<-list(category = "Consonants",levels = c("Levelphoneme24"))
dataPhoneme25<-list(category = "Consonants",levels = c("Levelphoneme25"))
dataPhoneme26<-list(category = "Consonants",levels = c("Levelphoneme26"))
dataPhoneme27<-list(category = "Consonants",levels = c("Levelphoneme27"))
dataPhoneme28<-list(category = "Consonants",levels = c("Levelphoneme28"))
dataPhoneme29<-list(category = "Consonants",levels = c("Levelphoneme29"))
dataPhoneme30<-list(category = "Consonants",levels = c("Levelphoneme30"))
dataPhoneme31<-list(category = "Consonants",levels = c("Levelphoneme31"))
dataPhoneme32<-list(category = "Consonants",levels = c("Levelphoneme32"))
dataPhoneme33<-list(category = "Consonants",levels = c("Levelphoneme33"))
dataPhoneme34<-list(category = "Consonants",levels = c("Levelphoneme34"))
dataPhoneme35<-list(category = "Consonants",levels = c("Levelphoneme35"))
dataPhoneme36<-list(category = "Consonants",levels = c("Levelphoneme36"))
dataPhoneme37<-list(category = "Consonants",levels = c("Levelphoneme37"))
dataPhoneme38<-list(category = "Consonants",levels = c("Levelphoneme38"))
dataPhoneme39<-list(category = "Consonants",levels = c("Levelphoneme39"))
########################################################################
data_list_fit <- list(
  #data1, #data2, #data3, #data4, #data5,#data6, #data7
  dataPhoneme1,dataPhoneme2,dataPhoneme3,dataPhoneme4,dataPhoneme5,
  dataPhoneme6,dataPhoneme7,dataPhoneme8,dataPhoneme9,
  dataPhoneme10,dataPhoneme11,dataPhoneme12,dataPhoneme13,dataPhoneme14,
  dataPhoneme15,dataPhoneme16,dataPhoneme17,dataPhoneme18,dataPhoneme19,
  dataPhoneme20,dataPhoneme21,dataPhoneme22,dataPhoneme23,dataPhoneme24,
  dataPhoneme25,dataPhoneme26,dataPhoneme27,dataPhoneme28,dataPhoneme29,
  dataPhoneme30,dataPhoneme31,dataPhoneme32,dataPhoneme33,dataPhoneme34,
  dataPhoneme35,dataPhoneme36,dataPhoneme37,dataPhoneme38,dataPhoneme39
)

####################################################################
# Create the Cartesian product (including also the priors)
# combined_model_data_list <- expand.grid(
#   model_opt = names(model_list),
#   data_index = seq_along(data_list_fit),
#   #prior_index = seq_along(prior_list_fit)
#   stringsAsFactors = FALSE
# )
# 
# # Build the combined list
# list_to_fit <- apply(combined_model_data_list, 1, function(row) {
#   model_name <- row[["model_opt"]]
#   data_index <- as.integer(row[["data_index"]])
#   data_entry <- data_list_fit[[data_index]]
#   #prior_entry
#   
#   list(
#     model_opt = model_name,
#     category = data_entry$category,
#     levels = data_entry$levels,
#     model_specific = model_list[[model_name]]#,
#     #prior_specific = prior_list[[prior_entry]]
#   
#   )
# })
###############################################################################
list_to_fit <- list()
counter <- 1

for (model_entry in model_list) {
  for (d_idx in seq_along(data_list_fit)) {
    for (prior_entry in prior_list_fit) {
      if (model_entry$name %in% prior_entry$valid_models) {
        list_to_fit[[counter]] <- list(
          model_opt = model_entry$name,
          category = data_list_fit[[d_idx]]$category,
          levels = data_list_fit[[d_idx]]$levels,
          model_specific = model_entry$object,
          prior_name = prior_entry$name,
          prior_specific = prior_entry$object
        )
        counter <- counter + 1
      }
    }
  }
}


###############################################################################
export("return_dict_exp")
return_dict_exp = function(){
  return(list_to_fit)
}