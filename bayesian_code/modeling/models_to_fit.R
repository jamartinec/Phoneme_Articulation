import("brms")
import("splines")

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

model3 <- bf( # apply inverse logit function to asymptote, for constraing the values
  mean_prob ~  inv_logit(asymptote)/ (1+  exp(-exp(logalpha) * eta)),
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

model10 <- bf( # apply inverse logit function to asymptote, for constraing the values
  mean_prob ~  inv_logit(asymptote)/ (1+  exp(-exp(logalpha) * eta)),
  asymptote ~ (1 + expected_phoneme),
  eta ~ 1 + expected_phoneme +  ns(age_months,4) + (1 | speaker),
  logalpha  ~ 1 + expected_phoneme, 
  phi ~ 1 + expected_phoneme + age_months,  # Precision parameter
  family = Beta(link = "identity"),
  nl = TRUE)  # Non-linear model

model_list <- list(
  #"model0" = model0#,
  #"model1" = model1,
  #"model2" = model2,
  "model3" = model3#,
  #"model4" = model4
  #"model5" = model5,#
  #"model6" = model6#
)
########################################################################
data1<-list(
  category = "Vowels",
  levels = c("Level1", "Level2")
  )

data2<-list(
  category = "Vowels",
  levels = c("Level4", "Level5")
)

data3<-list(
  category = "Vowels",
  levels = c("Level3")
)

data4<-list(
  category = "Consonants",
  levels = c("Level3")
)

data5<-list(
  category = "Consonants",
  levels = c("Level4")
)

data6<-list(
  category = "Consonants",
  levels = c("Level5")
)

data7<-list(
  category = "Consonants",
  levels = c("Level6")
)
########################################################################
data_list_fit <- list(
  data1, 
  data2, data3, 
  data4, 
  data5, data6, data7
)

####################################################################
# Create the Cartesian product
combined_model_data_list <- expand.grid(
  model_opt = names(model_list),
  data_index = seq_along(data_list_fit),
  stringsAsFactors = FALSE
)

# Build the combined list
list_to_fit <- apply(combined_model_data_list, 1, function(row) {
  model_name <- row[["model_opt"]]
  data_index <- as.integer(row[["data_index"]])
  data_entry <- data_list_fit[[data_index]]
  
  list(
    model_opt = model_name,
    category = data_entry$category,
    levels = data_entry$levels,
    model_specific = model_list[[model_name]]
  )
})

export("return_dict_exp")
return_dict_exp = function(){
  return(list_to_fit)
}