import("tidyr")
import("purrr")
import("brms")
import("splines")

model3 <- bf( # apply inverse logit function to asymptote, for constraint the values
  mean_prob ~  inv_logit(asymptote)/ (1 + exp(-exp(logalpha) * eta)),
  asymptote ~ (1 + expected_phoneme),
  eta ~ 1 + expected_phoneme +  age_months + (1 | speaker),
  logalpha  ~ 1 + expected_phoneme, 
  phi ~ 1 + expected_phoneme + age_months,  # Precision parameter
  family = Beta(link = "identity"),
  nl = TRUE)  # Non-linear model

model5 <- bf(
  mean_prob ~ exp(logalpha) * eta, 
  eta ~ 1 + expected_phoneme +  ns(age_months,2) + (1 | speaker),
  logalpha  ~ 1 + expected_phoneme, 
  phi ~ 1 + expected_phoneme + age_months,  # Precision parameter
  family = Beta(link = "logit"),
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



model_list <- list(
  #model3 = list(name = "model3", object = model3),
  #model5 = list(name = "model5", object = model5)
  model9 = list(name = "model9", object = model9)
)

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
data_list_viz <- list(
  data1, 
  data2,
  data3, 
  data4, 
  data5, 
  data6, 
  data7
)

list_to_viz <- list()
counter <- 1

for (model_entry in model_list) {
  for (d_idx in seq_along(data_list_viz)) {
    #for (prior_entry in prior_list_fit) {
    #if (model_entry$name %in% prior_entry$valid_models) {
    list_to_viz[[counter]] <- list(
      model_opt = model_entry$name,
      category = data_list_viz[[d_idx]]$category,
      levels = data_list_viz[[d_idx]]$levels
    )
    counter <- counter + 1
  }
}

###############################################################################
export("return_dict_exp")
return_dict_exp = function(){
  return(list_to_viz)
}