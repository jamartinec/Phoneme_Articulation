import("tidyr")
import("purrr")
import("brms")
import("splines")
import("stats")

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

model0_Version2 <- bf(
  mean_prob ~ 1 + ns(age_months, 3),
  phi ~ 1  + age_months,  # Precision parameter
  family = Beta(link = "logit")
)

model_binomial <- bf(
  sum_score | trials(num_score) ~ 1 + ns(age_months, 4),
  family = binomial(link = "logit")
)


model_list <- list(
  #model3 = list(name = "model3", object = model3),
  #model5 = list(name = "model5", object = model5)
  #model9 = list(name = "model9", object = model9),
   #model0_Version2 = list(name="model0_Version2", object = model0_Version2)
  #model_binomial = list(name="model_binomial", object = model_binomial)
  #model_binomial_AAPS = list(name="model_binomial_AAPS", object = model_binomial)#,
  #model_binomial_Probability = list(name="model_binomial_Probability", object = model_binomial)
  model_binomial_Probability_singleWords = list(name="model_binomial_Probability_singleWords", object = model_binomial)
)
#----------------------------------------------------------------------------
data1<-list(category = "Vowels",levels = c("Level1", "Level2"))
data2<-list(category = "Vowels",levels = c("Level4", "Level5"))
data3<-list(category = "Vowels",levels = c("Level3"))
data4<-list(category = "Consonants",levels = c("Level3"))
data5<-list(category = "Consonants",levels = c("Level4"))
data6<-list(category = "Consonants",levels = c("Level5"))
data7<-list(category = "Consonants",levels = c("Level6"))
#------------------------------------------------------------------------------
# Modeling each phoneme independently
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
#------------------------------------------------------------------------------
data_list_viz <- list(
  #data1, 
  #data2,
  #data3, 
  #data4, 
  #data5, 
  #data6, 
  #data7
  dataPhoneme1,dataPhoneme2,dataPhoneme3,dataPhoneme4,dataPhoneme5,
   dataPhoneme6,dataPhoneme7
  ,dataPhoneme8,dataPhoneme9,
   dataPhoneme10,dataPhoneme11,dataPhoneme12,dataPhoneme13,dataPhoneme14,
   dataPhoneme15,dataPhoneme16,dataPhoneme17,dataPhoneme18,dataPhoneme19,
   dataPhoneme20,dataPhoneme21,dataPhoneme22,dataPhoneme23,dataPhoneme24,
   dataPhoneme25,dataPhoneme26,dataPhoneme27,dataPhoneme28,dataPhoneme29,
   dataPhoneme30,dataPhoneme31,dataPhoneme32,dataPhoneme33,dataPhoneme34,
   dataPhoneme35,dataPhoneme36,dataPhoneme37,dataPhoneme38,dataPhoneme39
)

list_to_viz <- list()
counter <- 1

for (model_entry in model_list) {
  for (d_idx in seq_along(data_list_viz)) {
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