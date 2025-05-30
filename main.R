library(tidyverse)
library(dplyr)
library(posterior)
library(yaml)

################################################################################

lib_bayesian_code_modeling1 <- modules::use("bayesian_code/modeling/fit_bayesian_model.R")
lib_bayesian_code_visuals1 <- modules::use("bayesian_code/visuals/run_visuals.R")
lib_bayesian_code_model_validation1 <- modules::use("bayesian_code/model_validation/model_validation.R")

###############################################################################3

###############################################################################
# Multiple versions of the models are available. Refer to ./bayesian_code/README.md
# for the naming conventions. The prefix used will depend on the specific model type
folder_path = "./data/processed_data/"
model_opt = "model2"
prefix <- paste(folder_path, model_opt, "/", sep="")
################################################################################
# Example execution
category <- "Consonants"
levels <- c("Level6" )#,"Level5")

#model_specific <- list(
# phi_formula = ~ 1 + expected_phoneme
#)

model_specific <- list(
  phi_formula = ~ 1 + age_months
)



################################################################################
# Model Fitting
################################################################################
lib_bayesian_code_modeling1$run_bayesian_modeling(category, levels, prefix, model_specific)

# Specify the models you want to run:
raw_yaml <- read_yaml("bayesian_code/modeling/models_to_fit.yaml")
list_to_fit <- lapply(raw_yaml, function(entry) {
  
  if (!is.null(entry$model_specific)) {
    # Flatten list of named lists to a single named list
    model_spec <- entry$model_specific
    flattened <- setNames(
      lapply(model_spec, function(x) as.formula(x[[1]])),
      sapply(model_spec, function(x) names(x)[1])
    )
    entry$model_specific <- flattened
  }
  
  entry
})
lib_bayesian_code_modeling1$iterate_run_bayesian_modeling(list_to_fit)
#######################################################################################################
# Visuals
################################################################################


lib_bayesian_code_visuals1$run_visuals(category, levels, prefix)
# Specify models you want to generate visualizations for:

raw_list_visuals <- read_yaml("bayesian_code/visuals/models_to_visualize.yaml")
list_to_visualize <- lapply(raw_list_visuals, function(entry) {
  entry$levels <- as.character(entry$levels)
  entry
})

lib_bayesian_code_visuals1$iterate_run_visuals(list_to_visualize)
##############################################################################
# MODEL VALIDATION
##############################################################################
# Validate a single model:
#dict_validation <- model_validation(category, levels, prefix)
#print(dict_validation)

# Specify models to evaluate:
raw_list <- read_yaml("bayesian_code/model_validation/models_to_validate.yaml")
list_to_validate <- lapply(raw_list, function(entry) {
  entry$levels <- as.character(entry$levels)
  entry
})
results <- lib_bayesian_code_model_validation1$iterate_model_validation(list_to_validate)
#print(results)