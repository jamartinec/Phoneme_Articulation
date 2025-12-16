library(tidyverse)
library(dplyr)
library(posterior)
library(yaml)

################################################################################

lib_bayesian_code_modeling1 <- modules::use("bayesian_code/modeling/fit_bayesian_model.R")
lib_bayesian_code_modeling3 <- modules::use("bayesian_code/modeling/models_to_fit.R")
lib_bayesian_code_visuals1 <- modules::use("bayesian_code/visuals/run_visuals.R")
lib_bayesian_code_visuals2 <- modules::use("bayesian_code/visuals/models_to_viz.R")
lib_bayesian_code_model_validation1 <- modules::use("bayesian_code/model_validation/model_validation.R")
lib_bayesian_code_model_validation2 <- modules::use("bayesian_code/model_validation/models_to_validate.R")
###############################################################################
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
# Specify the models you want to run:
#raw_yaml <- read_yaml("bayesian_code/modeling/models_to_fit.yaml")
# list_to_fit <- lapply(raw_yaml, function(entry) {
#   
#   if (!is.null(entry$model_specific)) {
#     # Flatten list of named lists to a single named list
#     model_spec <- entry$model_specific
#     flattened <- setNames(
#       lapply(model_spec, function(x) as.formula(x[[1]])),
#       sapply(model_spec, function(x) names(x)[1])
#     )
#     entry$model_specific <- flattened
#   }
#   
#   entry
# })
#source("./bayesian_code/modeling/models_to_fit.R")
list_to_fit<-lib_bayesian_code_modeling3$return_dict_exp()
print(list_to_fit)
lib_bayesian_code_modeling1$iterate_run_bayesian_modeling(list_to_fit)
#######################################################################################################
# Visuals
################################################################################
#visualize a single model:
#lib_bayesian_code_visuals1$run_visuals(category, levels, prefix)
# Specify models you want to generate visualizations for:
##########################################################################
## in case you prefer to specify the models to visualize using the yaml file
###########################################################################
# raw_list_visuals <- read_yaml("bayesian_code/visuals/models_to_visualize.yaml")
# list_to_visualize <- lapply(raw_list_visuals, function(entry) {
#   entry$levels <- as.character(entry$levels)
#   entry
# })

## in case you prefer to specify the models to visualize using the R file
list_to_visualize <- lib_bayesian_code_visuals2$return_dict_exp()
print(list_to_visualize)
lib_bayesian_code_visuals1$iterate_run_visuals(list_to_visualize)
##############################################################################
# MODEL VALIDATION
##############################################################################
# Validate a single model:
#dict_validation <- model_validation(category, levels, prefix)
#print(dict_validation)

# Specify models to evaluate:
###########################################################################
## in case you prefer to specify the models to validate using the yaml file
###########################################################################
# raw_list <- read_yaml("bayesian_code/model_validation/models_to_validate.yaml")
# list_to_validate <- lapply(raw_list, function(entry) {
#   entry$levels <- as.character(entry$levels)
#   entry
# })
###########################################################################
## in case you prefer to specify the models to validate using the R file
list_to_validate <- lib_bayesian_code_model_validation2$return_dict_exp()
print(list_to_validate)
results_filename <- "model_validation_results_july16"
###########################################################################
results <- lib_bayesian_code_model_validation1$iterate_model_validation(list_to_validate,results_filename)
